;;; ron.el --- Rust Object Notation parser -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Luke Holland
;;
;; Author: Luke Holland
;; Maintainer: Luke Holland
;; Created: February 20, 2026
;; Modified: February 20, 2026
;; Version: 0.0.1
;; Keywords: convenience data languages text tools
;; Homepage: https://github.com/yelobat/ron
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Most of this code is inspired by json.el. The goal is to avoid
;; turning JSON into RON, and to be able to convert RON directly
;; into Elisp objects which can then be manipulated like normal
;; Elisp objects, and then to be able to convert these objects
;; back into RON.
;;
;; The grammar is adapted from:
;; https://github.com/ron-rs/ron/blob/master/docs/grammar.md
;;
;;  Description
;;
;;; Code:

(require 'map)
(require 'cl-lib)
(require 'subr-x)

;;;; Parameters

(cl-defstruct ron-false
  "Value to use when reading RON `false'.")

(cl-defstruct ron-true
  "Value to use when reading RON `true'.")

(defvar ron-encoding-default-indentation "  "
  "String used for a single indentation level during encoding.
This value is repeated for each further nested element.
Used only when `ron-encoding-pretty-print' is non-nil.")

(defvar ron--print-indentation-prefix "\n"
  "String used to start indentation during encoding.
Used only when `ron-encoding-pretty-print' is non-nil.")

(defvar ron--print-indentation-depth 0
  "Current indentation level during encoding.
Dictates repetitions of `ron-encoding-default-indentation'.
Used only when `ron-encoding-pretty-print' is non-nil.")

(defvar ron-encoding-lisp-style-closings nil
  "If non-nil, delimiters ], ), and } will be formatted Lisp-style.
This means they will be placed on the same line as the last
element of the respective array or object, without indentation.
Used only when `ron-encoding-pretty-print' is non-nil.")

(defvar ron--print-keyval-separator ":"
  "String used to separate key-value pairs during encoding.")

(defvar ron-encoding-pretty-print nil
  "If non-nil, then the output of `ron-encode' will be pretty-printed.")

;; Error conditions

(define-error 'ron-error "Unknown RON error")
(define-error 'ron-end-of-file "End of file when parsing RON"
              '(end-of-file ron-error))
(define-error 'ron-readtable-error "RON readtable error" 'ron-error)
(define-error 'ron-comma-error "Missing comma" 'ron-error)
(define-error 'ron-list-format "Invalid List Format" 'ron-error)
(define-error 'ron-tuple-format "Invalid Tuple Format" 'ron-error)
(define-error 'ron-enum-format "Invalid Enum Format" 'ron-error)
(define-error 'ron-string-format "Invalid String Format" 'ron-error)
(define-error 'ron-identifier-format "Invalid Identifier Format" 'ron-error)
(define-error 'ron-optional-format "Invalid Optional Format" 'ron-error)
(define-error 'ron-number-format "Invalid Number format" 'ron-error)

;;;; Keywords

(rx-define ron--whitespace-single
  (in ?\n ?\t ?\r ?\s ?\u000B ?\u000C
      ?\u0085 ?\u200E ?\u200F ?\u2028 ?\u2029 ?/))

(rx-define ron--post-value (| ron--whitespace-single (in ",}]():") eol))

;; TODO Should be the same as XID_Start
(rx-define ron--identifier-start
  (in "a-z" "A-Z" "_"))

;; TODO Should be the same as XID_Continue
(rx-define ron--identifier-rest
  (in "a-z" "A-Z" "0-9" "_"))

(rx-define ron--identifier
  (: ron--identifier-start (* ron--identifier-rest)))

(defun ron--reserved (string)
  "Return a rx definition for STRING as a reserved name."
  (macroexpand `(rx (: ,string) ron--post-value)))

(defconst ron--reserved-nan
  (ron--reserved "NaN")
  "The floating point NaN reserved constant in RON.")

(defconst ron--reserved-inf
  (ron--reserved "inf")
  "The floating point inf reserved constant in RON.")

(defconst ron--reserved-true
  (ron--reserved "true")
  "The boolean true reserved constant in RON.")

(defconst ron--reserved-false
  (ron--reserved "false")
  "The boolean false reserved constant in RON.")

(defconst ron--reserved-none
  (ron--reserved "None")
  "The boolean None reserved constant in RON.")

(defconst ron--reserved-some
  (macroexpand `(rx (: "Some" (| ron--whitespace-single ?\())))
  "The optional Some reserved tuple variant in RON.")

;;;; Utilities

;; Predicates

(defun ron-alist-p (list)
  "Non-nil if and only if LIST is an alist with simple keys."
  (declare (pure t) (side-effect-free error-free))
  (if (= (safe-length list) 0)
      nil
    (while (and (consp (car-safe list))
                (atom (caar list))
                (setq list (cdr list))))
    (null list)))

(defun ron-plist-p (list)
  "Non-nil if and only if LIST is a plist with keyword keys."
  (declare (pure t) (side-effect-free error-free))
  (if (= (safe-length list) 0)
      nil
    (while (and (keywordp (car-safe list))
                (consp (cdr list))
                (setq list (cddr list))))
    (null list)))

;; Reader utilities

(define-inline ron-advance (&optional n)
  "Advances N characters forward, or 1 character if N is nil.
On reaching the end of the accessible region of the buffer, stop
and signal an error."
  (inline-quote (forward-char ,n)))

(define-inline ron-peek ()
  "Return the character at point.
At the end of the accessible region of the buffer, return 0."
  (inline-quote (following-char)))

;;;; Reader

;; TODO Optimize this
(defmacro ron-readtable-dispatch (char)
  "Dispatch reader function for CHAR at point.
If CHAR is nil, signal `ron-end-of-file'."
  (declare (debug t))
  (macroexp-let2 nil char char
    `(cond
      ;;;; Handle reserved keywords

      ;; Handle Floating point keywords
      ((looking-at-p ron--reserved-nan) (ron-read-number))
      ((looking-at-p ron--reserved-inf) (ron-read-number))

      ;; Handle Some tuple variant
      ((looking-at-p ron--reserved-some) (ron-read-optional))
      ((looking-at-p ron--reserved-none) (ron-read-optional))

      ;; Handle true keyword
      ((looking-at-p ron--reserved-true) (ron-read-true))
      ((looking-at-p ron--reserved-false) (ron-read-false))

      ;;;; Handle Strings
      ((looking-at-p "r#") (ron-read-string))
      ((eq ,char ?\") (ron-read-string))

      ;;;; Handle Lists
      ((eq ,char ?\[) (ron-read-list))

      ;;;; Handle Maps
      ((eq ,char ?\{) (ron-read-map))

      ;;;; Handle Structs
      ;; TODO This is not a complete implementation
      ;; of identifiers
      ,@(map-apply
         (lambda (key expr)
           `((eq ,char ,key) ,expr))
         `(,@(mapcar (lambda (c) (list c #'ron-read-struct))
                     '(?_ ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n
                       ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z ?A ?B
                       ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P
                       ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z ?\())))

      ;;;; Handle numbers
      ,@(map-apply
         (lambda (key expr)
           `((eq ,char ,key) ,expr))
         `(,@(mapcar (lambda (c) (list c #'ron-read-number))
                     '(?- ?+ ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?.))))

      ;;;; Signal unrecognized characters
      (,char (signal 'ron-readtable-error (list ,char)))

      ;;;; Signal end of file
      (t     (signal 'ron-end-of-file ())))))

(defun ron-read ()
  "Parse and return the RON object following point.
Advances point just passed the RON object."
  (ron-skip-whitespace)
  (let ((case-fold-search nil))
    (ron-readtable-dispatch (char-after))))

;; Syntactic sugar for the reader

(defun ron-read-from-string (string)
  "Read the RON object contained in STRING and return it."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (ron-read)))

(defun ron-read-file (file)
  "Read the first RON object contained in FILE and return it."
  (with-temp-buffer
    (insert-file-contents file)
    (ron-read)))

;; Writing utilities

(defun ron-write-file (object file &optional minimize)
  "Write the RON object stored in OBJECT to FILE.
With prefix argument MINIMIZE, minimize it instead."
  (with-temp-file file
      (insert (ron-encode object))
    (ron-pretty-print-buffer minimize)))

;; Encoder utilities

(defmacro ron--with-output-to-string (&rest body)
  "Eval BODY in a temporary buffer bound to `standard-output'.
Return the resulting buffer contents as a string."
  (declare (indent 0) (debug t))
  `(with-output-to-string
     (with-current-buffer standard-output
       (setq-local inhibit-modification-hooks t)
       ,@body)))

(defmacro ron--with-indentation (&rest body)
  "Eval BODY with the RON encoding nesting incremented by one step.
This macro sets up appropriate variable bindings for
`ron--print-indentation' to produce the correct indentation when
`ron-encoding-pretty-print' is non-nil."
  (declare (debug t) (indent 0))
  `(let ((ron--print-indentation-prefix
          (if ron-encoding-pretty-print ron--print-indentation-prefix ""))
         (ron--print-keyval-separator (if ron-encoding-pretty-print ": " ":"))
         (ron--print-indentation-depth (1+ ron--print-indentation-depth)))
     ,@body))

(defun ron--print-indentation ()
  "Insert the current indentation for RON encoding at point.
Has no effect if `ron-encoding-pretty-print' is nil."
  (when ron-encoding-pretty-print
    (insert ron--print-indentation-prefix)
    (dotimes (_ ron--print-indentation-depth)
      (insert ron-encoding-default-indentation))))

;;;; Encoder

(defun ron--print (object)
  "Like `ron-encode', but insert or print the RON OBJECT at point."
  (cond
   ((null object) (insert "()"))
   ((ron-true-p object) (insert "true"))
   ((ron-false-p object) (insert "false"))
   ((ron-some-p object) (ron--print-some object))
   ((ron-none-p object) (ron--print-none object))
   ((ron-plist-p object) (ron--print-struct object))
   ((ron-alist-p object) (ron--print-alist object))
   ((stringp object) (ron--print-string object))
   ((listp object) (ron--print-array object))
   ((symbolp object) (insert (symbol-name object)))
   ((numberp object) (prin1 object))
   ((vectorp object) (ron--print-vector object))
   ((hash-table-p object) (ron--print-unordered-map object))
   (t (signal 'ron-end-of-file ()))))

(defun ron--print-vector (vector)
  "Insert a RON representation of VECTOR at point."
  (insert ?\[)
  (unless (length= vector 0)
    (ron--with-indentation
      (ron--print-indentation)
    (let ((first t))
      (mapc (lambda (elt)
              (if first
                  (setq first nil)
                (insert ?,))
              (ron--print elt))
            vector)))
    (or ron-encoding-lisp-style-closings
        (ron--print-indentation)))
  (insert ?\])
  vector)

(defun ron--print-alist (alist)
  "Insert a RON representation of ALIST at point."
  (insert ?\()
  (unless (length= alist 0)
    (ron--with-indentation
      (ron--print-indentation)
      (let ((first t))
        (mapc (lambda (elt)
                (if first
                    (setq first nil)
                  (insert ?,)
                  (ron--print-indentation))
                (let ((key (car elt))
                      (value (cdr elt)))
                  (ron--print key)
                  (insert ?:)
                  (ron--print value)))
              alist)))
    (or ron-encoding-lisp-style-closings
        (ron--print-indentation)))
  (insert ?\))
  alist)

(defun ron--print-struct (struct)
  "Insert a RON representation of STRUCT at point."
  (let ((identifier (substring (prin1-to-string (car struct)) 1))
        (list (cadr struct)))
    (insert identifier)
    (ron--print list)))

(defun ron--print-array (array)
  "Insert a RON representation of ARRAY at point."
  (insert ?\()
  (ron--with-indentation
    (ron--print-indentation)
    (let ((first t))
      (mapc (lambda (elt)
              (if first
                  (setq first nil)
                (insert ?,)
                (ron--print-indentation))
              (ron--print elt))
            array)))
  (or ron-encoding-lisp-style-closings
      (ron--print-indentation))
  (insert ?\))
  array)

(defun ron--print-pair (key val)
  "Insert a RON representation of KEY VAL pair at point."
  (ron--print-indentation)
  (ron--print key)
  (insert ron--print-keyval-separator)
  (ron--print val)
  (insert ?,))

(defun ron--print-unordered-map (map)
  "Insert a RON representation of MAP at point."
  (insert ?{)
  (unless (map-empty-p map)
    (ron--with-indentation
      (map-do #'ron--print-pair map)
      (delete-char (- 1)))
  (or ron-encoding-lisp-style-closings
      (ron--print-indentation)))
  (insert ?}))

(defun ron--print-string (string)
  "Insert a RON representation of STRING at point."
  (insert ?\")
  (princ string)
  (insert ?\")
  string)

(defun ron--print-some (some)
  "Insert a RON representation of SOME at point."
  (insert "Some(")
  (ron--print (ron-some-value some))
  (insert ?\))
  some)

(defun ron--print-none (none)
  "Insert a RON representation of NONE at point."
  (insert "None")
  none)

(defun ron-encode (object)
  "Return a RON representation of OBJECT as a string.

OBJECT should have a structure like one returned by `ron-read'.
If an error is detected during encoding, an error based on
`ron-error' is signaled."

  (ron--with-output-to-string (ron--print object)))

;;;; Parsing

;; Whitespace + Comments
;; See
;; https://github.com/ron-rs/ron/blob/master/docs/grammar.md
;; for the definitions on whitespace in RON.

(define-inline ron-skip-whitespace ()
  "Skip past the whitespace at point."
  (inline-quote
   (while (looking-at-p (rx ron--whitespace-single))
     (cond
      ;; Skip line if it is a single line comment.
      ((looking-at-p (rx "//"))
       (forward-line))

      ;; Skip nested block, handling recursive nested
      ;; blocks in the process.
      ((looking-at-p (rx "/*"))
       (let ((depth 1))
         (ron-advance 2)
         (while (> depth 0)
           (cond
            ((eobp) (signal 'ron-end-of-file ()))
            ((looking-at-p (rx "/*"))
             (setq depth (1+ depth))
             (ron-advance 2))
            ((looking-at-p (rx "*/"))
             (setq depth (1- depth))
             (ron-advance 2))
            (t (ron-advance))))))

      ;; Handle normal whitespace
      (t (or (looking-at (rx ron--whitespace-single))
             (signal 'ron-end-of-file ()))
         (goto-char (match-end 0)))))))

;; Numbers
;; See
;; https://github.com/ron-rs/ron/blob/master/docs/grammar.md
;; for the definitions on numbers in RON.

(rx-define ron--integer-suffix
  (: (| ?i ?u)
     (| "8" "16" "32"
        "64" "128")))

(rx-define ron--unsigned-binary
  (: "0b"
     (: (| ?0 ?1)
        (* (| ?0 ?1 ?_)))))

(rx-define ron--unsigned-octal
  (: "0o"
     (| (in "0-7"))
     (* (| (in "0-7" "_")))))

(rx-define ron--unsigned-hexadecimal
  (: "0x"
     (| xdigit)
     (* (| xdigit ?_))))

(rx-define ron--unsigned-decimal
  (: digit
     (* (| digit ?_))))

(rx-define ron--unsigned
  (| ron--unsigned-binary
     ron--unsigned-octal
     ron--unsigned-hexadecimal
     ron--unsigned-decimal))

(rx-define ron--integer
  (: (? (| ?- ?+))
     ron--unsigned
     (? ron--integer-suffix)))

(rx-define ron--float-suffix
  (: ?f (| "32" "64")))

(rx-define ron--float
  (: (? (| ?- ?+))
     (| "inf" "NaN"
        (| (: ?. digit (* (| digit ?_)))
           (: digit (* (| digit ?_)) (? ?. (* digit)))))
     (? (in "Ee") (? (| ?- ?+)) (* (| digit ?_)))
     (? ron--float-suffix)))

;; TODO This is a little untidy, need to fix
;; this at some point due to fighting over
;; parsing float and integers near the end
(defun ron-read-number ()
  "Read a RON number at point."
  (let ((sign "+")
        (base 10)
        number)

    ;; Skip the optional sign, changing the sign
    ;; if it is negative.
    (when (looking-at (rx (in "+-")))
      (when (= (ron-peek) ?-)
        (setq sign "-"))
      (ron-advance))

    ;; Check the various cases
    (cond
     ((looking-at-p "0x")
      (looking-at (rx ron--integer))
      (setq number (substring (match-string 0) 2))
      (setq base 16))
     ((looking-at-p "0b")
      (looking-at (rx ron--integer))
      (setq number (substring (match-string 0) 2))
      (setq base 2))
     ((looking-at-p "0o")
      (looking-at (rx ron--integer))
      (setq number (substring (match-string 0) 2))
      (setq base 8))
     (t (or (looking-at (rx (| ron--float ron--integer)))
            (signal 'ron-number-format (list (point))))
        (setq number (match-string 0))))

    (setq number (substring (string-replace "_" "" number)))
    (goto-char (match-end 0))
    (when (looking-at (rx ron--integer-suffix))
      (goto-char (match-end 0)))

    (or (looking-at-p (rx ron--post-value))
        (signal 'ron-number-format (list (point))))
    (string-to-number (concat sign number) base)))

;; TODO Bytes
;; See
;; https://github.com/ron-rs/ron/blob/master/docs/grammar.md
;; for the definitions on bytes in RON.

(rx-define ron--byte
  (: ?b ?'
     (| ascii
        (: "\\"
           (| (: ?x xdigit xdigit)
              (in "ntr\\0\"'"))))
     ?'))

(defun ron-read-byte ()
  "Read a RON byte at point."
  (or (looking-at (rx ron--byte))
      (signal 'ron-byte-format (list (point))))
  (goto-char (match-end 0))
  (read (concat "?" (substring (match-string 0) 2 -1))))

;; Strings
;; See
;; https://github.com/ron-rs/ron/blob/master/docs/grammar.md
;; for the definitions on strings in RON.

(defun ron-read-string-standard ()
  "Read a standard RON string at point."

  ;; Skip the '"'
  (ron-advance)

  (let (characters
        (char (ron-peek)))

    (while (/= char ?\")
      (push char characters)
      (ron-advance)
      (setq char (ron-peek)))

  ;; Skip the '"'
  (ron-advance)
  (concat (nreverse characters))))

(defun ron-read-string-raw ()
  "Read a raw RON string at point."

  ;; Skip the 'r'
  (ron-advance)

  (let ((hcount 0)
        characters
        char
        done)

    ;; Count how many hash tags delimit this raw string.
    (while (= (ron-peek) ?#)
      (ron-advance)
      (setq hcount (1+ hcount)))

    (when (/= (ron-peek) ?\")
      (signal 'ron-string-format (list "\"" (ron-peek))))

    ;; Skip the '"'
    (ron-advance)
    (push ?\" characters)

    (setq char (ron-peek))
    (while (not done)
      (while (/= char ?\")
        (push char characters)
        (ron-advance)
        (setq char (ron-peek)))

      (let ((count 0))

        ;; Add the '"' for safe keeping
        (push ?\" characters)
        (ron-advance)

        (setq char (ron-peek))

        (while (and (not done) (= char ?#))
          (setq count (1+ count))
          (if (= count hcount)
              (setq done t)
            (push char characters)
            (ron-advance)
            (setq char (ron-peek))))

        (when (= count hcount)
            (setq done t))))
    (concat (nreverse characters))))

(defun ron-read-string ()
  "Read a RON string at point."
  (cond
   ((= (ron-peek) ?\") (ron-read-string-standard))
   ((= (ron-peek) ?r) (ron-read-string-raw))
   (t (signal 'ron-string-format (list (point))))))

;; Optional
;; See
;; https://github.com/ron-rs/ron/blob/master/docs/grammar.md
;; for the definitions on Optional in RON.

(cl-defstruct ron-some value)
(cl-defstruct ron-none)

(defun ron-new-some (value)
  "Create a list which represents the Some tuple variant with VALUE."
  (make-ron-some :value value))

(defun ron-new-none ()
  "Create a list which represents the Some tuple variant, as None."
  (make-ron-none))

(defun ron-read-optional ()
  "Read a RON optional object at point."
  (cond
   ;; Some case
   ((= (ron-peek) ?S)
    (ron-advance 4)
    (ron-skip-whitespace)
    (when (/= (ron-peek) ?\()
      (signal 'ron-optional-format (list (point))))
    (ron-advance)
    (ron-skip-whitespace)
    (prog1 (ron-new-some (ron-read))
      (ron-skip-whitespace)
      (when (/= (ron-peek) ?\))
        (signal 'ron-optional-format (list (point))))
      (ron-advance)
      (or (looking-at (rx ron--post-value))
          (signal 'ron-optional-format (list (point))))))

   ;; None case
   ((= (ron-peek) ?N)
    (ron-advance 4)
    (prog1 (ron-new-none)
      (or (looking-at (rx ron--post-value))
          (signal 'ron-optional-format (list (point))))))

   ;; Invalid optional case
   (t (signal 'ron-optional-format (list (point))))))

;; List
;; See
;; https://github.com/ron-rs/ron/blob/master/docs/grammar.md
;; for the definitions on List in RON.

(defun ron-read-list ()
  "Read a RON list object at point."
  ;; Skip '['
  (ron-advance)

  (let ((elements nil))
    (while (/= (ron-peek) ?\])

      ;; Push the value to elements
      (push (ron-read) elements)

      (ron-skip-whitespace)
      (when (/= (ron-peek) ?\])
        (if (= (ron-peek) ?,)
            (progn (ron-advance)
                   (ron-skip-whitespace))
          (signal 'ron-list-format (list "," (ron-peek))))))

    ;; Skip ']'
    (ron-advance)
    (or (looking-at (rx ron--post-value))
        (signal 'ron-list-format (list (point))))
    (nreverse (vconcat elements))))

;; Map
;; See
;; https://github.com/ron-rs/ron/blob/master/docs/grammar.md
;; for the definitions on Map in RON.

(defun ron-new-map ()
  "Create a new Elisp object corresponding to any empty RON map."
  (make-hash-table :test #'equal))

(defun ron-put-map (key value map)
  "Insert KEY and VALUE into MAP."
  (puthash key value map)
  map)

(defun ron-read-map ()
  "Read a RON map object at point."
  ;; Skip '{'
  (ron-advance)

  (let ((elements (ron-new-map))
        key value)
    (while (/= (ron-peek) ?\})

      ;; Read the key
      (setq key (ron-read))

      ;; Skip the whitespace
      (ron-skip-whitespace)

      ;; Handle ':'
      (if (= (ron-peek) ?:)
          (ron-advance)
        (signal 'ron-map-format (list ":" (ron-peek))))

      ;; Skip the whitespace
      (ron-skip-whitespace)

      ;; Read the value
      (setq value (ron-read))

      ;; Insert the key and value
      (setq elements (ron-put-map key value elements))

      ;; Skip the whitespace
      (ron-skip-whitespace)

      ;; If not the end, handle comma
      (when (/= (ron-peek) ?\})
        (if (= (ron-peek) ?,)
            (progn (ron-advance)
                   (ron-skip-whitespace))
          (signal 'ron-map-format (list "," (ron-peek))))))

    ;; Skip the '}'
    (ron-advance)
    (or (looking-at (rx ron--post-value))
        (signal 'ron-map-format (list (point))))

    elements))

;; Boolean
;; See
;; https://github.com/ron-rs/ron/blob/master/docs/grammar.md
;; for the definitions on Boolean in RON.

(defun ron-read-true ()
  "Read a RON true value at point."
  ;; Skip 'true'
  (ron-advance 4)
  (make-ron-true))

(defun ron-read-false ()
  "Read a RON false value at point."
  ;; Skip 'false'
  (ron-advance 5)
  (make-ron-false))

;; Tuple
;; See
;; https://github.com/ron-rs/ron/blob/master/docs/grammar.md
;; for the definitions on Tuple in RON.

(defun ron-read-tuple ()
  "Read a RON tuple object at point."
  ;; Skip '('
  (ron-advance)
  (let ((elements nil))
    (while (/= (ron-peek) ?\))

      ;; Push the value to elements
      (push (ron-read) elements)

      (ron-skip-whitespace)
      (when (/= (ron-peek) ?\))
        (if (= (ron-peek) ?,)
            (progn (ron-advance)
                   (ron-skip-whitespace))
          (signal 'ron-tuple-format (list "," (ron-peek))))))

    ;; Skip ')'
    (ron-advance)
    (or (looking-at (rx ron--post-value))
        (signal 'ron-tuple-format (list (point))))

    (nreverse elements)))

(defun ron-read-named-tuple ()
  "Read a RON named tuple object at point."

  ;; Skip '('
  (ron-advance)

  (let ((elements)
        key value)

    (while (/= (ron-peek) ?\))

      ;; Read the key
      (setq key (ron-read))

      ;; Skip the whitespace
      (ron-skip-whitespace)

      (when (/= (ron-peek) ?:)
        (signal 'ron-tuple-format (list ":" (ron-peek))))

      ;; Skip the ':'
      (ron-advance)

      ;; Read the value
      (setq value (ron-read))

      (setq elements (cons (cons key value) elements))

      (ron-skip-whitespace)
      (when (/= (ron-peek) ?\))
        (if (= (ron-peek) ?,)
            (progn
              (ron-advance)
              (ron-skip-whitespace))
          (signal 'ron-tuple-format (list "," (ron-peek))))))

    ;; Skip the ')'
    (ron-advance)

    (or (looking-at (rx ron--post-value))
        (signal 'ron-tuple-format (list (point))))

    (nreverse elements)))

;; Struct
;; See
;; https://github.com/ron-rs/ron/blob/master/docs/grammar.md
;; for the definitions on Enum in RON.

(defun ron-read-tuple-cases ()
  "Read a RON Tuple or a Named Tuple at point."

  (let (restore
        (variant 'tuple))

    ;; Save a restore point
    (setq restore (point))

    ;; Skip the '('
    (ron-advance)

    (when (/= (ron-peek) ?\))
      ;; Read the value (or key)
      (ron-read)

      ;; Skip the whitespace
      (ron-skip-whitespace)

      ;; Determine the case
      (cond
       ((= (ron-peek) ?\)) (setq variant 'tuple))
       ((= (ron-peek) ?,) (setq variant 'tuple))
       ((= (ron-peek) ?:) (setq variant 'named))
       (t (signal 'ron-tuple-format (list (ron-peek))))))

    ;; Restore the save point
    (goto-char restore)

    ;; Dispatch the case
    (if (eq variant 'tuple)
        (ron-read-tuple)
      (ron-read-named-tuple))))

(with-temp-buffer
  (save-excursion (insert "()"))
  (ron-read-tuple-cases))

(defun ron-read-struct ()
  "Read a RON struct object at point."
  (let (identifier)
    (if (= (ron-peek) ?\()
        (ron-read-tuple-cases)

      ;; Read the enum identifier
      (setq identifier (ron-read-identifier))

      ;; Skip the whitespace
      (ron-skip-whitespace)

      ;; Read the tuple cases
      (if (/= (ron-peek) ?\()
          identifier
        (let ((tuple (ron-read-tuple-cases)))
          (list (intern (concat ":" (symbol-name identifier))) tuple))))))

;; Identifier
;; See
;; https://github.com/ron-rs/ron/blob/master/docs/grammar.md
;; for the definitions on Identifier in RON.

(defun ron-read-identifier ()
  "Read a RON identifier object at point."

  ;; Read the identifier
  (if (looking-at (rx ron--identifier))
      (goto-char (match-end 0))
    (signal 'ron-identifier-format (list (point))))

  (prog1 (intern (match-string 0))
  (or (looking-at (rx ron--post-value))
      (signal 'ron-identifier-format (list (point))))))

;;;; Pretty printing & minimizing

(defun ron-pretty-print-buffer (&optional minimize)
  "Pretty-print current buffer.
With prefix argument MINIMIZE, minimize it instead."
  (interactive "P")
  (ron-pretty-print (point-min) (point-max) minimize))

(defun ron-pretty-print (begin end &optional minimize)
  "Pretty-print selected region, BEGIN and END.
With prefix argument MINIMIZE, minimize it instead."
  (interactive "r\nP")
  (let ((ron-encoding-pretty-print (null minimize))
        (orig-buffer (current-buffer))
        error)

    (with-temp-buffer

      (let ((tmp-buffer (current-buffer)))

        (set-buffer orig-buffer)
        (replace-region-contents
         begin end
         (lambda ()

           (let ((pos (point))
                 (keep-going t))
             (while keep-going
               (condition-case err
                   (let ((ron (ron-read)))
                     (setq pos (point))
                     (set-buffer tmp-buffer)
                     (insert (ron-encode ron))
                     (set-buffer orig-buffer))
                 (t

                  (setq keep-going nil)
                  (set-buffer orig-buffer)

                  (append-to-buffer tmp-buffer pos (point-max))

                  (unless (eq (car err) 'ron-end-of-file)
                    (setq error err)))))
             tmp-buffer))
         2.0
         64)))

    (when error
      (signal (car error) (cdr error)))))

(provide 'ron)
;;; ron.el ends here
