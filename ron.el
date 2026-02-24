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

;;;; Parameters

(defvar ron-false :ron-false
  "Value to use when reading RON `false'.")

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

;;;; Reader utilities

(define-inline ron-advance (&optional n)
  "Advances N characters forward, or 1 character if N is nil.
On reaching the end of the accessible region of the buffer, stop
and signal an error."
  (inline-quote (forward-char ,n)))

(define-inline ron-peek ()
  "Return the N + 1 characters after point, or the character at point if N is nil.
At the end of the accessible region of the buffer, return 0."
  (inline-quote (following-char)))

;;;; Reader

;; TODO Figure out how to make this run faster
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
      (setq number (substring (string-replace "_" "" (match-string 0)) 2))
      (setq base 16))
     ((looking-at-p "0b")
      (looking-at (rx ron--integer))
      (setq number (substring (string-replace "_" "" (match-string 0)) 2))
      (setq base 2))
     ((looking-at-p "0o")
      (looking-at (rx ron--integer))
      (setq number (substring (string-replace "_" "" (match-string 0)) 2))
      (setq base 8))
     (t (or (looking-at (rx (| ron--float ron--integer)))
            (signal 'ron-number-format (list (point))))
        (setq number (match-string 0))))

    (goto-char (match-end 0))
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

(defun ron-new-some (value)
  "Create a list which represents the Some tuple variant with VALUE."
  (list 'Some value))

(defun ron-new-none ()
  "Create a list which represents the Some tuple variant, as None."
  (list 'None nil))

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
    (nreverse elements)))

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
  t)

(defun ron-read-false ()
  "Read a RON false value at point."
  ;; Skip 'false'
  (ron-advance 5)
  ron-false)

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

    (nreverse (vconcat elements))))

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

    ;; Check if this is a tuple
    (if (/= (ron-peek) ?\()
        nil

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
        (ron-read-named-tuple)))))

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
      (let ((tuple (ron-read-tuple-cases)))
        (if tuple
            (list identifier tuple)
          identifier)))))

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

(provide 'ron)
;;; ron.el ends here
