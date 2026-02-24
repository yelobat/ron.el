;;; test-ron.el --- ERT test suite for ron.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Luke Holland
;;
;; Author: Luke Holland
;; Maintainer: Luke Holland
;; Created: February 21, 2026
;; Modified: February 21, 2026
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/yelobat/test-ron
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Testing RON parsing functionality provided by ron.el
;; using the ERT (Emacs Lisp Regression Testing) tool.
;;  Description
;;
;;; Code:

(require 'ron)
(require 'ert)

;; Tests: Numbers

(ert-deftest unsigned-integer-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "    /* A comment /* /**/ in a comment */ before an integer */512"))
               (ron-read)) 512)))

(ert-deftest negative-integer-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "    /* A comment /* /**/ in a comment */ before an integer */\n-431"))
               (ron-read)) (- 431))))

(ert-deftest positive-integer-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "    /* A comment /* /**/ in a comment */ before an integer */\n\t999"))
               (ron-read)) 999)))

(ert-deftest binary-integer-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "    /* A comment /* /**/ in a comment */ before an integer */\n-0b00110"))
               (ron-read)) (- 6))))

(ert-deftest octal-integer-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "    /* A comment /* /**/ in a comment */ before an integer */\n-0o12345"))
               (ron-read)) (- 5349))))

(ert-deftest hexadecimal-integer-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "/*99*///999 \n-0x12345"))
               (ron-read)) (- 74565))))

(ert-deftest positive-float-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "/* // Comment*/\n\n\n\t\t\n+12345.12345"))
               (ron-read)) 12345.12345)))

(ert-deftest negative-float-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "/* // Comment*/\n\n\n\t\t\n-12345.12345"))
               (ron-read)) (- 12345.12345))))

(ert-deftest inf-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "/* // Comment*/\n\n\n\t\t\ninf"))
               (ron-read)) (string-to-number "inf"))))

(ert-deftest NaN-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "/* // Comment*/\n\n\n\t\t\nNaN"))
               (ron-read)) (string-to-number "NaN"))))

(ert-deftest exponent-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "/* // Comment*/\n\n\n\t\t\n.540E54"))
               (ron-read)) 5.4e53)))

;; Tests: Bytes

;;(ert-deftest byte-ascii-test ()
;;  (should (= (with-temp-buffer
;;               (save-excursion
;;                 (insert "/* // Comment*/\n\n\n\t\t\n  b'f'"))
;;               (ron-read-byte)) ?f)))
;;
;;(ert-deftest byte-escaped-ascii-test ()
;;  (should (= (with-temp-buffer
;;               (save-excursion
;;                 (insert "/* // Comment*/\n\n\n\t\t\n  b'\0'"))
;;               (ron-read-byte)) ?\0)))
;;
;;(ert-deftest byte-escaped-byte-test ()
;;  (should (= (with-temp-buffer
;;               (save-excursion
;;                 (insert "/* // Comment*/\n\n\n\t\t\n  b'\x77'"))
;;               (ron-read-byte)) ?\x77)))

;; Tests: Optional

(ert-deftest optional-some-test ()
  (should (equal (with-temp-buffer
               (save-excursion
                 (insert "/* // Comment*/\n\n\n\t\t\n  Some /**/ (9.0e2)"))
               (ron-read)) (ron-new-some 900.0))))

(ert-deftest optional-none-test ()
  (should (equal (with-temp-buffer
               (save-excursion
                 (insert "/* // Comment*/\n\n\n\t\t\n None Some /**/ (9.0e2)"))
               (ron-read)) (ron-new-none))))

;; Tests: Lists

(ert-deftest list-empty-test ()
  (should (equal (with-temp-buffer
                   (save-excursion
                     (insert "/* Leading comment */\n [] // Trailing comment"))
                   (ron-read)) (list))))

(ert-deftest list-elements-test ()
  (should (equal (with-temp-buffer
                   (save-excursion
                     (insert "/* Leading comment */\n [/*
Comments allowed with elements*/ 5, 2] // Trailing comment"))
                   (ron-read)) (list 5 2))))

;; Tests: Maps

(defun eqhash (a b)
  "Compare two hash tables A and B, return t if they are equal."
  (and
   (hash-table-p a)
   (hash-table-p b)
   (eq (hash-table-count a) (hash-table-count b))
   (catch 'flag (maphash (lambda (key value)
                           (or (equal (gethash key b) value)
                               (throw 'flag nil)))
                         a)
          (throw 'flag t))))

(ert-deftest map-empty-test ()
  (should (eqhash (with-temp-buffer
                   (save-excursion
                     (insert "/* Leading comment */\n {} // Trailing comment"))
                   (ron-read)) (ron-new-map))))

(ert-deftest map-elements-test ()
  (let ((map (ron-new-map)))
    (setq map (ron-put-map 5 7 map))
    (setq map (ron-put-map (ron-new-some 5) 9 map))
  (should (eqhash (with-temp-buffer
                   (save-excursion
                     (insert "/* Leading comment */\n {5: 7, \n Some  (5): /**/ 9} // Trailing comment"))
                   (ron-read)) map))))

;; Tests: Tuple

(ert-deftest tuple-empty-test ()
  (should (equal (with-temp-buffer
                   (save-excursion
                     (insert "/* Leading comment */\n () // Trailing comment"))
                   (ron-read)) (vector))))

(ert-deftest tuple-elements-test ()
  (should (equal (with-temp-buffer
                   (save-excursion
                     (insert "/* Leading comment */\n ((()), 2, /**/ 3) // Trailing comment"))
                   (ron-read)) (vector (vector (vector)) 2 3))))

;; Tests: Boolean

(ert-deftest true-test ()
  (should (eq (with-temp-buffer
                   (save-excursion
                     (insert "/* Leading comment */\n true // Trailing comment"))
                   (ron-read)) t)))

(ert-deftest false-test ()
  (should (eq (with-temp-buffer
                   (save-excursion
                     (insert "/* Leading comment */\n false // Trailing comment"))
                   (ron-read)) ron-false)))

;; Tests: Identifier

(ert-deftest identifier-test ()
  (should (eq (with-temp-buffer
                   (save-excursion
                     (insert "/* Leading comment */\n identifier // Trailing comment"))
                   (ron-read)) 'identifier)))

;; Tests: Strings

(ert-deftest string-empty-test ()
  (should (equal (with-temp-buffer
                (save-excursion
                  (insert "/* Leading comment */\n \"\" // Trailing comment"))
                (ron-read)) "")))

(ert-deftest string-standard-test ()
  (should (equal (with-temp-buffer
                (save-excursion
                  (insert "/* Leading comment */\n \"Standard String\" // Trailing comment"))
                (ron-read)) "Standard String")))

(ert-deftest string-raw-test ()
  (should (equal (with-temp-buffer
                (save-excursion
                  (insert "/* Leading comment */\n r#\"Raw String\"# // Trailing comment"))
                (ron-read)) "\"Raw String\"")))

;; Tests: Structs

(ert-deftest struct-empty-test ()
  (should (eq (with-temp-buffer
                (save-excursion
                  (insert "/* Leading comment */\n () // Trailing comment"))
                (ron-read)) (vector))))

(ert-deftest struct-empty-labelled-test ()
  (should (equal (with-temp-buffer
                (save-excursion
                  (insert "/* Leading comment */\n Label() // Trailing comment"))
                (ron-read)) (list 'Label (vector)))))

(ert-deftest struct-populated-test ()
  (should (equal (with-temp-buffer
                (save-excursion
                  (insert "/* Leading comment */\n (5, 4, 1, 2, 3) // Trailing comment"))
                (ron-read)) (vector 5 4 1 2 3))))

(ert-deftest struct-populated-named-test ()
  (let (alist)
    (setq alist (cons (cons 'id 16) alist))
    (setq alist (cons (cons 'five 9) alist))
    (setq alist (cons (cons 6 'hey) alist))
    (setq alist (cons (cons 9 'four) alist))
    (setq alist (nreverse alist))
    (should
     (equal
      (with-temp-buffer
        (save-excursion
          (insert "/* Leading comment */\n (id: 16, /**/ five: 9, 6: hey, 9: four) // Trailing comment"))
        (ron-read)) alist))))

;; Tests: File Reading

(provide 'test-ron)
;;; test-ron.el ends here
