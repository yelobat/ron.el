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

;; Utilities

;; Tests: Whitespace + Comments

(ert-deftest single-line-whitespace-test ()
  (should (string-equal (with-temp-buffer
                          (save-excursion
                            (insert "    // This comment has a new line at the end
// This one can finish on an empty line"))
                          (ron-skip-whitespace)
                          (buffer-substring (point) (point-max))) "")))

(ert-deftest multi-line-whitespace-test ()
  (should (string-equal (with-temp-buffer
                          (save-excursion
                            (insert "    /* A comment /* in a */ comment */\n/********\n\r\t***//**/"))
                          (ron-skip-whitespace)
                          (buffer-substring (point) (point-max))) "")))

(ert-deftest single-and-multi-line-whitespace-test ()
  (should (string-equal (with-temp-buffer
                          (save-excursion
                            (insert "\n\n\n\t\n\t\r\n/*/*\n*\n*\n**///Blah Blah Blah\n*/\n505"))
                          (ron-skip-whitespace)
                          (buffer-substring (point) (point-max))) "505")))

;; Tests: Commas

(ert-deftest simple-comma-test ()
  (should (string-equal (with-temp-buffer
                          (save-excursion
                            (insert ","))
                          (ron-skip-comma)
                          (buffer-substring (point) (point-max))) "")))

(ert-deftest whitespace-comma-test ()
  (should (string-equal (with-temp-buffer
                          (save-excursion
                            (insert "    /* A comment before a comma */,"))
                          (ron-skip-comma)
                          (buffer-substring (point) (point-max))) "")))

(ert-deftest whitespace-and-more-commas-test ()
  (should (string-equal (with-temp-buffer
                          (save-excursion
                            (insert "    /* A comment /* in a comment */ before a comma */\n,\n/**/,"))
                          (ron-skip-comma)
                          (ron-skip-comma)
                          (buffer-substring (point) (point-max))) "")))

;; Tests: Numbers

(ert-deftest unsigned-integer-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "    /* A comment /* /**/ in a comment */ before an integer */512"))
               (ron-read-number)) 512)))

(ert-deftest negative-integer-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "    /* A comment /* /**/ in a comment */ before an integer */\n-431"))
               (ron-read-number)) (- 431))))

(ert-deftest positive-integer-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "    /* A comment /* /**/ in a comment */ before an integer */\n\t999"))
               (ron-read-number)) 999)))

(ert-deftest binary-integer-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "    /* A comment /* /**/ in a comment */ before an integer */\n-0b00110"))
               (ron-read-number)) (- 6))))

(ert-deftest octal-integer-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "    /* A comment /* /**/ in a comment */ before an integer */\n-0o12345"))
               (ron-read-number)) (- 5349))))

(ert-deftest hexadecimal-integer-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "/*99*///999 \n-0x12345"))
               (ron-read-number)) (- 74565))))

(ert-deftest positive-float-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "/* // Comment*/\n\n\n\t\t\n+12345.12345"))
               (ron-read-number)) 12345.12345)))

(ert-deftest negative-float-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "/* // Comment*/\n\n\n\t\t\n-12345.12345"))
               (ron-read-number)) (- 12345.12345))))

(ert-deftest inf-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "/* // Comment*/\n\n\n\t\t\ninf"))
               (ron-read-number)) (string-to-number "inf"))))

(ert-deftest NaN-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "/* // Comment*/\n\n\n\t\t\nNaN"))
               (ron-read-number)) (string-to-number "NaN"))))

(ert-deftest not-NaN-test ()
  (should-error
   (with-temp-buffer
     (save-excursion
       (insert "/* // Comment*/\n\n\n\t\t\nnaN"))
     (ron-read-number))
   :type 'ron-number-format))

(ert-deftest exponent-test ()
  (should (= (with-temp-buffer
               (save-excursion
                 (insert "/* // Comment*/\n\n\n\t\t\n.540E54"))
               (ron-read-number)) 5.4e53)))

(provide 'test-ron)
;;; test-ron.el ends here
