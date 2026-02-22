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
    (ron-read-comma)
    (buffer-substring (point) (point-max))) "")))

(ert-deftest whitespace-comma-test ()
  (should (string-equal (with-temp-buffer
    (save-excursion
      (insert "    /* A comment before a comma */,"))
    (ron-read-comma)
    (buffer-substring (point) (point-max))) "")))

(ert-deftest whitespace-and-more-commas-test ()
  (should (string-equal (with-temp-buffer
    (save-excursion
      (insert "    /* A comment /* in a comment */ before a comma */\n,\n/**/,"))
    (ron-read-comma)
    (ron-read-comma)
    (buffer-substring (point) (point-max))) "")))

(provide 'test-ron)
;;; test-ron.el ends here
