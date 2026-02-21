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
;; Package-Requires: ((emacs "27.1"))
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

;;;; Parameters

;; Error conditions

(define-error 'ron-error "Unknown RON error")
(define-error 'ron-end-of-file "End of file when parsing RON"
              '(end-of-file ron-error))

;;;; Parsing

;; Whitespace + Comments
;; See
;; https://github.com/ron-rs/ron/blob/master/docs/grammar.md
;; for the definitions on whitespace in RON.

(rx-define ron--whitespace-single
  (in ?\n ?\t ?\r ?\s ?\u000B ?\u000C
      ?\u0085 ?\u200E ?\u200F ?\u2028 ?\u2029))

(rx-define ron--whitespace-pre-value
  (| ron--whitespace-single "/"))

(define-inline ron-skip-whitespace ()
  "Skip past the whitespace at point."
  (inline-quote
   (while (looking-at-p (rx ron--whitespace-pre-value))
     (cond
      ;; Skip line if it is a single line comment.
      ((looking-at-p (rx "//"))
       (forward-line))

      ;; Skip nested block, handling recursive nested
      ;; blocks in the process.
      ((looking-at-p (rx "/*"))
       (let ((depth 1))
         (forward-char 2)
         (while (> depth 0)
           (cond
            ((eobp) (signal 'ron-end-of-file ()))
            ((looking-at-p (rx "/*"))
             (setq depth (1+ depth))
             (forward-char 2))
            ((looking-at-p (rx "*/"))
             (setq depth (1- depth))
             (forward-char 2))
            (t (forward-char))))))

      ;; Handle normal whitespace
      (t (or (looking-at (rx ron--whitespace-single))
             (signal 'ron-end-of-file ()))
         (goto-char (match-end 0)))))))

(provide 'ron)
;;; ron.el ends here
