;;; example.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Luke Holland
;;
;; Author: Luke Holland
;; Maintainer: Luke Holland
;; Created: February 23, 2026
;; Modified: February 23, 2026
;; Version: 0.0.1
;; Keywords: files languages lisp text tools
;; Homepage: https://github.com/yelobat/example
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;; This example showcases a practical usecase for this package.
;; If you are familiar with the bevy game engine then you will
;; find this example very useful. Loading scenes and manipulating
;; them can be done in Emacs now by using this package to read
;; scenes, modify them with Emacs Lisp, and then write them back
;; to files.
;;
;;; Code:

(require 'map)
(require 'ron)

(defconst example-scene-path (expand-file-name "examples/example.scn.ron")
  "The example scene we will load.")

(defconst example-modified-scene-path (expand-file-name "examples/output.scn.ron")
  "The modified scene which will be saved.")

(defvar example-scene nil
  "The variable which will store the example scene at `example-scene-path'.")

(defvar example-entities nil
  "The variable which will store the entities in `example-scene'.")

(defvar example-entity-ids nil
  "The variable which will store the entity IDs in `example-entities'.")

;; Let's load the scene
(setq example-scene
      (ron-read-file example-scene-path))

;; Let's extract the entities
(setq example-entities (alist-get 'entities example-scene))

;; Let's extract the entity IDs
(setq example-entity-ids (map-keys example-entities))

;; Now let's say that we want to update the Transforms of each of the entities
;; by making them all translated by 10px.

;; We need to be able to add lists together and treat them like vectors.
(defun vector-add (&rest vs)
  "Add all of the vectors in VS together.
Assume length of each vector in VS have length of 3."
  (let ((tx 0.0)
        (ty 0.0)
        (tz 0.0))
    (dolist (v vs)
      (pcase v
        (`(,x ,y ,z) :when (and (numberp x) (numberp y) (numberp z))
         (setq tx (+ x tx))
         (setq ty (+ y ty))
         (setq tz (+ z tz)))))
    (list tx ty tz)))

;; Now we can iterate over all of the entities and update their translation
;; fields inside of the Transform component.
(dolist (id example-entity-ids)
  (let* ((entity (gethash id example-entities))
         (components (alist-get 'components entity))
         (key "bevy_transform::components::transform::Transform")
         (transform (gethash key components)))
    (let ((translation (pop transform)))
      (push (cons 'translation (vector-add (list 10.0 0.0 0.0) (cdr translation))) transform)
      (puthash key transform components))))

;; Let's write the output to another file.
(ron-write-file example-scene example-modified-scene-path)
(find-file example-modified-scene-path)

(provide 'example)
;;; example.el ends here
