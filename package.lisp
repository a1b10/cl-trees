;;;; package.lisp
(in-package :cl-user)

(defpackage #:cl-trees
  (:use #:cl)
  (:export #:interval
           #:interval-node
           #:before-p
           #:overlapping-p
           #:interval-insert
           #:interval-find
           #:find-before
           #:find-overlapping
           #:find-after)
