;;;; cl-trees.asd

(asdf:defsystem #:cl-trees
  :name "cl-trees"
  :description "Interval trees with content for Common Lisp"
  :author "Gwang-Jin Kim"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "cl-trees")))
