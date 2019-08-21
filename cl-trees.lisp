;;;; cl-trees.lisp

(in-package #:cl-trees)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; binary search trees (PBI book)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct node
  contents
  (prev nil)
  (post nil))

(defun bst-insert (obj bst before)
  (cond ((null bst) (make-node :contents obj))
	(t
	 (let ((contents (node-contents bst)))
	   (cond ((eql obj contents) bst)
		 ((funcall before obj contents)
		  (make-node
		   :contents contents
		   :prev (bst-insert obj (node-prev bst) before)
		   :after (node-after bst)))
		 (t
		  (make-node
		   :contents contents
		   :prev (node-prev bst)
		   :after (bst-insert obj (node-post bst) before))))))))

;;; cost is very low because all rest ist shared with previous tree

;;; entry finding is simpler than divide and conqure of array

(defun bst-find (obj bst before)
  (cond ((null bst) nil)
	(t
	 (let ((contents (node-contents bst)))
	   (cond ((eql obj contents) bst)
		 ((funcall before obj contents)
		  (bst-find obj (node-prev bst) before))
		 (t
		  (bst-find obj (nocd-post bst) before)))))))

;;; removing node: Graham 137 page 74
;;; destructive version: Graham 137 page 202
		  
		 
		 

