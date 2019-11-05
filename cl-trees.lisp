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

;;; entry finding is simpler than divide and conquer of array

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; intervall trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct interval
  (start 0.0)
  (end 0.0)
  (description nil))

(defstruct inverval-node
  contents
  (before nil)
  (overlapping nil)
  (after nil))

(defun before-p (iv1 iv2)
  (< (interval-start iv1) (interval-end iv2)))

(defun overlapping-p (iv1 iv2 &key (type "open"))
  (cond ((equal type "open")
         (and (< (interval-start iv2) (interval-end iv1))
              (< (interval-start iv1) (interval-end iv2))))
        ((equal type "closed")
         (and (not (before-p iv1 iv2))
              (not (before-p iv2 iv1))))
        (t
         (error "Please specify correct :type")))) ; is this really correct?

(defun interval-insert (iv tree &key (type "open"))
  (if (null tree)
      (make-interval-node :contents iv))
      (let ((contents (interval-node-contents tree)))
        (make-interval-node
         :contents contents
         :before (if (before-p iv contents)
                     (interval-insert iv (interval-node-before tree))
                     (interval-node-before tree))
         :overlapping (if (overlapping-p iv contents)
                          (interval-insert iv (interval-node-overlapping tree))
                          (interval-node-overlapping tree))
         :after (if (before-p contents iv) ;; after
                    (interval-insert iv (interval-node-after tree))
                    (interval-node-after tree)))))

(defun interval-find (iv tree)
  (if (null tree)
      nil
      (let ((contents (interval-node-contents tree)))
        (if (equal iv contents)
            tree
            (interval-find iv
                           (cond ((before-p iv contents)
                                  (interval-node-before tree))
                                 ((overlapping-p iv contents)
                                  (interval-node-overlapping tree))
                                 ((after-p iv contents)
                                  (interval-node-after tree))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find all before intervals in tree to iv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun find-prev (iv tree)
;;   (if (null tree)
;;       nil
;;       (let ((contents (interval-node-contents tree)))
;;         (cond ((prev-p iv contents)
;;                ;; search the before AND overlapping subtrees
;;                (append (find-prev iv (interval-node-prev tree))
;;                        (find-prev iv (interval-node-over tree))))
;;               ((over-p iv contents)
;;                ;; search all three subtrees
;;                (append (find-prev iv (interval-node-prev tree))
;;                        (find-prev iv (interval-node-over tree))
;;                        (find-prev iv (interval-node-post tree))))
;;               ((after iv contents) ; error in book! forgot opening paren
;;                ;; include current interval AND search overlaps and after subtrees
;;                (cons contents
;;                      (append (find-prev iv (interval-node-over tree))
;;                              (find-prev iv (interval-node-post tree)))))))))

;; if interval is before current one,
;; don't include current one or the ones AFTER it,
;; since they will surely not be before given interval, but to search other two branches.
;; if there is overlap, current interval is still not included, but all three subtrees needs to be searched.
;; if given interval is after current interval, current interval does satisfy
;; criterion, so put it on beginning of list
;; resulting from searching the overlaps and after subtrees.

;; which is actually:

;; (defun find-before (iv tree)
;;   (cond ((null tree) nil)
;;         (t (let ((contents (interval-node-contents tree)))
;;              (cond ((before-p iv contents)
;;                     ;; search before AND overlapping subtrees
;;                     (append (find-before iv (interval-node-before tree))
;;                             (find-before iv (interval-node-overlapping tree))))
;;                    ((overlapping-p iv contents)
;;                     ;; search all three subtrees
;;                     (append (find-before iv (interval-node-before tree))
;;                             (find-before iv (interval-node-overlapping tree))
;;                             (find-before iv (interval-node-after tree))))
;;                    (t ;; after
;;                     ;; search after AND overlapping subtrees
;;                     (append (list contents)
;;                             (find-before iv (interval-node-overlapping tree))
;;                             (find-before iv (interval-node-after tree)))))))))

;;;; using append you can emulate 'cons-ing
;; (cons 'a '(b c)) ;; => '(a b c)
;; (append (list a) '(b c)) => '(a b c)

;;;; nice trick:
;;;; using append you can leave out elements by putting/returning nil at that position
;; (append '(a) '() '(b)) => '(a b)
;; (append '(a) nil '(c)) => '(a c)
;; (append nil '(b c)) => '(b c)

;; that I will use now.

(defun find-before (iv tree)
  (cond ((null tree) nil)
        (t (let ((contents (interval-node-contents tree)))
             (append (if (before-p contents iv) (list contents) '())
                     (find-before iv (if (before-p contents iv) '() (interval-node-before tree)))
                     (find-before iv (interval-node-overlapping tree))
                     (find-before iv (if (before-p iv contents) '() (interval-node-after tree))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find all overlapping intervals in tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; do corresponding for find-post and find-overlap

;; (defun find-over (iv tree)
;;   (if (null tree)
;;       nil
;;       (let ((contents (interval-node-contents tree)))
;;         (cond ((prev-p iv contents)
;;                ;; search all but don't add current interval
;;                (append (find-over iv (interval-node-prev tree))
;;                        (find-over iv (interval-node-over tree))
;;                        (find-over iv (interval-node-post tree))))
;;               ((over-p iv contents)
;;                ;; include current interval AND search all tree
;;                (cons contents
;;                      (append (find-over iv (interval-node-prev tree))
;;                              (find-over iv (interval-node-over tree))
;;                              (find-over iv (interval-node-post tree)))))
;;               ((post-p iv contents)
;;                ;; search all but don't add current interval
;;                (append (find-over iv (interval-node-prev tree))
;;                        (find-over iv (interval-node-over tree))
;;                        (find-over iv (interval-node-post tree))))))))

(defun find-overlapping (iv tree)
  (cond ((null tree) nil)
        (t ;; if overlapping include current interval AND search all tree else only search all tree
         (append (if (overlapping-p iv (interval-node-contents tree)) (list contents) nil)
                 (find-overlapping iv (interval-node-before tree))
                 (find-overlapping iv (interval-node-overlapping tree))
                 (find-overlapping iv (interval-node-after tree))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find all after intervals in tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;; the same for after

;; (defun find-post (iv tree)
;;   (if (null tree)
;;       nil
;;       (let ((contents (interval-node-contents tree)))
;;         (cond ((prev-p iv contents)
;;                ;; include current interval AND search the overlaps AND post subtrees
;;                (cons contents
;;                      (append (find-post iv (interval-node-over tree))
;;                              (find-post iv (interval-node-post tree)))))
;;               ((over-p iv contents)
;;                ;; search all three subtrees
;;                (append (find-post iv (interval-node-prev tree))
;;                        (find-post iv (interval-node-over tree))
;;                        (find-post iv (interval-node-post tree))))
;;               ((post-p iv contents)
;;                ;; search overlaps and post subtrees
;;                (append (find-post iv (interval-node-over tree))
;;                        (find-post iv (interval-node-post tree))))))))

(defun find-after (iv tree)
  (cond ((null tree) nil)
        (t (let ((contents (interval-node-contents tree)))
             (append (if (before-p iv contents) (list contents) '())
                     (find-after iv (if (before-p contents iv) '() (interval-node-before tree)))
                     (find-after iv (interval-node-overlapping tree))
                     (find-after iv (if (before-p iv contents) '() (interval-node-after tree))))))))





























