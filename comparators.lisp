(in-package #:secretary)

(defun make-comparison-expr (field value)
  `(equal (getf event ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defun make-match-expr (field value)
  `(search ,value (getf event ,field)))

(defun make-match-list (fields)
  (loop while fields
     collecting (make-match-expr (pop fields) (pop fields))))
