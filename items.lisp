(in-package #:secretary)

(defun load-items ()
  (load-data *items* (data-filename "items")))

(defun save-items ()
  (save-data *items* (data-filename "items")))

(defparameter *items*
  (let ((init-contents (load-items)))
    (make-array (length init-contents)
                :initial-contents init-contents
                :adjustable t)))

;; Item functions

(defun record-item (item)
  (vector-push-extend item *items*))

(defun make-item (type description added deadline fulfilled)
  (list :type type
        :description description
        :added added
        :deadline deadline
        :fulfilled fulfilled))

(defun add-item ()
  (record-item
   (make-item
    (prompt-read "Type")
    (prompt-read "Description")
    (iso-now)
    "2025-01-01T00:00:00"
    ""))
  (save-items))

(defun groceries ()
  (remove-if-not
   #'(lambda (item)
       (and (equal (getf item :fulfilled) "")
            (equal (getf item :type) "grocery")))
   *items*))

(defun grocery-list ()
  (loop
     for item being the elements of (groceries)
       do (format t
                  "* ~a~%"
                  (getf item :description))))
