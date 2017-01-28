;;;; secretary.lisp

(in-package #:secretary)

(defvar *events* nil)
(defparameter *filename* "event-list.db")

;; Time functions

(defun universal-from-iso (timestamp)
  (encode-universal-time
   (parse-integer (subseq timestamp 17)) ; seconds
   (parse-integer (subseq timestamp 14 16)) ; minutes
   (parse-integer (subseq timestamp 11 13)) ; hours
   (parse-integer (subseq timestamp 8 10)) ; date
   (parse-integer (subseq timestamp 5 7)) ; month
   (parse-integer (subseq timestamp 0 4))))

(defun iso-from-universal (universal-timestamp)
  (multiple-value-bind
        (sec min hr date month year)
      (decode-universal-time universal-timestamp)
    (format nil
            "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
            year month date hr min sec)))

;; Event functions

(defun make-event (timestamp actors description tags)
  (list :id (+ 1 (length *events*))
        :timestamp timestamp
        :actors actors
        :description description
        :tags tags))

(defun record-event (event)
  (push event *events*) event)

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun add-event (&optional type)
  (cond
    ((equal type :todo)
     (record-event
      (make-event
       "todo"
       "me"
       (prompt-read "Description")
       (prompt-read "Tags"))))
    ((equal type :trade)
     (record-event
      (make-event
       (prompt-read "Timestamp")
       "me"
       (prompt-read "Description")
       "trade")))
    ((equal type :personal)
     (record-event
      (make-event
       (iso-from-universal (get-universal-time))
       "me"
       (prompt-read "Description")
       (prompt-read "Tags"))))
    ((equal type :time)
     (record-event
      (make-event
       (prompt-read "Timestamp")
       (prompt-read "Actors")
       (prompt-read "Description")
       (prompt-read "Tags"))))
    (t
     (record-event
      (make-event
       (iso-from-universal (get-universal-time))
       (prompt-read "Actors")
       (prompt-read "Description")
       (prompt-read "Tags"))))))

(defun add-todo ()
  (add-event :todo))

(defun save-events ()
  (with-open-file (out *filename*
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *events* out)))
  (length *events*))

(defun load-events ()
  (with-open-file (in *filename*)
    (with-standard-io-syntax
      (setf *events* (read in))))
  (length *events*))

(defun make-comparison-expr (field value)
  `(equal (getf event ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (event) (and ,@(make-comparisons-list clauses))))

(defun make-match-expr (field value)
  `(search ,value (getf event ,field)))

(defun make-match-list (fields)
  (loop while fields
     collecting (make-match-expr (pop fields) (pop fields))))

(defmacro matching (&rest clauses)
  `#'(lambda (event) (and ,@(make-match-list clauses))))

(defun select (selector-fn)
  (remove-if-not selector-fn *events*))

(defun update (selector-fn &key timestamp actors description tags)
  (setf *events*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if timestamp (setf (getf row :timestamp) timestamp))
               (if actors (setf (getf row :actors) actors))
               (if description (setf (getf row :description) description))
               (if tags (setf (getf row :tags) tags)))
             row)
         *events*))
  (length *events*))

(defun last-n-events (num-events)
  (subseq *events* 0 num-events))

(defun compare-ids (first-event second-event)
  (< (getf first-event :id)
                (getf second-event :id)))

(defun todos ()
  "Returns a list of todos sorted by ID"
  (dolist
      (event
        (sort
         (select (where :timestamp "todo")) #'compare-ids))
    (format t
            "~a) ~a (~a)~%"
            (getf event :id)
            (getf event :description)
            (getf event :tags))))

(defun stamp (event-id &optional timestamp)
  (update (where :id event-id)
          :timestamp (cond (timestamp timestamp)
                           (t (iso-from-universal (get-universal-time))))))

(defun clone-and-stamp-todo (event-id)
  "Creates a clone of the event with event-id and stamps the original"
  (record-event
   (make-event
    "todo"
    "me"
    (getf (first (select (where :id event-id))) :description)
    (getf (first (select (where :id event-id))) :tags)))
  (stamp event-id))
