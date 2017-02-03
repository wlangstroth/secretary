;;;; secretary.lisp

(in-package #:secretary)

(defvar *events* nil)

(defun data-filename (type)
  (format nil "data/~a.db" type))

(defun load-data (vec filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf vec (read in)))))

(defun save-data (var filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print var out)))
  (length var))

;; Event functions

(defun max-event-id ()
  (reduce
   #'max
   (map 'list #'(lambda (item)
                  (getf item :id))
        *events*)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun make-event (timestamp description tags)
  (list :id (+ 1 (max-event-id))
        :timestamp timestamp
        :description description
        :tags tags))

(defun record-event (event)
  (push event *events*) event)

(defun add-event (&optional type)
  (cond
    ((equal type :todo)
     (record-event
      (make-event
       "todo"
       (prompt-read "Description")
       (prompt-read "Tags"))))
    ((equal type :trade)
     (record-event
      (make-event
       (iso-now)
       (prompt-read "Description")
       "trade")))
    ((equal type :time)
     (record-event
      (make-event
       (prompt-read "Timestamp")
       (prompt-read "Description")
       (prompt-read "Tags"))))
    (t
     (record-event
      (make-event
       (iso-now)
       (prompt-read "Description")
       (prompt-read "Tags")))))
  (save-events))

(defun add-todo ()
  (add-event :todo))

(defun save-events ()
  (with-open-file (out *events-filename*
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *events* out)))
  (length *events*))

(defun load-events ()
  (with-open-file (in *events-filename*)
    (with-standard-io-syntax
      (setf *events* (read in))))
  (length *events*))

(defmacro where (&rest clauses)
  `#'(lambda (event) (and ,@(make-comparisons-list clauses))))

(defmacro matching (&rest clauses)
  `#'(lambda (event) (and ,@(make-match-list clauses))))

(defun select (selector-fn)
  (remove-if-not selector-fn *events*))

(defun update (selector-fn &key timestamp description tags)
  (setf *events*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if timestamp (setf (getf row :timestamp) timestamp))
               (if description (setf (getf row :description) description))
               (if tags (setf (getf row :tags) tags)))
             row)
         *events*))
  (save-events)
  (length *events*))

(defun delete-events (selector-fn)
  (setf *events*
        (remove-if selector-fn *events*))
  (save-events))

(defun last-n (num-events)
  (subseq *events* 0 num-events))

(defun compare-ids (first-event second-event)
  (< (getf first-event :id)
     (getf second-event :id)))

(defun show-todos ()
  "Returns a formatted list of todos sorted by ID"
  (dolist
      (event
        (sort
         (select (where :timestamp "todo")) #'compare-ids))
    (format t
            "[~4d] ~a (~a)~%"
            (getf event :id)
            (getf event :description)
            (getf event :tags))))

(defun stamp (event-id &optional timestamp)
  (update (where :id event-id)
          :timestamp (cond
                       (timestamp timestamp)
                       (t (iso-now)))))

(defun clone-and-stamp-todo (event-id)
  "Creates a clone of the event with event-id and stamps the original"
  (record-event
   (make-event
    "todo"
    (getf (first (select (where :id event-id))) :description)
    (getf (first (select (where :id event-id))) :tags)))
  (stamp event-id))

(defun non-todos ()
  (remove-if
     #'(lambda (event)
         (equal (getf event :timestamp) "todo"))
     *events*))

(defun future-events ()
  (remove-if-not
   #'(lambda (event)
       (> (seconds-until event) 0))
   (non-todos)))
