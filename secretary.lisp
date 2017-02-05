;;;; secretary.lisp

(in-package #:secretary)

(defvar *events* nil)
(defvar *events-filename* "event-list.db")

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

(defun make-event (timestamp description tags references)
  (list :id (+ 1 (max-event-id))
        :timestamp timestamp
        :description description
        :tags tags
        :references references))

(defun record-event (event)
  (push event *events*) event)

(defun add-trade ()
     (record-event
      (make-event
       (iso-now)
       (prompt-read "Description")
       "trade"
       "")))

(defun add-todo ()
  (record-event
   (make-event
    "todo"
    (prompt-read "Description")
    (prompt-read "Tags")
    (prompt-read "Depends on"))))

(defun add-event (&optional type)
  (cond
    ((equal type :time)
     (record-event
      (make-event
       (prompt-read "Timestamp")
       (prompt-read "Description")
       (prompt-read "Tags")
       (prompt-read "References"))))
    (t
     (record-event
      (make-event
       (iso-now)
       (prompt-read "Description")
       (prompt-read "Tags")
       (prompt-read "References"))))))

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
         *events*)))

(defun delete-events (selector-fn)
  (setf *events*
        (remove-if selector-fn *events*)))

(defun last-n (&optional (num-events 1))
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

(defun groceries ()
  (remove-if-not
   #'(lambda (event)
       (and
        (string= (getf event :timestamp) "need")
        (string= (getf event :tags) "grocery")))
   *events*))

(defun grocery-list ()
  (let ((groc-vec (groceries)))
    (loop
       for item being the elements of groc-vec
       do (format t "* ~50a ~10@a~%"
                  (getf item :description)
                  (format nil "(#~d)" (getf item :id))))))

(defun future-events ()
  (remove-if
   #'(lambda (event)
       (or (string< (getf event :timestamp) (iso-now))
           (string= (getf event :timestamp) "todo")
           (string= (getf event :timestamp) "need")))
   *events*))
