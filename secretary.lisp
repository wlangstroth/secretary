;;;; secretary.lisp

(in-package #:secretary)

(defvar *events* nil)
(defvar *events-filename* "event-list.db")

;; Event functions

(defun max-event-id ()
  (reduce
   #'max
   (map 'list
        #'(lambda (item)
            (getf item :id))
        *events*)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun make-event (timestamp description tags references expires)
  (list :id (+ 1 (max-event-id))
        :timestamp timestamp
        :description description
        :tags tags
        :references references
        :expires expires))

(defun record-event (event)
  (push event *events*) event)

(defun add-trade ()
  (let ((timestamp (prompt-read "Time (default now)")))
    (record-event
     (make-event
      (cond  ((= 0 (length timestamp)) (iso-now))
             (t timestamp))
      (prompt-read "Description")
      "trade"
      ""
      ""))))

(defun add-todo ()
  (record-event
   (make-event
    "todo"
    (prompt-read "Description")
    (prompt-read "Tags")
    (prompt-read "Depends on")
    (prompt-read "Deadline"))))

(defun add-event ()
  (let ((timestamp (prompt-read "Time (default now)")))
    (record-event
     (make-event
      (cond  ((= 0 (length timestamp)) (iso-now))
             (t timestamp))
      (prompt-read "Description")
      (prompt-read "Tags")
      (prompt-read "References")
      (prompt-read "Expires")))))

(defun add-note ()
  (record-event
   (make-event
    (iso-now)
    (prompt-read "Description")
    (prompt-read "Tags")
    ""
    "")))

(defun add-need ()
  (record-event
   (make-event
    "need"
    (prompt-read "Description")
    (prompt-read "Tags")
    ""
    "")))

(defun add-grocery ()
  (record-event
   (make-event
    "need"
    (prompt-read "Item")
    "grocery"
    ""
    "")))

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

(defun update (selector-fn &key timestamp description tags references expires)
  (setf *events*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if timestamp (setf (getf row :timestamp) timestamp))
               (if description (setf (getf row :description) description))
               (if tags (setf (getf row :tags) tags))
               (if references (setf (getf row :references) references))
               (if expires (setf (getf row :expires) expires)))
             row)
         *events*))
  (select selector-fn))

(defun delete-events (selector-fn)
  (setf *events*
        (remove-if selector-fn *events*))
  (length *events*))

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
            "[~4d] ~a~%"
            (getf event :id)
            (getf event :description))))

(defun stamp (event-id &optional timestamp)
  (update (where :id event-id)
          :timestamp (cond
                       (timestamp timestamp)
                       (t (iso-now))))
  (select (where :id event-id)))

(defun groceries ()
  (remove-if-not
   #'(lambda (event)
       (and
        (string= (getf event :timestamp) "need")
        (string= (getf event :tags) "grocery")))
   *events*))

(defun grocery-list ()
  (loop
     for item in (groceries)
     do (format t "* ~50a ~10@a~%"
                (getf item :description)
                (format nil "(#~d)" (getf item :id)))))



(defun grocery-message ()
  (let (groc-vec)))

(defun future-events ()
  (remove-if
   #'(lambda (event)
       (or (string< (getf event :timestamp) (iso-now))
           (string= (getf event :timestamp) "todo")
           (string= (getf event :timestamp) "need")))
   *events*))
