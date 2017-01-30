;;;; secretary.lisp

(in-package #:secretary)

(load "comparators")

(defvar *events* nil)
(defvar *data* nil)

(defparameter *events-filename* "event-list.db")

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

(defun max-event-id ()
  (reduce
   #'max
   (map 'list #'(lambda (item)
                  (getf item :id))
        *events*)))

;; Event functions

(defun make-event (timestamp actors description tags depends-on)
  (list :id (+ 1 (max-event-id))
        :timestamp timestamp
        :actors actors
        :description description
        :tags tags
        :depends-on depends-on))

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
       (prompt-read "Tags")
       (prompt-read "Depends On"))))
    ((equal type :pill)
     (record-event
      (make-event
       (iso-from-universal (get-universal-time))
       "me"
       "Did some drugs"
       "pill"
       "")))
    ((equal type :trade)
     (record-event
      (make-event
       (prompt-read "Timestamp")
       "me"
       (prompt-read "Description")
       "trade"
       (prompt-read "Depends On"))))
    ((equal type :me)
     (record-event
      (make-event
       (iso-from-universal (get-universal-time))
       "me"
       (prompt-read "Description")
       (prompt-read "Tags")
       "")))
    ((equal type :time)
     (record-event
      (make-event
       (prompt-read "Timestamp")
       (prompt-read "Actors")
       (prompt-read "Description")
       (prompt-read "Tags")
       "")))
    (t
     (record-event
      (make-event
       (iso-from-universal (get-universal-time))
       (prompt-read "Actors")
       (prompt-read "Description")
       (prompt-read "Tags")
       "")))))

(defun add-todo ()
  (add-event :todo))

(defun save-data (var filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print var out)))
  (length var))

(defun save-events ()
  (save-data *events* *events-filename*))

(defun load-data (var filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf var (read in))))
  (length var))

(defun load-events ()
  (load-data *events*
             *events-filename*))

(defmacro where (&rest clauses)
  `#'(lambda (event) (and ,@(make-comparisons-list clauses))))

(defmacro matching (&rest clauses)
  `#'(lambda (event) (and ,@(make-match-list clauses))))

(defun select (selector-fn)
  (remove-if-not selector-fn *events*))

(defun update (selector-fn &key timestamp actors description tags depends-on)
  (setf *events*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if timestamp (setf (getf row :timestamp) timestamp))
               (if actors (setf (getf row :actors) actors))
               (if description (setf (getf row :description) description))
               (if tags (setf (getf row :tags) tags))
               (if depends-on (setf (getf row :depends-on) depends-on)))
             row)
         *events*))
  (length *events*))

(defun last-n-events (num-events)
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
            "[~3d] ~a (~a)~%"
            (getf event :id)
            (getf event :description)
            (getf event :tags))))

(defun stamp (event-id &optional timestamp)
  (update (where :id event-id)
          :timestamp (cond
                       (timestamp timestamp)
                       (t (iso-from-universal (get-universal-time))))))

(defun clone-and-stamp-todo (event-id)
  "Creates a clone of the event with event-id and stamps the original"
  (record-event
   (make-event
    "todo"
    "me"
    (getf (first (select (where :id event-id))) :description)
    (getf (first (select (where :id event-id))) :tags)
    ""))
  (stamp event-id))

(defparameter *hour-seconds* 3600)
(defparameter *day-seconds* 86400)
(defparameter *week-seconds* 604800)

(defun duration-from-seconds (seconds)
  (cond
    ((< seconds 1) "")
    ((< seconds 60) (format nil "~dS" seconds))
    ((< seconds 3600)
     (format nil "~dM~a"
             (floor (/ seconds 60))
             (duration-from-seconds
              (- seconds (* 60 (floor (/ seconds 60)))))))
    ((< seconds *day-seconds*)
     (format nil "~dH~a"
             (floor (/ seconds *hour-seconds*))
             (duration-from-seconds
              (- seconds
                 (* *hour-seconds* (floor (/ seconds *hour-seconds*)))))))
    ((< seconds (* *week-seconds*))
     (format nil "~dD~a"
             (floor (/ seconds *day-seconds*))
             (duration-from-seconds
              (- seconds
                 (* *day-seconds* (floor (/ seconds *day-seconds*)))))))
    (t (format nil "~dW~a"
               (floor (/ seconds *week-seconds*))
               (duration-from-seconds
                (- seconds
                   (* *week-seconds* (floor (/ seconds *week-seconds*)))))))))

(defun time-since (selector-fn)
  (duration-from-seconds
   (-
    (get-universal-time)
    (universal-from-iso
     (getf (first (select selector-fn)) :timestamp)))))

(defun time-until (selector-fn)
  (duration-from-seconds
   (seconds-until
    (first (select selector-fn)))))

(defun seconds-until (event)
  (-
   (universal-from-iso
    (getf event :timestamp))
   (get-universal-time)))

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
