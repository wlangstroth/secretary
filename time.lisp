;; Time functions

(in-package #:secretary)

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

(defun iso-now ()
  (iso-from-universal (get-universal-time)))

(defconstant +hour-seconds+ 3600)
(defconstant +day-seconds+ 86400)
(defconstant +week-seconds+ 604800)

(defun duration-from-seconds (seconds)
  (cond
    ((< seconds 1) "")
    ((< seconds 60) (format nil "~dS" seconds))
    ((< seconds 3600)
     (format nil "~dM~a"
             (floor (/ seconds 60))
             (duration-from-seconds
              (- seconds (* 60 (floor (/ seconds 60)))))))
    ((< seconds +day-seconds+)
     (format nil "~dH~a"
             (floor (/ seconds +hour-seconds+))
             (duration-from-seconds
              (- seconds
                 (* +hour-seconds+ (floor (/ seconds +hour-seconds+)))))))
    ((< seconds (* +week-seconds+))
     (format nil "~dD~a"
             (floor (/ seconds +day-seconds+))
             (duration-from-seconds
              (- seconds
                 (* +day-seconds+ (floor (/ seconds +day-seconds+)))))))
    (t (format nil "~dW~a"
               (floor (/ seconds +week-seconds+))
               (duration-from-seconds
                (- seconds
                   (* +week-seconds+ (floor (/ seconds +week-seconds+)))))))))

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
