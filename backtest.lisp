(in-package #:secretary)

(defparameter *csv-data* nil
  "Where the data goes")

(defun without-commas (string-row)
  (remove-if
   #'(lambda (c) (string= "," c))
   string-row))

(defun row-from-string (row-string)
  (with-input-from-string (in row-string)
    (loop for x = (read in nil nil)
       while x collect x)))

(defun interleave (names values)
  (cond
    ((or (eql names '())
         (eql values '()))
     '())
     (t
      (append
       (list (first names) (first values))
       (interleave (rest names) (rest values))))))


;; Split string on first comma, separating the date from the rest

(defun values-from-row (row-string)
  (let
      ((split-index (position #\, row-string)))
    (append (list (subseq row-string 0 split-index))
            (row-from-string
             (without-commas
                 (subseq row-string split-index))))))

(defparameter *column-symbols*
  '(:timestamp
    :open-ask :open-bid
    :high-ask :high-bid
    :low-ask :low-bid
    :close-ask :close-bid))

(defun candles-from-file (filename)
  (with-open-file (in filename)
    (do ((line (read-line in nil)
               (read-line in nil)))
         ((null line))
         (print (row-from-string (take-commas line))))))

;; Strategies to Test

;; (defun three-day-majority (ohlc-list)
;;   ())

;; (defun backtest (test-fn)
;;   (format nil "hi"))
