(in-package #:secretary)

(defun without-commas (string-row)
  "Take the commas out of a string"
  (remove-if
   #'(lambda (c) (string= "," c))
   string-row))

(defun prices-from-string (price-string)
  (with-input-from-string (in price-string)
    (loop for x = (read in nil nil)
       while x collect x)))

(defun interleave (names values)
  "
Mix two lists together: one with symbols, and the other with values,
and get back a plist.
"
  (cond
    ((or (eql names '())
         (eql values '()))
     '())
     (t
      (append
       (list (first names) (first values))
       (interleave (rest names) (rest values))))))

(defun values-from-row (row-string)
  (let
      ((split-index (position #\, row-string)))
    (append (list (subseq row-string 0 split-index))
            (prices-from-string
             (without-commas
                 (subseq row-string split-index))))))

(defparameter *column-symbols*
  '(:timestamp
    :open-ask :open-bid
    :high-ask :high-bid
    :low-ask :low-bid
    :close-ask :close-bid))

(defparameter *candle-data* nil
  "Where the data goes")

(defun candles-from-file (filename)
  (setf *candle-data* nil)
  (with-open-file (in filename)
    (do ((line (read-line in nil)
               (read-line in nil)))
         ((null line))
      (setf *candle-data*
            (append (list (interleave
                           *column-symbols*
                           (values-from-row line)))
                    *candle-data*)))))

;; Strategies to Test

;; Three day majority for NATGAS:
;; Start with t - 3 days. Determine the direction of that day. If the next two
;; days have new maxima in the same direction, that's the easy case: either
;; continue or start a new trade in that direction.
;; If the middle day is down range from the first, but the second is up range
;; from the first, same as above.
;; If the two end days are down range, exit an existing trade.
;;
;; (defun three-day-majority (ohlc-list)
;;   ())

;; (defun backtest (test-fn)
;;   (format nil "hi"))
