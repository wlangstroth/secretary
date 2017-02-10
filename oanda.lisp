(in-package #:secretary)

(load "secrets")

;; Either you do the following, or you have to (flexi-streams:octets-to-string)
;; anything that comes from (drakma:http-request)
(setq drakma:*text-content-types*
      (cons '("application" . "json")
            drakma:*text-content-types*))

(defvar *base-url* "https://api-fxtrade.oanda.com/v1/")
(defvar *account-url*
  (concatenate 'string *base-url* "accounts/" *default-account*))
(defvar *trades-url*
  (concatenate 'string *account-url* "/trades"))

;; *bearer* is in the secrets file
(defun auth-bearer ()
  (format nil "Bearer ~a" *bearer*))

(defun price-url-of (instrument)
  (concatenate 'string *base-url* "prices?instruments=" instrument))

(defun oanda-request (url)
  (drakma:http-request
   url
   :additional-headers `(("Authorization" . ,(auth-bearer)))))

(defun price-of (instrument &optional (bid-or-ask "bid"))
  "SBCL complains unless bid-or-ask is coerced"
  (st-json:getjso* (coerce bid-or-ask 'string)
                   (first (prices instrument))))

(defun prices (instrument)
  (st-json:getjso
   "prices"
   (st-json:read-json
    (oanda-request
     (price-url-of instrument)))))

(defun trades ()
  (st-json:getjso
   "trades"
   (st-json:read-json
    (oanda-request *trades-url*))))

(defun show-trade (trade-obj)
  (let*
      ((name (st-json:getjso* "instrument" trade-obj))
       (units (st-json:getjso* "units" trade-obj))
       (trade-price (st-json:getjso* "price" trade-obj))
       (side (st-json:getjso* "side" trade-obj))
       (trade-time (subseq (st-json:getjso* "time" trade-obj) 0 16))
       (price
        (cond
          ((string= side "sell") (price-of name "ask"))
          (t (price-of name "bid"))))
       (adjustment-factor
        (cond
          ((string= (subseq name 0 3) "USD") price)
          (t 1)))
       (side-factor
        (cond
          ((string= side "sell") -1)
          (t 1)))
       (price-diff (/ (* side-factor
                         (- (* price units)
                            (* trade-price units)))
                      adjustment-factor))
       (bought-sold
        (cond ((string= side "sell") "↓")
              (t "↑")))
       (pl-string (format nil "~$" price-diff)))
    (format t "~%~a ~a ~5@a ~10a @ ~7@a | P/L: ~7@a ~%"
            trade-time
            bought-sold
            units name
            trade-price
            pl-string)))

(defun show-trades ()
  (format t "~a~%" (iso-now))
  (loop for trade in (trades)
     do (show-trade trade)))

(defun account ()
  (st-json:read-json
   (oanda-request *account-url*)))

(defun balance ()
  (st-json:getjso* "balance" (account)))

(defparameter *position-factor* 0.025)

(defun position-size (&optional (size *position-factor*))
  (* (balance) size))

(defun profit-loss ()
  (format t "~%P/L: ~a"
          (st-json:getjso*
           "unrealizedPl"
           (account))))

(defun trade-status ()
  (show-trades)
  (profit-loss))

(defun instrument-url (instrument field)
  "*default-account* is in the secrets file"
  (concatenate 'string
               *base-url*
               "instruments?"
               "accountId="
               *default-account*
               "&instruments="
               instrument
               "&fields="
               field
               ))

(defun instrument-data (&optional (instrument "EUR_USD") (field "marginRate"))
  (st-json:read-json
   (oanda-request (instrument-url instrument field))))

(defun midpoint (high low)
  (/ (+ high low) 2))

; For *_USD, side * (position / (current_price - stop))
; USD_*, above / current_price
(defun units-for-trade (instrument side stop)
  (let*
      ((side-factor
        (cond ((string= side "sell") -1)
              (t 1)))
       (price
        (cond
          ((string= side "sell") (price-of instrument "ask"))
          ((string= side "buy") (price-of instrument "bid")))))
    (format t "~a" (* side-factor (/ (position-size) (- price stop))))))

(defun make-trade (instrument side stop)
  (units-for-trade (instrument side stop)))

(defun deal-or-no-deal (instrument)
  ;; get candles
  ;; Work backwards from present candle, find extremes until five are found
  ;; Use the closest three to form a first hypothesis of trend
  ;; Correct for new wide-swinging extremes
  ;; Find midpont between last two extremes
  ;; If movement is in the same direction as the trend, and reaches the midpoint
  ;; then deal
  )
