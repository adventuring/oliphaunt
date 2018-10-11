(in-package :oliphaunt)

;;; Time handling.

(defun days-ago (days)
  "Return a time that is DAYS days in the past."
  (local-time:timestamp- (local-time:now) days :day))

(defun yesterday ()
  "Get the same time as now, but yesterday.

See: `DAYS-AGO'"
  (days-ago 1))

(defun 2-days-ago ()
  "Two days ago.

See: `DAYS-AGO'"
  (days-ago 2))

(defun 3-days-ago ()
  "Three days ago.

See: `DAYS-AGO'"
  (days-ago 3))

(defun header-time (&optional (time (get-universal-time)))
  "Format TIME (or now) as an RFC-1123 timestring for HTTP headers.

Accepts either a LOCAL-TIME:TIMESTAMP or NUMBER of Universal Time."
  (local-time:format-rfc1123-timestring
   nil
   (etypecase time
     (number (local-time:universal-to-timestamp time))
     (local-time:timestamp time))))

(defun year<-universal-time (time)
  (nth-value 5 (decode-universal-time time)))

(defun file-write-year (file)
  (or (year<-universal-time (file-write-date file))
      0))



(defun translate-american-ish-date (created-at)
  (register-groups-bind ((#'parse-integer month)
                         (#'parse-integer day)
                         (#'parse-integer year)
                         (#'parse-integer hour)
                         (#'parse-integer minute)
                         (#'parse-integer second))
      ("(\\d\\d)/(\\d\\d)/(201\\d) ([012]\\d):([0-5]\\d):([0-5]\\d)"
       created-at :sharedp t)
    (check-type year (integer 2015 2018))
    (check-type month (integer 1 12))
    (check-type day (integer 1 31))
    (assert (<= day (case month
                      ((4 6 9 11) 30)
                      (2 (case year
                           (2016 29)
                           (otherwise 28)))
                      ((1 3 5 7 8 10 12) 31))))
    (check-type hour (integer 00 (24)))
    (check-type minute (integer 0 (60)))
    (check-type second (integer 0 (60)))
    (encode-timestamp 0 second minute hour day month year)))
