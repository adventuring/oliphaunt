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


