(in-package :medard)

(use-package :utils)
(use-package :brightsky)

(defparameter current-offset 8 "The number of hours CST is from UTC")
(defparameter current-latitude 29.9 "The current latitude.")
(defparameter current-longitude -95.7 "The current longitude.")





(defun %wttr-current (latitude longitude system)
  (let ((response (dex:get (format nil "http://wttr.in/~A,~A?~A&format=3"
				   latitude
				   longitude
				   (if (equal system :metric) "m" "u")))))
    (string-trim '(#\Newline #\Return)
		 (subseq response (+ (position #\: response) 2)))))

(defun main ()
  "Get the current weather."
  (ignore-errors
   (format
    t (brightsky:weather
       current-latitude current-longitude current-offset))))
