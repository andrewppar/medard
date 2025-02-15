(in-package :weather-widget)

(defun string-trim-whitespace (string-to-trim)
  (string-trim
   '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)
   string-to-trim))

(defparameter ansiweather
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (list "which" "ansiweather") :output :string)
    (if error-output
	exit-code
	(string-trim-whitespace output))))

(defun parse-ansiweather-output (output)
  (let* ((output-alist (mapcar
			(lambda (arg)
			  (destructuring-bind (raw-key raw-value)
			      (uiop:split-string arg :separator '(#\:))
			    (let ((key (string-downcase (string-trim-whitespace raw-key)))
				  (value (string-trim-whitespace raw-value)))
			      (cond ((= 0 (or (search "weather in" key) 1))
				     (cons :temp value))
				    ((= 0 (or (search "feels like" key) 1))
				     (cons :feels-like value))
				    ((= 0 (or (search "wind" key) 1))
				     (cons :wind value))
				    ((= 0 (or (search "humidity" key) 1))
				     (cons :humidity value))
				    (t nil)))))
			(uiop:split-string (string-trim-whitespace output) :separator '(#\-))))
	 (temp (car (uiop:split-string (cdr (assoc :temp  output-alist)))))
	 (feels-like (car (uiop:split-string (cdr (assoc :feels-like output-alist))))))
    (format nil "~A(~A)°F" temp feels-like)))

(defun weather (location)

;; add cache here so we don't just keep fetching...
  (multiple-value-bind (result error-output exit-code)
      (uiop:run-program (list ansiweather
			      "-l" location
			      "-u" "imperial"
			      "-a" "false"
			      "-h" "true"
			      "-w" "true"
			      "-H" "true"
			      "-p" "false"
			      "-i" "false")
			:output :string)
    (if error-output
	exit-code
	(parse-ansiweather-output result))))

(defun main () ()
  (format t (weather (or (uiop:getenv "LOCATION") "Houston"))))
