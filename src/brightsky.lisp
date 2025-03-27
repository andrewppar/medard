(in-package :brightsky)
(use-package :utils)

(defun time->brightsky (time)
  "Convert a local-time `time` into a brightsky acceptable format."
  (local-time:format-timestring
   nil time :format local-time:+rfc3339-format/date-only+))

(defun brightsky->time (timestring offset-hours)
  "Parse a brightsky timestring to localtime."
  (local-time:parse-timestring timestring :offset (* 60 60 offset-hours)))

(defun date (timestring offset)
  "Convert `timestring` into a brightsky acceptable format."
  (let ((time (if timestring
		  (local-time:parse-timestring
		   timestring :offset (- (* 60 60 offset)))
		  (local-time:now))))
    (time->brightsky (local-time:timestamp+ time offset :hour))))

(defun starts-with-p (string prefix)
  "Check if STRING starts with PREFIX."
  (let ((prefix-length (length prefix)))
    (and (>= (length string) prefix-length)
         (string= (subseq string 0 prefix-length) prefix))))

(defun ends-with-p (string postfix)
  (starts-with-p (reverse string) (reverse postfix)))

(defun ->icon (icon)
  "Replace brightsky icon description with a nerd font icon."
  (let ((night? (ends-with-p icon "night")))
    (cond ((starts-with-p icon "clear")
	   (if night? "" ""))
	  ((starts-with-p icon "partly-cloudy")
	   (if night? "" ""))
	  ((starts-with-p icon "cloudy") "󰅟")
	  ((starts-with-p icon "fog") "")
	  ((starts-with-p icon "sleet") "")
	  ((starts-with-p icon "snow") "󰼶")
	  ((starts-with-p icon "hail") "󰖒")
	  ((starts-with-p icon "thuderstorm") "")
	  ((starts-with-p icon "wind") "󰖝")
	  ((starts-with-p icon "rain") "")
	  (t icon))))

(defun ->local (weather-block system offset)
  "Convert a brightsky response to local values."
  (let ((base (utils:clj/update
	       (utils:clj/update
		weather-block
		:timestamp (lambda (timestring)
			     (brightsky->time timestring offset)))
	       :icon #'->icon)))
    (cond ((eq system :metric) base)
	  ((eq system :imperial)
	   (utils:clj/update
	    (utils:clj/update base :temperature #'utils:celsius->farenheit)
	    :wind--speed #'utils:kph->mph))
	  (t base))))

(defun weather-block< (block-one block-two)
  "Check if `block-one` occurred earlier than `block-two`."
  (let ((time-one (utils:clj/get block-one :timestamp))
	(time-two (utils:clj/get block-two :timestamp)))
    (local-time:timestamp< time-one time-two)))

(defun fetch (lat long offset for-date system)
  "Get brightsky data for `lat` and `long` at `date`.
Return results parsed into `system`."
  (let* ((datestring (date for-date offset))
	 (params (format nil "lat=~A&lon=~A&date=~A" lat long datestring))
	 (url (format nil "https://api.brightsky.dev/weather?~A" params))
	 (response (dex:get url)))
    (mapcar
     (lambda (weather-block)
       (->local weather-block system offset))
     (utils:clj/get (cl-json:decode-json-from-string response) :weather))))

(defun current-hour ()
  "The closest hour to now."
  (let ((now (local-time:now)))
    (if (> (local-time:timestamp-minute now) 30)
	(+ 1 (local-time:timestamp-hour now))
	(local-time:timestamp-hour now))))

(defun current-day ()
  "The current day."
  (local-time:timestamp-day (local-time:now)))

(defun current-block (weather-blocks)
  "Get the block for the current day and hour."
  (let ((hour (current-hour))
	(day (current-day)))
    (some
     (lambda (weather-block)
       (let ((block-time (utils:clj/get weather-block :timestamp)))
	 (when (and (= hour (local-time:timestamp-hour block-time))
		    (= day (local-time:timestamp-day block-time)))
	   weather-block)))
     weather-blocks)))

(defun block-distance (block-one block-two)
  (let* ((time-one (utils:clj/get block-one :timestamp))
	 (day-one (* (local-time:timestamp-day time-one) 24))
	 (hour-one (local-time:timestamp-hour time-one))
	 (time-two (utils:clj/get block-two :timestamp))
	 (day-two (* (local-time:timestamp-day time-two) 24))
	 (hour-two (local-time:timestamp-hour time-two)))
    (- (+ day-two hour-two) (+ day-one hour-one))))

(defun closest-block (weather-blocks)
  (let* ((current (list (cons :timestamp (local-time:now))))
	 (test-pred (lambda (other) (weather-block< current other)))
	 (just-above (car (sort
			   (remove-if-not test-pred weather-blocks)
			   #'weather-block<)))
	 (just-below (car (last (sort
				 (remove-if test-pred weather-blocks)
				 #'weather-block<)))))
    (cond ((and just-above just-below)
	   (let  ((above-distance (block-distance current just-above))
		  (below-distance (block-distance just-below current)))
	     (if (< above-distance below-distance) just-above just-below)))
	  (just-above just-above)
	  (just-below just-below))))

(defun block-summary (weather-block system &optional parens?)
  (let ((temperature (utils:clj/get weather-block :temperature))
	(icon (utils:clj/get weather-block :icon))
	(unit (if (equal system :imperial) "F" "C")))
    (if parens?
	(format nil "(~A ~A°~A)" icon temperature unit)
	(format nil "[~A ~A°~A]" icon temperature unit))))

(defun max-temperature (weather-blocks)
  (car (sort weather-blocks (lambda (block-one block-two)
			      (> (utils:clj/get block-one :temperature)
				 (utils:clj/get block-two :temperature))))))

(defun timeline(weather-blocks system)
  (let* ((block-timeline (sort weather-blocks #'weather-block<))
	 (result (list (block-summary (car (last block-timeline)) system)))
	 (current-block (closest-block weather-blocks))
	 (current-time (utils:clj/get current-block :timestamp))
	 (idx 2))
    (dolist (weather-block (butlast (cdr block-timeline)))
      (cond ((equal (utils:clj/get weather-block :timestamp) current-time)
	     (push (block-summary weather-block system t) result)
	     (setq idx 1))
	    ((= (mod idx 4) 0)
	     (push (utils:clj/get weather-block :icon) result))
	    (t (push " " result)))
      (setq idx (+ idx 1)))
    (push (block-summary (car block-timeline) system) result)
    (format nil "~{~A~}" result)))

(defun current (weather-blocks system)
  (let* ((unit (if (equal system :imperial) "F" "M"))
	 (current (closest-block weather-blocks))
	 (current-temp (utils:clj/get current :temperature))
	 (current-icon (utils:clj/get current :icon))
	 (above (sort
		 (remove-if-not
		  (lambda (other)
		    (weather-block< current other))
		  weather-blocks)
		 #'weather-block<))
	 (next (if (equal current (car above)) (cadr above) (car above)))
	 (next-temp (utils:clj/get next :temperature))
	 (next-icon (utils:clj/get next :icon))
	 (direction (cond
		      ((> next-temp current-temp)
		       (format nil "~c[31m~:*~c[m" (code-char 27)))
		      ((= next-temp current-temp) "")
		      (t
		       (format nil "~c[34m~:*~c[m" (code-char 27))))))
    (format nil "~A ~A°~A~A ~A"
	    current-icon current-temp unit direction next-icon)))

(defun weather (lat long offset &optional date (system :imperial))
  "Display the current temperature and conditions with progress
indications."
  (current (fetch lat long offset date system) system))
