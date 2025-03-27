(defpackage utils
  (:use :cl)
  (:export #:clj/get #:clj/select-keys #:clj/assoc #:clj/update
	   #:kph->mph #:celsius->farenheit))

(defpackage brightsky
  (:use :cl)
  (:export #:weather))

(defpackage medard
  (:use :cl)
  (:export #:main))
