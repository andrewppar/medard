(in-package :utils)

(defun clj/get (alist key &optional default)
  "Retrieve the value at `key` in `alist`."
  (or (cdr (assoc key alist :test #'equal)) default))

(defun clj/select-keys (alist keys)
  "Create a new alist with only `keys`."
  (let ((result '()))
    (dolist (binding alist)
      (when (member (car binding) keys :test #'equal)
	(push binding result)))
    result))

(defun clj/assoc (alist key value)
  "Replace value in `alist` at `key` with new `value`."
  (mapcar
   (lambda (binding)
     (if (equal (car binding) key)
	 (cons key value)
	 binding))
   alist))

(defun clj/update (alist key function)
  "Update value at `key` in `alist` by applying function to it."
  (clj/assoc alist key (funcall function (clj/get alist key))))

(defun celsius->farenheit (celsius)
  "Convert celsius degrees to farenheit."
  (floor (+ (* celsius 1.8) 32)))

(defun farenheit->celsius (farenheit)
  "Convert farenheit to celsius."
  (floor (/ (- farenheit 32) 1.8)))

(defun kph->mph (kph)
  "Convert kilometers per hour to miles per hour."
  (* kph 0.621371))

(defun mph->kph (mph)
  "Convert miles per hour to kilometers per hour."
  (/ mph 0.621371))
