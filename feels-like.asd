(defsystem "feels-like"
    :description "see what the weather is"
    :version "0.0.1"
    :author "Andrew Parisi <andrew.p.parisi@gmail.com>"
    :licence "Public Domain"
    :components ((:file "./src/package")
		 (:file "./src/feels-like"))
    :build-operation "program-op"
    :build-pathname "./feels-like"
    :entry-point "feels-like:main"
    )
