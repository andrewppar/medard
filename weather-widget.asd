(defsystem "weather-widget"
    :description "see what the weather is"
    :version "0.0.1"
    :author "Andrew Parisi <andrew.p.parisi@gmail.com>"
    :licence "Public Domain"
    :components ((:file "./src/package")
		 (:file "./src/weather-widget"))
    :build-operation "program-op"
    :build-pathname "./weather-widget"
    :entry-point "weather-widget:main"
    )
