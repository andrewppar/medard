(require 'asdf)
(asdf:load-asd (pathname (truename "weather-widget.asd")))
(asdf:operate 'asdf:build-op 'weather-widget)
