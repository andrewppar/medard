(require 'asdf)
(asdf:load-asd (pathname (truename "feels-like.asd")))
(asdf:operate 'asdf:build-op 'feels-like)
