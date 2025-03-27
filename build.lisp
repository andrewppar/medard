(load (sb-ext:posix-getenv "ASDF"))
(asdf:load-asd (pathname (truename "medard.asd")))
(asdf:load-system 'medard)
(sb-ext:save-lisp-and-die
 #p"medard"
 :compression t
 :purify t
 :toplevel #'medard::main
 :executable t)
