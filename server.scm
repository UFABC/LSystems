(declare (uses tcp))

(load "draw-svg.scm")
(load "l-systems.scm")

(define l (tcp-listen 4242))
(define-values (i o) (tcp-accept l))
(write-line "Hello!" o)
(print (read-line i))
(close-input-port i)
(close-output-port o)