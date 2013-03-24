(declare (uses tcp))
(define-values (i o) (tcp-connect "localhost" 4242))
(read-line i)
(write-line "Good Bye!" o)
(print (read-line i))

