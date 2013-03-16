(number->string/maybe 10)

(make-attr-list 'x 1 'y 2 'z 3)

(define line (make-svg-line 2 3 10 20 "blue" "red" 5))

(xml-write-tag line)

(list-of-points 23 25 10 20 50 60)

(make-svg-triangle 10 20 100 200 1000 2000
                   "red" "blue" 4)

(define image
  (make-svg-image (list (make-svg-triangle 0   0
                                           200 200
                                           0   200
                                           "blue"
                                           "rgb(255,0,0)"
                                           3)
                        (make-svg-line 0   100
                                       100 100
                                       "green"
                                       "green"
                                       5))))

(xml-write-tag image)

(images->xml image "whoo.svg")

(define make-sierpinski
  (lambda (x1 y1
      x2 y2
      x3 y3 fill-out fill-in  n)
    (define make-sierpinski-aux
      (lambda (x1 y1
          x2 y2
          x3 y3 fill n acc)
        (if (positive? n)
            (let ((x12 (/ (+ x1 x2) 2.0))
                  (y12 (/ (+ y1 y2) 2.0))
                  (x13 (/ (+ x1 x3) 2.0))
                  (y13 (/ (+ y1 y3) 2.0))
                  (x23 (/ (+ x2 x3) 2.0))
                  (y23 (/ (+ y2 y3) 2.0)))
              (append acc (list (make-svg-triangle x12 y12 x23 y23 x13 y13 fill fill 0))
                      (make-sierpinski-aux x1 y1 x12 y12 x13 y13 fill (- n 1) '())
                      (make-sierpinski-aux x2 y2 x12 y12 x23 y23 fill (- n 1) '())
                      (make-sierpinski-aux x3 y3 x13 y13 x23 y23 fill (- n 1) '())
                      ))
            acc)))
      (make-svg-image
       (append (list (make-svg-triangle x1 y1 x2 y2 x3 y3 fill-out fill-out 0))
               (make-sierpinski-aux x1 y1 x2 y2 x3 y3 fill-in n '())))))


(begin
  (let ((s (make-sierpinski 0 600 300 0 600 600
                            "black" "white"
                            10)))
    (images->xml s "sierpinski.svg")))


(define image
  (let ((tri (make-triangle/sides 200 282.842 200)))
    (let ((svg-tri (triangle->svg tri 0 0 "black" "blue" 4)))
      (make-svg-image (list svg-tri)))))

(xml-write-tag image)

