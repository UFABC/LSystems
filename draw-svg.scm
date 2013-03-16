(define number->string/maybe
  (lambda (x)
    (if (number? x)
        (number->string x)
        x)))

(define make-attr-list
  (lambda args
    (map number->string/maybe args)))

(define make-svg-line
  (lambda (x1 y1 x2 y2 stroke fill stroke-width)
    (cons 'line
          (make-attr-list 'x1 x1
                          'y1 y1
                          'x2 x2
                          'y2 y2
                          'fill fill
                          'stroke stroke
                          'stroke-width stroke-width))))

(define list-of-points
  (lambda args
    (let next-point ((points (map number->string args))
                     (result ""))
      (if (null? points)
          result
          (next-point (cddr points)
                      (string-append result
                                     (car points)  ","
                                     (cadr points) " "))))))

(define make-svg-triangle
  (lambda (x1 y1
      x2 y2
      x3 y3
      stroke fill stroke-width)
    (cons 'polygon
          (make-attr-list 'fill fill
                          'stroke stroke
                          'stroke-width stroke-width
                          'points (list-of-points x1 y1 x2 y2 x3 y3)))))


(define svg-preamble
  "<?xml version=\"1.0\" standalone=\"no\"?>
<!DOCTYPE svg PUBLIC
         \"-//W3C//DTD SVG 1.1//EN\"
         \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">")

(define make-svg-image
  (lambda (objects)
    (append (list 'svg
                  'version 1.1
                  'xmlns "http://www.w3.org/2000/svg")
            objects)))


(define images->xml
  (lambda (img file)
    (with-output-to-file file
      (lambda ()
        (display svg-preamble)
        (newline)
        (xml-write-tag img)))))



;;;;;;

(define triangle?
  (lambda (obj)
    (eq? (car obj) 'triangle)))

(define make-triangle/sides
  (lambda (a b c)
    (list 'triangle a b c)))

(define triangle-side
  (lambda (tri idx)
    (list-ref tri (+ idx 1))))

(define square
  (lambda (x)
    (* x x)))

(define triangle-compute-angle
  (lambda (a b c)
    (let ((numerator (+ (square b)
                        (square c)
                        (- (square a))))
          (denominator (* 2 b c)))
      (acos (/ numerator denominator)))))

(define triangle->svg
  (lambda (tri x y stroke fill stroke-width)
    (let ((a (triangle-side tri 0))
          (b (triangle-side tri 1))
          (c (triangle-side tri 2)))
      (let ((angle (triangle-compute-angle b a c)))
        (let ((x2 (+ x a))
              (y2 y)
              (x3 (* (cos angle) b))
              (y3 (* (sin angle) b)))
          (make-svg-triangle x  y
                             x2 y2
                             x3 y3
                             stroke fill stroke-width))))))

(define make-triangle/angles
  (lambda (alpha beta gamma)
    ...))

(define triangle-angle
  (lambda (tri idx)
    ...))