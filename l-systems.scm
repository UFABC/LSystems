;Mapeamento da linguagem
(define F  ;Desenha um traço pra frente
             (make-svg-line 50   100
                                       100 100
                                       "blue"
                                       "green"
                                       5)
            )

(define lsystems
  (lambda (axioma )
    (print axioma)
    (print (string? axioma))
    (print (symbol? axioma))
    (print (list? axioma))
    (print (list? (cons axioma axioma)))
    (print (cons axioma axioma))
    (print (cdr(list axioma)))
    (make-svg-image (list axioma))))
    (print (list axioma))

(define imagem (lsystems 'F))

(xml-write-tag imagem)

(images->xml imagem "lsystems.svg")
