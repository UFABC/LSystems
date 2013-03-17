;Mapeamento da linguagem
(define I  ;Desenha um traço pra frente
  (lambda (angulo)
    (make-svg-line 50 100 ;x e y inicial 
                   (* (cos angulo) 100); x final * angulo
                   (* (sin angulo) 100); y inicial * angulo
                   "blue" ;cor do preenchimento
                   "green" ; contorno
                   5)))

(define lsystems
  (lambda (axioma ) ; falta fazer um parse de axioma pra uma lista.
    (make-svg-image (list (I 1.23) (I 0.3432)))))

(define imagem (lsystems '(I 1.32)))

(xml-write-tag imagem)

(images->xml imagem "lsystems.svg")
