(define set-xy!
  (lambda (ponto)
    (set! (vector-ref pilha 0) (car ponto))
    (set! (vector-ref pilha 1) (cdr ponto))))

(define altera-angulo!
  (lambda (incremento)
    (set! (vector-ref pilha 2)  (+ angulo incremento))))

(define empilha
  (lambda (estado-antigo)
    (let ((estado-atual (vector x y angulo)))
    (append pilha '(estado-antigo) '(estado-atual)))))

(define-syntax desempilha
  (lambda ()
    (let desempilha-aux ((pilha pilha) (pilha-desempilhada '()))
      (if (eq? length(pilha) 1)
          (set! pilha pilha-desempilhada)
          (desempilha-aux (cdr pilha) (cons (car pilha) pilha-desempilhada))))))

(define F ;Desenha um traço
  (lambda (xf yf)
    (make-svg-line x y xf yf 
                   "blue" ;cor do preenchimento
                   "green" ; contorno
                   5)))

(define calcula-final-de-reta ; calcula xf e yf com base e x, y e angulo na pilha
  (lambda ()
    (let ((incX (* (cos angulo) d))
          (incY (* (sin angulo) d)))
      (cons (+ x incX) (+ y incY)))))


(define desenha
  (lambda (frase ang-dir ang-esq)
    (case (car frase)
      (\#F (let ((ponto-final (calcula-final-de-reta)))
              (F (car ponto-final) (cdr ponto-final))
              (set-xy! ponto-final)))
      (\#[ (empilha estado-atual))
      (\#] (desempilha))
      (\#- (altera-angulo! ang-esq))
      (\#+ (altera-angulo! ang-dir)))
      (desenha (cdr frase) ang-dir ang-esq)))    

(define derivar-variavel
  (lambda (variavel)
    (let (derivacao (assoc variavel regras))
      (if derivacao
          '(derivacao)
          '(variavel)))))

(define derivar-frase
  (lambda (frase)
    (let (variavel (car frase)) (frase (cdr frase)) (nova-frase '()))
        (if (eq? (cdr frase) '())
            nova-frase
            (let ((derivacao (derivar-variavel variavel)))
              (derivar-frase (car frase) (cdr frase) (append derivacao nova-frase))))))

(define iterar-l-system
  (lambda (n frase)
      (if (zero? n)
          (frase)
          (iterar-l-system (- n 1) (derivar-frase frase)))))

(define iniciar-l-system
  (lambda (args)
    (let* ((iteracoes (assoc it args))
           (nome-imagem (assoc nome args))
           (angulo-direito (assoc dir args))
           (angulo-esquerdo (assoc esq args))
           (nome (if nome-imagem
                     nome-imagem
                     "lsystem.svg"))
           (frase-feita (assoc frase args))
           (frase (if frase-feita
                      frase-feita
                      (iterar-l-system iteracoes axioma)))
           (imagem (desenha frase angulo-direito angulo-esquerdo)))
      (images->xml imagem nome))))