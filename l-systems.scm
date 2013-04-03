(load "draw-svg.scm")
(define comprimento-reta 100)
(define pilha (list (vector 0 0 0)))
(define regras '())

(define-syntax get
  (syntax-rules (x y angulo estado)
    ((_ x) (vector-ref (car pilha) 0))
    ((_ y) (vector-ref (car pilha) 1))
    ((_ angulo) (vector-ref (car pilha) 2))
    ((_ estado) (car pilha))))

(define set-xy!
  (lambda (ponto)
    (set! (get x) (car ponto))
    (set! (get y) (cdr ponto))))

(define altera-angulo!
  (lambda (incremento)
    (set! (get angulo)  (+ (get angulo) incremento))))

(define empilha-estado!
  (lambda ()
      (set! pilha (cons (get estado) pilha))))

(define desempilha-estado!
  (lambda ()
    (set! pilha (cdr pilha))))

(define F ;Desenha um traço
  (lambda (xf yf)
    (make-svg-line x y xf yf 
                   "blue" ;cor do preenchimento
                   "green" ; contorno
                   5)))

(define calcula-final-de-reta ; calcula xf e yf com base e x, y e angulo na pilha
  (lambda ()
    (let ((incX (* (cos angulo) comprimento-reta))
          (incY (* (sin angulo) comprimento-reta)))
      (cons (+ (get x) incX) (+ (get y) incY)))))

(define desenhar
  (lambda (frase ang-dir ang-esq)
    (let desenhar-aux ((frase frase) (ang-dir ang-dir) (ang-esq ang-esq) (xml-list '()))
      (if (null? frase)
        (make-svg-image xml-list)
        (begin     
          (case (car frase)
            ((#\F) (let* ((ponto-final (calcula-final-de-reta))
                          (reta (F (car ponto-final) (cdr ponto-final))))
                      (set! svg-image (cons reta xml-list))
                      (set-xy! ponto-final)))
            ((#\[) (empilha-estado!))
            ((#\]) (desempilha-estado!))
            ((#\-) (altera-angulo! ang-esq))
            ((#\+) (altera-angulo! ang-dir)))
            (desenhar-aux (cdr frase) ang-dir ang-esq xml-list))))))

(define assoc/default
  (lambda (termo alist default)
    (let ((valor (assoc termo alist)))
      (if valor
          (cadr valor)
          default))))

(define derivar-variavel
  (lambda (termo)
    (assoc/default termo regras (list termo))))
          
(define derivar-frase
  (lambda (frase)
    (let derivar-frase-aux((var (car frase)) 
                           (restante (cdr frase))
                           (frase-derivada '()))
      (let ((derivacao (derivar-variavel var)))
        (if (null? restante)
            (append derivacao frase-derivada)
            (derivar-frase-aux (car restante) (cdr restante) (append derivacao frase-derivada)))))))

(define iterar-l-system
  (lambda (n frase)
      (if (zero? n)
          frase
          (begin (iterar-l-system (- n 1) (derivar-frase frase))))))

(define-syntax set-l-system
  (syntax-rules ()
    ((_ ax reg comp) (begin (set! axioma ax) (set! regras reg) (set! comprimento-reta comp)))))
          
(define iniciar-l-system
  (lambda (args)
    (let* ((iteracoes (assoc/default "it" args 5))
           (nome-imagem (assoc/default "nome" args "figura de teste.svg"))
           (angulo-direito (assoc/default "dir" args 0.5))
           (angulo-esquerdo (assoc/default "esq" args -0.5))
           ;(regras (cdr "regras" args))
           (regras (list (list #\F (string->list "FF"))))
           ;(axioma (assoc/default "axioma" args #f))
           (axioma (cons #\F '()))
           (comprimento-reta (assoc/default "d" args 100))
           (frase-inicial (assoc/default "frase" args axioma)))
      (set-l-system axioma regras comprimento-reta)      
      (let* ((frase (iterar-l-system iteracoes frase-inicial))
            (imagem (desenhar frase angulo-direito angulo-esquerdo)))
        ;(xml-write-tag imagem)
        (images->xml imagem nome-imagem)))))

(define-syntax insere-termos
  (syntax-rules ()
    ((_ alist lista) (set! alist (append alist lista)))))
    
(define args '())

(insere-termos args '(("it" 2) '("nome" "teste.svg") '("esq" -0.5) '("dir" 0.5)))

(iniciar-l-system args)
