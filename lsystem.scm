(load "draw-svg.scm")
(define altura-folhas 0) ;indica, a partir das pontas do L-system, qual profundidade do desenho será desenhada como folha
(define comprimento-reta 0) ;comprimento base para o desenho
(define largura-reta 5)
(define pilha (list (vector 300 0 0))) ;pilha é uma lista de vetores que contêm os valores de x, y e o angulo atual do sistema
(define axioma '()) ;axioma é uma lista de um só elemento
(define regras '()) ;regras é uma Alist

(define-syntax get 
  ;faz referencia dinamica às variáveis na pilha. Isso é necessário porque o (define...) cria um vínculo com o valor retornado pelo procedimento, e não ao procedimento em si, o que significa que o valor será sempre igual, mesmo se o programa alterar a estrutura dos parametros que o procedimento vinculado recebeu.
  ;cor está nessa macro porque é algo que não valia a pena para fazer uma função
  (syntax-rules (x y angulo estado)
    ((_ x) (vector-ref (car pilha) 0))
    ((_ y) (vector-ref (car pilha) 1))
    ((_ angulo) (vector-ref (car pilha) 2))
    ((_ estado) (car pilha))
    ((_ cor) (if (> (length pilha) altura-folhas)
                "green"
                "brown"))))

(define-syntax set-ref! 
  ;seta as posicoes da pilha que corresponderem às variaveis nos templates
  (syntax-rules (x y angulo estado)
    ((_ x valor) (set! (vector-ref (car pilha) 0) valor))
    ((_ y valor) (set! (vector-ref (car pilha) 1) valor))
    ((_ x y valorX valorY) (begin 
                            (set! (vector-ref (car pilha) 0) valorX)
                            (set! (vector-ref (car pilha) 1) valorY)))
    ((_ angulo valor) (set! (vector-ref (car pilha) 2) valor))))
    
(define altera-angulo! 
  ;soma ao angulo atual
  (lambda (incremento)
    (set-ref! angulo (+ (get angulo) incremento))))

(define empilha-estado!
  (lambda()
    (set! pilha (cons (vector (get x) (get y) (get angulo)) pilha))))

(define desempilha-estado!
  (lambda ()
    (set! pilha (cdr pilha))))

(define F ;Desenha um traço. É a peça elementar do L-system
  (lambda (xf yf)
    (make-svg-line (get x) (get y) xf yf 
                   (get cor);cor do preenchimento
                   (get cor); contorno
                   largura-reta)))

(define calcula-final-de-reta ; calcula xf e yf com base e x, y e angulo na pilha
  (lambda ()
    (let ((incX (* (sin (get angulo)) comprimento-reta))
          (incY (* (cos (get angulo)) comprimento-reta)))
      (cons (+ (get x) incX) (+ (get y) incY)))))

(define desenhar-svg
  (lambda (frase angulo-direito angulo-esquerdo)
    (let desenhar-aux ((frase frase) (ang-dir angulo-direito) (ang-esq angulo-esquerdo) (xml-list '()))
      (print pilha)
      (print frase)
      (if (null? frase)
        (make-svg-image xml-list)
        (begin
          (case (car frase)
            ((#\F) (let* ((final-reta (calcula-final-de-reta))
                          (xf (car final-reta))
                          (yf (cdr final-reta))
                          (reta (F xf yf)))
                      (set! xml-list (cons reta xml-list))
                      (set-ref! x y xf yf)))
            ((#\[) (empilha-estado!))
            ((#\]) (desempilha-estado!))
            ((#\-) (set-ref! angulo (+ (get angulo) ang-esq)))
            ((#\+) (set-ref! angulo (+ (get angulo) ang-dir)))
            (else (make-svg-image xml-list))) ;acontece de haver variaveis sem significado gráfico. Nesse caso, nada é feito.
            (desenhar-aux (cdr frase) ang-dir ang-esq xml-list))))))

(define assoc-with-default ;pesquisa em um alist um valor. Se não econtrar retorna o valor default
  (lambda (termo alist default)
    (let ((valor (assoc termo alist)))
    (if valor 
      (cadr valor)
          default))))

(define derivar-frase
  (lambda (frase)
    (let derivar-frase-aux((var (car frase)) 
                           (restante (cdr frase))
                           (frase-derivada '()))
      (let ((derivacao (assoc-with-default var regras (list var))))
        (if (null? restante)
            (append frase-derivada derivacao)
            (derivar-frase-aux (car restante) (cdr restante) (append frase-derivada derivacao)))))))

(define iterar-l-system
  (lambda (n frase)
   (if (zero? n)
       frase
       (begin (iterar-l-system (- n 1) (derivar-frase frase))))))

(define iniciar-l-system
  (lambda (args)
    (let* ((nome-imagem (assoc-with-default "nome" args "l-systems-server-side.svg"))
           (angulo-direito (assoc-with-default "dir" args -0.5))
           (angulo-esquerdo (assoc-with-default "esq" args 0.5))
           (regra-teste (list (list #\F (string->list "F[+F][-F]"))))
           (iteracoes (assoc-with-default "it" args 4)))
      (set! altura-folhas iteracoes)
      (set! comprimento-reta (assoc-with-default "d" args 75))
      (set! regras (assoc-with-default "regras" args regra-teste))
      (set! axioma (assoc-with-default "axioma" args '(#\F)))
      (let* ((frase-inicial (assoc-with-default "frase" args axioma))
             (frase-final (iterar-l-system iteracoes frase-inicial))
             (imagem (desenhar-svg frase-final angulo-direito angulo-esquerdo)))
        (print (list? imagem))
        (images->xml imagem nome-imagem)
        frase-final))))
