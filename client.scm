(declare (uses tcp))

(define porta 9000)
(load "draw-svg.scm")
(load "lsystem.scm")

(define enviar-mensagem
  (lambda (mensagem)
    (let-values ( ((in out) (tcp-connect "localhost" porta)))
      (display mensagem out)
      (newline out)
      (display "criando arquivo...")
      (let ((imagem (read in)))
        (print imagem)
        (images->xml (desenhar-svg (string->list imagem) -0.5 0.5) "l-system.svg"))
        
      (display "arquivo criado")
      (newline)    
      )))

(define menu-editar
  (lambda ()
    (let-values ( ((in out) (tcp-connect "localhost" porta)))
      (display '(4) out)
      (newline out)
      (display (read-line in))
      (newline)    
      )))

(define ler-xml
  (lambda (port)
    (let ((out (open-output-string)))
      (let loop ((c (peek-char port)))
        (cond ((eof-object? c)
               (read-char port)
               (get-output-string out))
              ((char=? c #\\)
               (read-char port)   ; discard
               (loop (peek-char port)))
              ((char-whitespace? c)
               (read-char port)
               (loop (peek-char port)))
              (else
               (let ((exp (read-char port)))
                 (display exp out)
                 (loop (peek-char port)))))))))

(define ler-axioma
  (lambda ()
    (let ((out (open-output-string)))
      (let loop ((c (peek-char)))
        (cond ((eof-object? c)
               (error "EOF encountered while parsing { ... } clause"))
              ((char=? c #\newline)
               (display "$" out)
               (read-char)   ; discard
               (get-output-string out))
              ((char-whitespace? c)
               (display " " out)
               (read-char)
               (loop (peek-char)))
              (else
               (let ((exp (read-char)))
                 (display exp out)
                 (loop (peek-char)))))))))

(define imprimir-menu
  (lambda ()
    (display "1) Criar arquivo svg, dada uma determinada linguagem.")
    (newline)
    (display "2) Editar ultima linguagem enviada e criar novo arquivo.")
    (newline)
    (display "3)Executar o programa de acordo com um valor N de passos passado.")
    (newline)
    (display "Digite sair para sair.")))

(define menu-criar-novo
  (lambda ()
    (display "Você escolheu criar novo l-system")
    (newline)
    (display "Tecle enter a cada item inserido.")
    (display "Digite { antes da primeira regra e } após a última regra.")
    (display "A regra segue um formato deste exemplo A = F[+F][-F]).")
    (newline)
    (display "Significado dos simbolos:")
    (display "[ -> empilha o estado atual e inicia um ramo com os valores do estado atual")
    (display "] -> encerra o desenho do ramo e desempilha o estado anterior")
    (display "+ -> aplica uma rotação para a direita no desenho da reta")
    (display "- -> aplica uma rotação para a direita no desenho da reta")
    (display "F -> desenha uma reta")
    (newline)
    (display "O axioma será a variável da primeira regra digitada.")
    (display "Após entrar com as regras, digite \"it\" <i>, onde i é o número de iterações ")
    (newline)
    (display "Valores opcionais: esq <angulo>, dir <angulo>, em radiano.")
    (newline)
    (display "Ao finalizar escreva a palavra fim, para encerrar a execução.")
    (newline)
    ))

(define menu-iterar-novamente
  (lambda ()
    (display "Digite aqui o número de iterações para a última linguagem proposta:")))

(define calcular-resposta
  (lambda(entrada)
    (cond ((equal? entrada '1)
           (menu-criar-novo)
           (executar-criacao))
          ((equal? entrada '2)
           (menu-editar)
           (executar-criacao))
          ((equal? entrada '3)
           (menu-iterar-novamente)
           (executar-criacao))
          (else
           (display "Parâmetro incorreto, digite novamente.")
           (newline)
           (executar)))
    ))

(define executar-criacao
  (lambda ()
    (read-char) ; descartar ultimo caractere lido
    (let criar((lista-final '(1))(sistema-l (ler-axioma)))
      (if (equal? sistema-l "fim$")
          (enviar-mensagem `(,@(reverse lista-final)))
          (criar (cons sistema-l lista-final) (ler-axioma))))))
    
(define executar
  (lambda ()
    (imprimir-menu)
    (newline)
    (let ((leitura (read)))
      (if (not (equal? leitura 'sair))
          (begin
            (calcular-resposta  leitura)
            (executar))))))
  (executar)