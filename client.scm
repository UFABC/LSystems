(declare (uses tcp))

(define porta 9000)

(define enviar-mensagem
  (lambda (mensagem)
    (let-values ( ((in out) (tcp-connect "localhost" porta)))
      (display mensagem out)
      (newline out)
      
      (display (read-line in))
      )))

(define ler-axioma
   (lambda ()
     (read-char) ; descartar ultimo caractere lido
     (let loop ((c (peek-char)) (exps '()))
       (cond ((eof-object? c)
              (error "EOF encountered while parsing { ... } clause"))
             ((char=? c #\newline)
              (read-char)   ; discard
              `(,@(reverse exps)))
             (else
              (let ((exp (read)))
                (loop (peek-char)
                      (cons exp exps))))))))

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
    (display "Você escolheu criar novo, digite um par, com a chava da linguagem e a linguagem usada.")
    (newline)
    (display "Sempre digite uma chave por linha e sua função separados por -, como no exemplo:")
    (newline)
    (display "A - (F 1.5432) (F 1.4324) (F 1.432)")
    (newline)
    (display "Defina os números de interações com: interacoes (numero)")
    (newline)
    (display "Encerre o processo digitando o comando fim.")
    (newline)))

(define calcular-resposta 
  (lambda(entrada)
    (cond ((equal? entrada '1)
          (menu-criar-novo)
          (executar-criacao))
        (else
          (display "Parâmetro incorreto, digite novamente.")
          (newline)
          (executar)))
    ))

(define executar-criacao
  (lambda ()
    (let ((sistema-l (ler-axioma)))
      (if (not (equal? sistema-l '(fim)))
          (enviar-mensagem sistema-l)
          (executar-criacao)))))
    
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