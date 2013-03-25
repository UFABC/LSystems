(declare (uses tcp))

(define porta 9000)

(define enviar-mensagem
  (lambda (mensagem)
    (let-values ( ((in out) (tcp-connect "localhost" porta)))
      (write-line mensagem out)
      (print (read-line in))
      (close-output-port out)
      )))

(define imprimir-menu
  (lambda ()
    (display "1) Criar arquivo svg, dada uma determinada linguagem.")
    (newline)
    (display "2) Editar ultima linguagem enviada e criar novo arquivo.")
    (newline)
    (display "3)Executar o programa de acordo com um valor N de passos passado.")
    (newline)
    (display "Digite sair para sair.")))

(define calcular-resposta 
  (lambda(entrada)
    (enviar-mensagem entrada)
    ))

(define executar
  (lambda ()
    (let ((leitura (read)))
      (if (not (equal? leitura 'sair))
          (begin
            (calcular-resposta  leitura)
            (executar))))))

(begin 
  (imprimir-menu)
  (newline)
  (executar))