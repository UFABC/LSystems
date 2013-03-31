(use tcp      ;; TCP/IP
     srfi-13  ;; String o;
     srfi-18) ;; Threads
(load "draw-svg.scm")
;;(load "l-systems.scm")

(define porta 9000)
(define linguagem-atual "")
(define svg-criado "svg vazio, algum erro ocorreu!")

(define criar-svg
  (lambda (texto)
    (print texto)))

(define editar-svg
  (lambda (texto)
    (print texto)))

(define executar-svg
  (lambda (texto)
    (print texto)))
(define interage
  (lambda (in out)
    ;; Manda um olá para o Fulano que conectou,
    ;; na porta out:
    (let ((linha (read-line in))) ;; lê da porta in
      (display (format "Client enviou dados: ~a~%" linha))
      
      (let ((acao (substring linha 1 2))
            (conteudo (substring linha 2 (- (string-length linha) 1))))
        (cond ((string= acao "1")
               (criar-svg conteudo))
              ((string= acao "2")
               (criar-svg conteudo))
              ((string= acao "3")
               (criar-svg conteudo))
              (else
               (print "Nenhuma opção válida")))));; display LOCAL!
    (display svg-criado out)
    (newline out)
    (flush-output out) ;; esvazia o buffer, mandando o que estiver pendente
    (interage in out)));; Poderia entrar em loop e continuar interagindo!

;; Aceita conexão TCP e chama interage.
(define trata
  (lambda (s)
    ;; tcp-accept aceita uma conexão e retorna duas portas,
    ;; uma de entrada e uma de saída
    (let-values (((in out) (tcp-accept s)))
      ;; tcp-addresses devolve dois IPs -- o do servidor e
      ;; o do cliente que se conectou.
      (let-values (((meu-ip ip-dele) (tcp-addresses in)))
        ;; Comece a thread que interage:
        (thread-start! (make-thread
                        (lambda ()
                          (interage in out)
                          ;; após interagir, fechamos a porta:
                          (close-output-port out))))))

    ;; Depois de criar uma thread para cuidar da conexão, reinicie
    ;; (chama trata novamente):
    (trata s)))


;; Pega uma porta, começa a ouvir, e chama o tratador
;; de socket.
(define inicia-servidor
  (lambda ()
    (let ((socket (tcp-listen porta)))
      (trata socket))))

(inicia-servidor)