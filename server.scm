(use tcp      ;; TCP/IP
     srfi-18) ;; Threads
;;(load "draw-svg.scm")
;;(load "l-systems.scm")

(define porta 9000)

(define interage
  (lambda (in out)
    ;; Manda um ol� para o Fulano que conectou,
    ;; na porta out:
    (display "Ol�, Fulano do outro lado!" out)
    (newline out)
    (flush-output out) ;; esvazia o buffer, mandando o que estiver pendente
    (let ((linha (read-line in))) ;; l� da porta in
      (display (format "O Fulano disse: ~a~%" linha)) ;; display LOCAL!
      (interage in out))));; tive que deletar essa linha, por que dava pau
   ;; Poderia entrar em loop e continuar interagindo!

;; Aceita conex�o TCP e chama interage.
(define trata
  (lambda (s)
    ;; tcp-accept aceita uma conex�o e retorna duas portas,
    ;; uma de entrada e uma de sa�da
    (let-values (((in out) (tcp-accept s)))
      ;; tcp-addresses devolve dois IPs -- o do servidor e
      ;; o do cliente que se conectou.
      (let-values (((meu-ip ip-dele) (tcp-addresses in)))
        ;; Comece a thread que interage:
        (thread-start! (make-thread
                        (lambda ()
                          (interage in out)
                          ;; ap�s interagir, fechamos a porta:
                          (close-output-port out))))))

    ;; Depois de criar uma thread para cuidar da conex�o, reinicie
    ;; (chama trata novamente):
    (trata s)))


;; Pega uma porta, come�a a ouvir, e chama o tratador
;; de socket.
(define inicia-servidor
  (lambda ()
    (let ((socket (tcp-listen porta)))
      (trata socket))))

(inicia-servidor)