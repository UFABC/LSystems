(use tcp      ;; TCP/IP
     srfi-13  ;; String o;
     srfi-18) ;; Threads
(load "draw-svg.scm")
;(load "l-systems.scm")

(define porta 9000)
(define linguagem-atual "")
(define retorno-dummy "(svg version 1.1 xmlns http://www.w3.org/2000/svg (polygon fill rgb(255,0,0) stroke blue stroke-width 3 points 0,0 200,200 0,200 ) (line x1 0 y1 100 x2 100 y2 100 fill green stroke green stroke-width 5))")

(define criar-l-system
  (lambda (texto)
    (print texto)
    "aqui vc vai tratar o texto e retornar o que vc precisa pra criar o l-system"))

(define editar-l-system
  (lambda (texto)
    (print texto)
    "aqui vc vai tratar o texto e retornar o que vc precisa pra criar o l-system, apenas alterando uma linha do que já foi feito"))

(define executar-l-system
  (lambda (texto)
    (print texto)
    "altera apenas o número de iteração, mas retorna um texto novo pra criar o svg novamente, ou seja só muda uma linha do texto tratado"))

(define criar-svg
  (lambda (l-system-texto)
    (print l-system-texto) ;Recebe o l-system em forma de texto e retorna o svg criado utilizando as funções para criar l-system
    retorno-dummy))

(define interage
  (lambda (in out l-system-criado)
    ;; Manda um olá para o Fulano que conectou,
    ;; na porta out:
    (let ((svg-criado (open-output-string))(linha (read-line in))) ;; lê da porta in
      (display (format "Client enviou dados: ~a~%" linha))
      (let ((acao (substring linha 1 2))
            (conteudo (substring linha 2 (- (string-length linha) 1))))
        (cond ((string= acao "1")
               (display (criar-l-system conteudo) l-system-criado))
              ((string= acao "2")
               (display l-system-criado (editar-svg conteudo l-system-criado)))
              ((string= acao "3")
               (display l-system-criado (criar-svg conteudo)))
              (else
               (display "Nenhuma opção válida")
               (display "Axioma não criado" l-system-criado)
               (svg-criado "Opção inválida")));; display LOCAL!
        
        (flush-output svg-criado) ;; esvazia o buffer, mandando o que estiver pendente
        (display (criar-svg (get-output-string l-system-criado)) svg-criado)
        (display (get-output-string svg-criado) out)
        (newline out)
        (flush-output out) ;; esvazia o buffer, mandando o que estiver pendente
        (interage in out l-system-criado)))));; Poderia entrar em loop e continuar interagindo!

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
                          (interage in out (open-output-string))
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