(use tcp      ;; TCP/IP
     srfi-13  ;; String o;
     srfi-18) ;; Threads
(load "draw-svg.scm")

(define porta 9000)
(load "lsystem.scm")

(define remove-char
  (lambda (texto char)
    (list->string
      (let remove-char-aux ((texto texto) (char char) (posicao 0))
        (if (= (tamanho texto) posicao)
          '()
          (if (eq? (string-ref texto posicao) char)
            (remove-char-aux texto char (+ posicao 1))
            (cons (string-ref texto posicao) (remove-char-aux texto char (+ posicao 1)))))))))

(define split
  (lambda (linha separador)
    (let split-aux ((linha linha) (sep separador) (posicao 0) (resultado '()))
      (if (= posicao (tamanho linha))
        (append resultado (list linha))
        (if (eq? (string-ref linha posicao) sep)
          (let ((sub-linha (substring linha (+ posicao 1) (tamanho linha)))
                (resultado (append resultado (list (substring linha 0 posicao)))))
            (split-aux sub-linha sep 0 resultado))
          (split-aux linha sep (+ posicao 1) resultado))))))

(define tirar-strings-vazias
  (lambda (lista)
    (if (null? lista)
      '()
      (if (string=? (car lista) "")
          (tirar-strings-vazias (cdr lista))
          (cons (car lista) (tirar-strings-vazias (cdr lista)))))))

(define cria-regras
  (lambda (regras-em-texto)
    (let ((lista-de-strings-de-regras (split regras-em-texto #\$)))
       (let cria-regras-aux ((lista lista-de-strings-de-regras) (regras '()))
          (if (null? lista)
              (cons "regras" regras)
              (let* ((regra-temp (split (car lista) #\=))  
                    (variavel (string-ref (car regra-temp) 0))
                    (producao (string->list (cadr regra-temp)))
                    (regra (cons variavel (list producao))))
                (cria-regras-aux (cdr lista) (append regras (list (list regra))))))))))

(define tamanho
  (lambda (objeto)
    (cond ((null? objeto) 0)
          ((list? objeto) (length objeto))
          ((string? objeto) (string-length objeto)))))

(define split-by-chars
  (lambda (linha abre-campo fecha-campo)
    (let split-by-chars-aux ((linha linha) (ac abre-campo) (fc fecha-campo) (posicao 0) (resultado '()))
      (if (or (string=? linha "") (= (tamanho linha) posicao))
        resultado
        (if (eq? (string-ref linha 0) ac)
          (if (eq? (string-ref linha posicao) fc)
            (let ((sub-linha (substring linha (+ posicao 1) (tamanho linha)))
                  (resultado (append resultado (list (substring linha 1 posicao)))))
              (split-by-chars-aux sub-linha ac fc 0 resultado))
            (split-by-chars-aux linha ac fc (+ posicao 1) resultado))        
          (let ((sub-linha (substring linha 1 (tamanho linha))))
                  (split-by-chars-aux sub-linha ac fc posicao resultado)))))))

(define input->args
  (lambda (texto)
    (let* ((campos (split texto #\$))
           (string-regras-temp (remove-char (car (split-by-chars texto #\{ #\})) #\space))
           (regras (cria-regras string-regras-temp))  
           (axioma (cons "axioma" (list (list (car (car (car (cdr regras)))))))))
      (let input->args-aux ((campos (cdr campos)) (args (list regras axioma)))
        (if (null? campos)
            args
            (let ((arg (tirar-strings-vazias(split (car campos) #\space))))
               (if (eq? arg '())
                 (input->args-aux (cdr campos) args)
                 (let ((arg (list (car arg) (string->number (cadr arg)))))
                   (input->args-aux (cdr campos) (cons arg args))))))))))

(define editar-l-system
  (lambda (args texto)
    (print texto)
    "aqui vc vai tratar o texto e retornar o que vc precisa pra criar o l-system, apenas alterando uma linha do que já foi feito"))

(define separar-numero-iteracao
  (lambda (texto)
    (print texto)
    ))

(define alterar-iteracoes
  (lambda (lista nova-iteracao)
    (if (null? lista)
      '()
      (if (string=? (car (car lista)) "it")
          (cons '("it" nova-iteracao) (cdr lista))
          (cons (car lista) (alterar-iteracoes (cdr lista) nova-iteracao))))))

(define menu-editar
  (lambda (l-system porta)
    (display (car l-system) porta)
    (newline porta)))

(define interage
  (lambda (in out l-system-criado)
    ;; Manda um olá para o Fulano que conectou,
    ;; na porta out:
    (let ((svg-criado (open-output-string))(linha (read-line in))) ;; lê da porta in
      (display (format "Client enviou dados: ~a~%" linha))
      (let ((acao (substring linha 1 2))
            (conteudo (substring linha 2 (- (string-length linha) 1))))
        (cond ((string= acao "1")
;;               (display conteudo l-system-criado)
               (set! l-system-criado (input->args conteudo)))
              ((string= acao "2")
               (set! l-system-criado (editar-svg l-system-criado conteudo)))
              ((string= acao "3")
               (set! l-system-criado (alterar-iteracoes args (separar-numero-iteracao conteudo))))
              (else
               (display "Nenhuma opção válida")
               (set! l-system-criado '("Axioma não criado"))
               (display "Opção inválida" svg-criado)));; display LOCAL!
        (if (not (string= acao "4"))
            (begin
              (print (string? (iniciar-l-system l-system-criado)))
              (print (list? (iniciar-l-system l-system-criado)))
              (write (iniciar-l-system l-system-criado) svg-criado)
              (print (get-output-string svg-criado))
              (write (get-output-string svg-criado) out)
              (flush-output svg-criado)) ;; esvazia o buffer, mandando o que estiver pendente
            (menu-editar l-system-criado out))
        (flush-output out) ;; esvazia o buffer, mandando o que estiver pendente
        (newline out)
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
                          (interage in out '())
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