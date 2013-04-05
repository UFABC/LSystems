(load "lsystem.scm")
(define input "F = F[-F[+F]][+F[-F]]$ it 5$ esq 30$ dir 30$")

(define remove-char
  (lambda (texto char)
    (list->string
      (let remove-char-aux ((texto texto) (char char) (posicao 0))
        (if (= (string-length texto) posicao)
          '()
          (if (eq? (string-ref texto posicao) char)
            (remove-char-aux texto char (+ posicao 1))
            (cons (string-ref texto posicao) (remove-char-aux texto char (+ posicao 1)))))))))

(define split
  (lambda (linha separador)
    (let split-aux ((linha linha) (sep separador) (posicao 0) (resultado '()))
      (if (= posicao (string-length linha))
        (append resultado (list linha))
        (if (eq? (string-ref linha posicao) sep)
          (let ((sub-linha (substring linha (+ posicao 1) (string-length linha)))
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

(define split-by-chars
  (lambda (linha abre-campo fecha-campo)
    (let split-by-chars-aux ((linha linha) (ac abre-campo) (fc fecha-campo) (posicao 0) (resultado '()))
      (if (or (string=? linha "") (= (string-length linha) posicao))
        resultado
        (if (eq? (string-ref linha 0) ac)
          (if (eq? (string-ref linha posicao) fc)
            (let ((sub-linha (substring linha (+ posicao 1) (string-length linha)))
                  (resultado (append resultado (list (substring linha 1 posicao)))))
              (split-by-chars-aux sub-linha ac fc 0 resultado))
            (split-by-chars-aux linha ac fc (+ posicao 1) resultado))        
          (let ((sub-linha (substring linha 1 (string-length linha))))
                  (split-by-chars-aux sub-linha ac fc posicao resultado)))))))

(define input->args
  (lambda (texto)
    (let* ((campos (split texto #\$))
           (string-regras (remove-char (car campos) #\space))
           (regras (split string-regras #\=))
           (regras (list "regras" (list (cons (string-ref (car regras) 0) (list (string->list (cadr regras))))))))
      (print campos)
      (let input->args-aux ((campos (cdr campos)) (args (list regras)))
        (if (null? campos)
            args
            (let ((arg (tirar-strings-vazias(split (car campos) #\space))))
               (if (eq? arg '())
                 (input->args-aux (cdr campos) args)
                 (if (char-numeric? (string-ref (cadr arg) 0))
                   (let ((arg (list (car arg) (string->number (cadr arg)))))
                     (input->args-aux (cdr campos) (cons arg args)))
                   (input->args-aux (cdr campos) (cons arg args))))))))))

(define args (input->args input))
(print args) ;só para ver o que a função está produzindo
(iniciar-l-system args)
(define regras (cadr (assoc "regras" args)))