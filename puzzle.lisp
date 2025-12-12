#|
# Projeto 1 - Inteligência Artificial
#
# Ficheiro destinado a implementar a solução do problema, definição dos operadores e heurísticas.
#
# Docente > Joaquim Filipe
#
# Alunos:
# > Danilo Victor, 202300224
# > Jean Oliveira, 202300095
# > Lucas Almeida, 202100067
#
# > Nota: Funções assinaladas com (*) são funções definidas além do enunciado.
|#


;; Variáveis de teste e operadores

;;; Tabuleiros

(defun tabuleiro-teste ()
  "Tabuleiro de teste sem nenhuma jogada realizada"
  '((nil nil 0   0   0   nil nil)
(nil nil 0   0   0   nil nil)
(0    0   0   1   1   1    1)
(0    0   0   0   1   1    1)
(0    0   0   1   1   1    1)
(nil nil 0   0   0   nil nil)
(nil nil 0   0   0   nil nil))
)

(defun no-teste ()
  "Cria um nó para testes"
  (list (tabuleiro-teste) 0 (heuristica-2 (tabuleiro-teste)) nil)
)

;; Seletores

;; Linha
(defun linha (n tabuleiro)
  "Retorna a linha n do tabuleiro"
  (cond ((posicao-validap n) (nth (1- n) tabuleiro))
             (t nil) 
   )
)

;; Coluna
(defun coluna (n tabuleiro)
  "Retorna a coluna n do tabuleiro"
  (cond ((posicao-validap n) (mapcar #'(lambda (x) (nth (1- n) x)) tabuleiro))
             (t nil)
   )
)

;; Célula
(defun celula (x y tabuleiro)
  "Retorna a célula (x, y) do tabuleiro"
  (cond ((and (posicao-validap x) (posicao-validap y)) (nth (1- y) (nth (1- x) tabuleiro)))
             (t nil)
   )
)

;; Funções Auxiliares

;; Celula-validap
(defun celula-validap (x y tabuleiro)
  "Determina se a célula (x, y) do tabuleiro é válida (!= nil)"
  (cond ((null (celula x y tabuleiro)) nil)
             (t t)
   )
)

;; Substituir-posicao
(defun substituir-posicao (n linha x)
  "Substitui o indice n da linha por x"
  (cond ((or (not (posicao-validap n)) (null (nth (1- n) linha))) nil)
             ((and (/= x 0) (/= x 1)) nil)
             ((= n 1) (cons x (rest linha)))
             (t (cons (first linha) (substituir-posicao (1- n) (rest linha) x)))
   )
)

;; Substituir
(defun substituir (l n tabuleiro x)
  "Substituir o indice n da linha l do tabuleiro por x"
  (cond ((null tabuleiro) nil)
             ((= l 1) (cons (substituir-posicao n (first tabuleiro) x) (rest tabuleiro)))
             (t (cons (first tabuleiro) (substituir (1- l) n (rest tabuleiro) x)))
  )
)

;; Posição Válida (*)
(defun posicao-validap(x)
  "Validar se a posição x é válida no tabuleiro 7x7"
  (cond ((or (< x 1) (> x 7)) nil)
             (t t)
   )
)

;; Operadores

;; Operadores
(defun operadores ()
  "Cria uma lista com todos os operadores do jogo"
  (list 'operador-cd 'operador-ce 'operador-cc 'operador-cb)
)

;; Captura Direita
(defun operador-cd (x y tabuleiro)
  "Realizar uma captura de pino à direita"
  (cond ((null tabuleiro) nil)
             ((or (not (celula-validap x y tabuleiro))
                  (not (celula-validap x (+ y 2) tabuleiro))) nil)
             ((equal (celula x y tabuleiro) 0) nil)
             ((equal (celula x (1+ y) tabuleiro) 0) nil)
             ((equal (celula x (+ y 2) tabuleiro) 1) nil)
             (t (substituir x y (substituir x (1+ y) (substituir x (+ y 2) tabuleiro 1) 0) 0))
   )
)

;; Captura Esquerda
(defun operador-ce (x y tabuleiro)
  "Realizar uma captura de pino à esquerda"
  (cond ((null tabuleiro) nil)
             ((or (not (celula-validap x y tabuleiro))
                  (not (celula-validap x (- y 2) tabuleiro))) nil)
             ((equal (celula x y tabuleiro) 0) nil)
             ((equal (celula x (1- y) tabuleiro) 0) nil)
             ((equal (celula x (- y 2) tabuleiro) 1) nil)
             (t (substituir x y (substituir x (1- y) (substituir x (- y 2) tabuleiro 1) 0) 0))
   )
)

;; Captura Cima
(defun operador-cc (x y tabuleiro)
  "Realizar uma captura de pino à cima"
  (cond ((null tabuleiro) nil)
             ((or (not (celula-validap x y tabuleiro))
                  (not (celula-validap (- x 2) y tabuleiro))) nil)
             ((equal (celula x y tabuleiro) 0) nil)
             ((equal (celula (1- x) y tabuleiro) 0) nil)
             ((equal (celula (- x 2) y tabuleiro) 1) nil)
             (t (substituir x y (substituir (1- x) y (substituir (- x 2) y tabuleiro 1) 0) 0))
   )
)

;; Captura Baixo
(defun operador-cb (x y tabuleiro)
  "Realizar uma captura de pino à baixo"
  (cond ((null tabuleiro) nil)
             ((or (not (celula-validap x y tabuleiro))
                  (not (celula-validap (+ x 2) y tabuleiro))) nil)
             ((equal (celula x y tabuleiro) 0) nil)
             ((equal (celula (1+ x) y tabuleiro) 0) nil)
             ((equal (celula (+ x 2) y tabuleiro) 1) nil)
             (t (substituir x y (substituir (1+ x) y (substituir (+ x 2) y tabuleiro 1) 0) 0))
   )
)

;; Nós

;; Construtor
(defun cria-no (tabuleiro heuristica &optional (p 0) (pai nil))
  "Criar um nó com o estado do tabuleiro sua profundidade e seu nó pai"
  (list tabuleiro p (funcall heuristica tabuleiro) pai)
)

;; Seletores
;; No-estado
(defun no-estado (no)
  "Ver o estado do tabuleiro"
  (first no)
)

;; No-profundidade
(defun no-profundidade (no)
  "Ver a profundidade do nó"
  (second no)
)

;; No-pai
(defun no-pai (no)
  "Ver o nó pai do nó"
  (fourth no)
)

(defun no-heuristica (no)
  "Ver a heuristica do nó"
  (third no)
)

(defun no-valor (no)
  "Calcula o valor do nó"
  (+ (no-profundidade no) (no-heuristica no))
)

(defun no-solucaop (no)
  "Verfica se o nó é solução"
  (let ((x (apply #'+ (mapcar #'(lambda (linha) (count 1 linha)) (no-estado no)))))
       (cond ((= 1 x) no)
          (t nil)
       )
  )
)

(defun movimentos-possiveis (tabuleiro)
  "Calcula a quantidade de movimentos possiveis em um tabuleiro"
  (apply #'+ (mapcar #'(lambda(operador) (contagem-movimentos tabuleiro operador)) (operadores)))
)

(defun contagem-movimentos (tabuleiro operador &optional(x 1) (y 1))
  "Gera um novo nó, a partir do operador e nó pai"
  (cond ((> x 7) 0)
        ((> y 7) (contagem-movimentos tabuleiro operador (1+ x)))
        (t (let* ((novo-tabuleiro (funcall operador x y tabuleiro))
                (contador (cond ((null novo-tabuleiro) (+ 0 (contagem-movimentos tabuleiro operador x (1+ y))))
                                (t (+ 1 (contagem-movimentos tabuleiro operador x (1+ y))))
                )))
                contador
           )
        )
  )
)

(defun distancia-ao-centro (tabuleiro &optional(x 1) (y 1))
  "Faz a soma da distancia de cada peão ao centro do tabuleiro (4 4)"
  (cond ((> x 7) 0)
        ((> y 7) (distancia-ao-centro tabuleiro (1+ x)))
        (t (let* ((contador (cond ((or (null (celula x y tabuleiro)) (= (celula x y tabuleiro) 0))
                                    (+ 0 (distancia-ao-centro tabuleiro x (1+ y))))
                                  (t (+ (+ (abs (- x 4)) (abs (- y 4))) (distancia-ao-centro tabuleiro x (1+ y))))
                           )
                ))
                contador
           )
        )
  )
)

(defun pinos-isolados (tabuleiro &optional(x 1) (y 1))
  (cond ((> x 7) 0)
        ((> y 7) (pinos-isolados tabuleiro (1+ x)))
        (t (+ (pino-isoladop tabuleiro x y) (pinos-isolados tabuleiro x (1+ y))))
  )
)

(defun pino-isoladop (tabuleiro x y)
  (let ((valor-celula (celula x y tabuleiro)))
    (cond ((or (null valor-celula) (= valor-celula 0)) 0)
          (t (let ((adjacentes-vazias 
                    (and (or (not (celula-validap (1- x) y tabuleiro))
                             (= (celula (1- x) y tabuleiro) 0))
                         (or (not (celula-validap (1+ x) y tabuleiro))
                             (= (celula (1+ x) y tabuleiro) 0))
                         (or (not (celula-validap x (1- y) tabuleiro))
                             (= (celula x (1- y) tabuleiro) 0))
                         (or (not (celula-validap x (1+ y) tabuleiro))
                             (= (celula x (1+ y) tabuleiro) 0)))))
               (cond (adjacentes-vazias 1) (t 0))
             )
          )
    )
  )
)