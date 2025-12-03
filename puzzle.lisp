#|
# Projeto 1 - Intelig�ncia Artificial
#
# Ficheiro destinado a implementar a solu��o do problema, defini��o dos operadores e heur�sticas.
#
# Docente > Joaquim Filipe
#
# Alunos:
# > Danilo Victor, 202300224
# > Jean Oliveira, 202300095
# > Lucas Almeida, 202100067
#
# > Nota: Fun��es assinaladas com (*) s�o fun��es definidas al�m do enunciado.
|#


;; Vari�veis de teste e operadores

;;; Tabuleiros

(defun tabuleiro-teste ()
"Tabuleiro de teste sem nenhuma jogada realizada"
  '(
	 (nil nil 1 1 1 nil nil)
	 (nil nil 1 1 1 nil nil)
	 (1 1 1 1 1 1 1)
	 (1 1 1 0 1 1 1)
	 (1 1 1 1 1 1 1)
	 (nil nil 1 1 1 nil nil)
	 (nil nil 1 1 1 nil nil)
	)
)

(defun no-teste ()
  "Cria um n� para testes"
  (list (tabuleiro-teste) 0 nil)
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

;; C�lula
(defun celula (x y tabuleiro)
  "Retorna a c�lula (x, y) do tabuleiro"
  (cond ((and (posicao-validap x) (posicao-validap y)) (nth (1- x) (nth (1- y) tabuleiro)))
             (t nil)
   )
)

;; Fun��es Auxiliares

;; Celula-validap
(defun celula-validap (x y tabuleiro)
  "Determina se a c�lula (x, y) do tabuleiro � v�lida (!= nil)"
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

;; Posi��o V�lida (*)
(defun posicao-validap(x)
  "Validar se a posi��o x � v�lida no tabuleiro 7x7"
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
  "Realizar uma captura de pino � direita"
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
  "Realizar uma captura de pino � esquerda"
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
  "Realizar uma captura de pino � cima"
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
  "Realizar uma captura de pino � baixo"
  (cond ((null tabuleiro) nil)
             ((or (not (celula-validap x y tabuleiro))
                  (not (celula-validap (+ x 2) y tabuleiro))) nil)
             ((equal (celula x y tabuleiro) 0) nil)
             ((equal (celula (1+ x) y tabuleiro) 0) nil)
             ((equal (celula (+ x 2) y tabuleiro) 1) nil)
             (t (substituir x y (substituir (1+ x) y (substituir (+ x 2) y tabuleiro 1) 0) 0))
   )
)

;; N�s

;; Construtor
(defun cria-no (tabuleiro &optional (p 0) (pai nil))
  "Criar um n� com o estado do tabuleiro sua profundidade e seu n� pai"
  (list tabuleiro p pai)
)

;; Seletores
;; No-estado
(defun no-estado (no)
  "Ver o estado do tabuleiro"
  (first no)
)

;; No-profundidade
(defun no-profundidade (no)
  "Ver a profundidade do n�"
  (second no)
)

;; No-pai
(defun no-pai (no)
  "Ver o n� pai do n�"
  (third no)
)
