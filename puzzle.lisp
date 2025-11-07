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
  (cond ((and (posicao-validap x) (posicao-validap y)) (nth (1- x) (nth (1- y) tabuleiro)))
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

;; Captura Direita
(defun captura-dir (x y tabuleiro)
  "Realizar uma captura de pino à direita"
  (cond ((null tabuleiro) nil)
             ((not (celula-validap x y tabuleiro)) nil)
             (t)
   )
)

;; Captura Esquerda
(defun captura-esq (x y tabuleiro)
  "Realizar uma captura de pino à esquerda"
  
)

;; Captura Cima
(defun captura-cima (x y tabuleiro)
  "Realizar uma captura de pino à cima"
  
)

;; Captura Baixo
(defun captura-baixo (x y tabuleiro)
  "Realizar uma captura de pino à baixo"
  
)
