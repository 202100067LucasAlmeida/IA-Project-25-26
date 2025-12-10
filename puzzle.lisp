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
  "Cria um nó para testes"
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

;; Operadores
(defun operadores ()
  "Cria uma lista com todos os operadores do jogo solitário"
  (list 'operador-cd 'operador-ce 'operador-cc 'operador-cb)
)

;; Captura Direita
(defun operador-cd (x y tabuleiro)
  "Realizar uma captura de pino à direita"
  (cond ((null tabuleiro) nil)
             ((not (celula-validap x y tabuleiro)) nil)
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
             ((not (celula-validap x y tabuleiro)) nil)
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
             ((not (celula-validap x y tabuleiro)) nil)
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
             ((not (celula-validap x y tabuleiro)) nil)
             ((equal (celula x y tabuleiro) 0) nil)
             ((equal (celula (1+ x) y tabuleiro) 0) nil)
             ((equal (celula (+ x 2) y tabuleiro) 1) nil)
             (t (substituir x y (substituir (1+ x) y (substituir (+ x 2) y tabuleiro 1) 0) 0))
   )
)

;; Nós

;; Construtor
(defun cria-no (tabuleiro &optional (p 0) (pai nil))
  "Criar um nó com o estado do tabuleiro sua profundidade e seu nó pai"
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
  "Ver a profundidade do nó"
  (second no)
)

;; No-pai
(defun no-pai (no)
  "Ver o nó pai do nó"
  (third no)
)

;; No-solucao
(defun no-solucaop (no)
  "Verifica se o nó é solução, se sim devolve o nó"
  (cond ((some #'(lambda (x) (= x 1)) (no-estado no)) no)
        (t nil)
  )
)

;; No-existep
(defun no-existep (no nos)
  (cond ((or (null no) (null nos)) nil)
             ((not (listp nos)) nil)
             ((equal no (first nos)) t)
             (t (no-existep no (rest nos)))
   )
)


;; Sucessores

;; novo-sucessor
(defun novo-sucessor (no operador)
  "Cria um novo sucessor do no aplicado o operador"
  (let ((novo-estado (and no operador
                          (eval (list operador (list 'quote (no-estado no)))))))
    (cond
      ((null no) nil)
      ((null operador) nil)
      ((null novo-estado) nil)
      (t (cria-no novo-estado (+ (no-profundidade no) 1) no)))
  )
)

;; Sucessores
(defun sucessores (no operadores procura &optional (profundidade))
  "Gera a lista de sucessores no no"
  (cond ((null no) nil)
             ((null operadores) nil)
             ((null procura) nil)
             ((and (equal procura 'dfs) (= (no-profundidade no) profundidade)) nil)
             (t (mapcar #'(lambda (x) (novo-sucessor no x)) operadores))
   )
)
