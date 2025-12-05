#|
# Projeto 1 - Inteligência Artificial
#
# Ficheiro destinado a implementar os métodos de procura.
#
# Docente > Joaquim Filipe
#
# Alunos:
# > Danilo Victor, 202300224
# > Jean Oliveira, 202300095
# > Lucas Almeida, 202100067
#
|#

(defun novo-sucessor (no operador)
  "Gera um novo nó, a partir do operador e nó pai"
  (cond ((or (null no) (null operador)) nil)
        (t (novo-sucessor-recursivo no operador))
  )
)

(defun novo-sucessor-recursivo (no operador &optional(x 1) (y 1))
  (cond ((> x 7) nil)
        ((> y 7) (novo-sucessor-recursivo no operador (1+ x)))
        (t (let ((novo-tabuleiro (funcall operador x y (no-estado no))))
             (cond ((null novo-tabuleiro) (novo-sucessor-recursivo no operador x (1+ y)))
                 (t (cons (cria-no novo-tabuleiro (1+ (no-profundidade no)) no) (novo-sucessor-recursivo no operador x (1+ y))))
             )
           )
        )
  )
)

(defun sucessores (no operadores procura &optional(profundidade))
  "Gera a lista de sucessores do nó"
  (cond ((null no) nil)
        ((null operadores) nil)
        ((null procura) nil)
        ((equal procura 'a*) (ordenar-nos (remove nil (mapcar #'(lambda (x) (novo-sucessor no x)) operadores))))
        ((and (equal procura 'dfs) (= (no-profundidade no) profundidade)) nil)
        (t (let ((todos-nos (apply #'append (mapcar #'(lambda (operador) (novo-sucessor no operador)) operadores))))
                (remove-if #'null todos-nos)
           )
        )
  )
)

(defun abertos-bfs (abertos sucessores)
  "Adiciona os sucessores a lista dos nós abertos no fim"
  (append abertos sucessores)
)
 
(defun abertos-dfs (abertos sucessores)
  "Adiciona os sucessores a lista dos nós abertos no início"
  (append sucessores abertos)
)

(defun bfs (no-inicial no-solucaop sucessores operadores &optional(lista-aberto) (lista-fechado))
  (cond ((or (null no-inicial) (null no-solucaop) (null sucessores) (null operadores)) nil)
        (t (bfs-recursivo no-solucaop sucessores operadores (cons no-inicial lista-aberto) lista-fechado))
  )
)

(defun bfs-recursivo (no-solucaop sucessores operadores lista-aberto lista-fechado)
  (cond ((null lista-aberto) nil)
        (t (let* ((lista-sucessores (funcall sucessores (car lista-aberto) operadores 'bfs))
                (x (format t "~a~%~%" lista-sucessores))
                (no-solucao (remove nil (mapcar #'(lambda (x) (funcall no-solucaop x)) lista-aberto))))
                (cond (no-solucao (car no-solucao))
                  (t (bfs-recursivo no-solucaop sucessores operadores (abertos-bfs (cdr lista-aberto) lista-sucessores)
                  (cons (car lista-aberto) lista-fechado)))
                )
           )
        )
  )
)

(defun dfs (no-inicial no-solucaop sucessores operadores profundidade-max &optional (lista-aberto nil) (lista-fechado nil))
  (cond ((or (null no-inicial) (null no-solucaop) (null sucessores) (null operadores) (null profundidade-max)) nil)
        (t (dfs-recursivo no-solucaop sucessores operadores profundidade-max (cons no-inicial lista-aberto) lista-fechado))
  )
)
 
(defun dfs-recursivo (no-solucaop sucessores operadores profundidade-max lista-aberto lista-fechado)
  (cond ((null lista-aberto) nil)
        ((= (no-profundidade (car lista-aberto)) profundidade-max)
            (dfs-recursivo no-solucaop sucessores operadores profundidade-max (cdr lista-aberto)
            (cons (car lista-aberto) lista-fechado))
        )
        (t (let* ((lista-sucessores (funcall sucessores (car lista-aberto) operadores 'dfs 10))
                (x (format t "~a~%~%" lista-sucessores))
                (no-solucao (remove nil (mapcar #'(lambda (x) (funcall no-solucaop x)) lista-aberto))))
                (cond (no-solucao (car no-solucao))
                  (t (dfs-recursivo no-solucaop sucessores operadores profundidade-max 
                     (abertos-dfs (cdr lista-aberto) lista-sucessores) (cons (car lista-aberto) lista-fechado)))
                )
           )
        )
  )
)

(defun a (no-inicial no-solucaop sucessores operadores profundidade-max &optional (lista-aberto nil) (lista-fechado nil))
  (recursive-a no-solucaop sucessores operadores profundidade-max (cons no-inicial lista-aberto) lista-fechado)
)

(defun recursive-a (no-solucaop sucessores operadores profundidade-max lista-aberto lista-fechado)
  (cond ((null lista-aberto) nil)
        ((= (no-profundidade (car lista-aberto)) profundidade-max) (recursive-a no-solucaop sucessores operadores profundidade-max (cdr lista-aberto) (cons (car lista-aberto) lista-fechado)))
        (t (let* ((s (filtrar-sucessores (filtrar-sucessores (funcall sucessores (car lista-aberto) operadores 'a* profundidade-max) lista-fechado) lista-aberto))
                ;(x (format t "~a~%~%" s))
                (ns (remove nil (mapcar #'(lambda (x) (funcall no-solucaop x)) lista-aberto))))
                (cond (ns (car ns))
                  (t (recursive-a no-solucaop sucessores operadores profundidade-max (abertos-dfs (cdr lista-aberto) s) (cons (car lista-aberto) lista-fechado)))
                )
           )
        )
  )
)

(defun heuristica (tabuleiro)
  "Calcula a heurística de acordo com a quantidade de movimentos possiveis no tabuleiro (Dado pelo professor!)"
  (cond ((or (= (first tabuleiro) 1) (= (second tabuleiro) 1)) 0)
        ((and (= (first tabuleiro) (second tabuleiro)) (not (= (first tabuleiro) 1))) 1)
        (t 2)
  )
)

(defun ordenar-nos (nos)
  "Ordena os nós de acordo com o valor de cada um"
  (cond ((null nos) nil)
        (t (let ((min-no (ordenar-nos-recursivo (first nos) (cdr nos))))
                (cons min-no (ordenar-nos (remove min-no nos)))
           )
        )
  )
)

(defun ordenar-nos-recursivo (no nos)
  (cond ((null nos) no)
        ((< (no-heuristica (car nos)) (no-heuristica no)) (ordenar-nos-recursivo (car nos) (cdr nos)))
        (t (ordenar-nos-recursivo no (cdr nos)))
  )
)