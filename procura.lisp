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

(defun novo-sucessor (no operador heuristica)
  "Gera um novo nó, a partir do operador e nó pai"
  (cond ((or (null no) (null operador)) nil)
        (t (novo-sucessor-recursivo no operador heuristica))
  )
)

(defun novo-sucessor-recursivo (no operador heuristica &optional(x 1) (y 1))
  (cond ((> x 7) nil)
        ((> y 7) (novo-sucessor-recursivo no operador heuristica (1+ x)))
        (t (let ((novo-tabuleiro (funcall operador x y (no-estado no))))
             (cond ((null novo-tabuleiro) (novo-sucessor-recursivo no operador heuristica x (1+ y)))
                 (t (cons (cria-no novo-tabuleiro heuristica (1+ (no-profundidade no)) no) (novo-sucessor-recursivo no operador heuristica x (1+ y))))
             )
           )
        )
  )
)

(defun sucessores (no operadores procura &key(profundidade)(limite 0)(heuristica 'heuristica-1))
  "Gera a lista de sucessores do nó"
  (cond ((null no) nil)
        ((null operadores) nil)
        ((null procura) nil)
        ((and (equal procura 'dfs) (= (no-profundidade no) profundidade)) nil)
        ((or (equal procura 'a*))
          (let ((todos-nos-ordenados (remove-if #'null (apply #'append 
                (mapcar #'(lambda (operador) (novo-sucessor no operador heuristica)) operadores)))))
          (ordenar-nos todos-nos-ordenados))
        )
        (t (let ((todos-nos (remove-if #'null (apply #'append 
                 (mapcar #'(lambda (operador) (novo-sucessor no operador heuristica)) operadores)))))
              todos-nos
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
                ;(x (format t "~a~%~%" lista-sucessores))
                ;(x (format t "~a~%~%" lista-aberto))
                ;(x (format t "~a~%~%~%" lista-fechado))
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
        (t (let* ((lista-sucessores (funcall sucessores (car lista-aberto) operadores 'dfs :profundidade profundidade-max))
                ;(x (format t "~a~%~%" lista-sucessores))
                ;(x (format t "~a~%~%" lista-aberto))
                ;(x (format t "~a~%~%~%" lista-fechado))
                (no-solucao (remove nil (mapcar #'(lambda (x) (funcall no-solucaop x)) lista-aberto))))
                (cond ((null lista-sucessores) (dfs-recursivo no-solucaop sucessores operadores profundidade-max (cdr lista-aberto)
                       (cons (car lista-aberto) lista-fechado)))
                       (t lista-sucessores)
                )
                (cond (no-solucao (car no-solucao))
                  (t (dfs-recursivo no-solucaop sucessores operadores profundidade-max 
                     (abertos-dfs (cdr lista-aberto) lista-sucessores) (cons (car lista-aberto) lista-fechado)))
                )
           )
        )
  )
)

(defun a (no-inicial no-solucaop sucessores operadores heuristica &optional (lista-aberto nil)(lista-fechado nil))
  (a-recursivo no-solucaop sucessores operadores heuristica (cons no-inicial lista-aberto) lista-fechado)
)

(defun a-recursivo (no-solucaop sucessores operadores heuristica lista-aberto lista-fechado)
  (cond ((null lista-aberto) nil)
        (t (let* ((lista-sucessores (funcall sucessores (car lista-aberto) operadores 'a* :heuristica heuristica))
                ;(x (format t "~a~%~%" lista-sucessores))
                ;(x (format t "~a~%~%" lista-aberto))
                ;(x (format t "~a~%~%~%" lista-fechado))
                (no-solucao (remove nil (mapcar #'(lambda (x) (funcall no-solucaop x)) lista-aberto))))
                (cond (no-solucao (car no-solucao))
                  (t (a-recursivo no-solucaop sucessores operadores heuristica 
                  (abertos-dfs (cdr lista-aberto) lista-sucessores) (cons (car lista-aberto) lista-fechado)))
                )
           )
        )
  )
)

(defun heuristica-1 (tabuleiro)
  "Calcula a heurística de acordo com a quantidade de movimentos possiveis no tabuleiro"
  (/ 1 (+ (movimentos-possiveis tabuleiro) 1))
)

(defun heuristica-2 (tabuleiro)
  "Calcula a heurística de acordo com a distancia de cada nó ao centro do tabuleiro, quão mais longe, pior a heurística"
  (distancia-ao-centro tabuleiro)
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