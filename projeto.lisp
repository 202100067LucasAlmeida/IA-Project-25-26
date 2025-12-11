#|
# Projeto 1 - InteligÃªncia Artificial
#
# Ficheiro destinado a implementar a interação com utilizador, ler e gravar ficheiro, carregar outros ficheiros do projeto
#
# Docente > Joaquim Filipe
#
# Alunos:
# > Danilo Victor, 202300224
# > Jean Oliveira, 202300095
# > Lucas Almeida, 202100067
#
|#

(load "D:\\Trabalhos\\IA\\IA-Project-25-26\\procura.lisp")
(load "D:\\Trabalhos\\IA\\IA-Project-25-26\\puzzle.lisp")

;;;;Função Inicial

(defun iniciar()
  (let* ((problema (escolher-problema))
         (algoritmo (ler-algoritmo))
         (heuristica (if (equal algoritmo 'a-estrela) (ler-heuristica)))
         (no (cria-no problema))
         (resultado (executar algoritmo no heuristica)))
    (mostrar-resultado resultado)
    (decidir-gravar resultado)))

;;Leitura ficheiros

(defun ler-problemas()
  (with-open-file (f "D:\\Trabalhos\\IA\\IA-Project-25-26\\problemas.dat" :direction :input)
    (labels ((ler-rec (acc)
               (let ((p (read f nil nil)))
                 (if p (ler-rec (cons p acc))
                   (reverse acc)))))
      (ler-rec nil))
    )
)

(defun mostrar-problemas-rec (lst n)
  (cond
   ((null lst) nil)
   (t (format t "~A - ~A~%" n (car lst))
      (mostrar-problemas-rec (cdr lst) (+ n 1))))
  )

(defun escolher-problema()
  (format t "~%Problemas disponíveis:~%")
  (mostrar-problemas-rec (ler-problemas) 1)
  (format t "~%Escolha o nº do problema: ")
  (let* ((n (read))
         (lista (ler-problemas)))
    (nth (1- n) lista))
  )

;;; Leitura de algoritmo

(defun ler-algoritmo ()
  (format t "~%Escolha o algoritmo:~%")
  (format t "1 - BFS~%2 - DFS~%3 - A*~%")
  (format t "> ")
  (case (read)
    (1 'bfs)
    (2 'dfs)
    (3 'a-estrela)
    (otherwise (ler-algoritmo)))
)

(defun ler-heuristica ()
  (format t "~%Escolha a heuristica:~%")
  (format t "1 - opção 1~%2 - Opçao 2~%")
  (case (read)
    (1 'heuristica-1)
    (2 'heuristica-2)
    (otherwise (ler-heuristica)))
  )

(defun executar (algoritmo no heuristica)
  (cond ((equal algoritmo 'bfs) (bfs no 'no-solucaop 'sucessores (operadores)))
        ((equal algoritmo 'dfs) (dfs no 'no-solucaop 'sucessores (operadores) 9999))
        ((equal algoritmo 'a-estrela) (a-estrela no 'no-solucaop 'sucessores (operadores) heuristica))
        (t (format t "Error"))
   )
)

;;; Ficheiros e resultados

(defun mostrar-resultado (res)
  (format t "~%Resultado: ~A~%" res)
)

(defun decidir-gravar (resultado)
  (format t "Guardar Resultado? (s/n): ")
  (case (read)
    ((s S) (gravar resultado))
    (otherwise (format t "~%Ok, não será gravado.~%")))
)

(defun gravar (resultado)
  (with-open-file (f "D:\\Trabalhos\\IA\\IA-Project-25-26\\resultados.dat" :direction :output :if-exists :append :if-does-not-exist :create)
   (format f "~%----------------------------------~%")
   (format f "~A~%" resultado)
   (format f "----------------------------------~%")
   (format t "~%Resultado guardado.~%")
  )
)
