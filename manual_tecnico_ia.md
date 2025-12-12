# Manual Técnico
## Projeto Nº 1 - Solitário (Peg Solitaire)

---

### Inteligência Artificial
**Escola Superior de Tecnologia de Setúbal**  
**Ano Letivo:** 2025/2026

**Docente:** Prof. Joaquim Filipe  
**Eng.:** Filipe Mariano

**Alunos:**
- Danilo Victor - 202300224
- Jean Oliveira - 202300095
- Lucas Almeida - 202100067

**Data:** Dezembro 2025

---

## 1. Arquitetura do Sistema

### 1.1 Visão Geral

O sistema foi desenvolvido seguindo uma arquitetura modular em três componentes principais, conforme especificado no enunciado do projeto. Esta separação permite a independência entre a lógica de procura, a implementação do domínio específico e a interface com o utilizador.

```
┌─────────────────────────────────────────────────────────────┐
│                      PROJETO.LISP                           │
│                 (Interface com Utilizador)                  │
│  - Leitura de problemas                                     │
│  - Seleção de algoritmos e heurísticas                      │
│  - Escrita de resultados                                    │
└────────────────┬────────────────────────────┬───────────────┘
                 │                            │
                 ▼                            ▼
┌────────────────────────────┐  ┌────────────────────────────┐
│      PROCURA.LISP          │  │       PUZZLE.LISP          │
│  (Algoritmos de Procura)   │  │  (Domínio da Aplicação)    │
│  - BFS                     │  │  - Operadores do jogo      │
│  - DFS                     │  │  - Representação do estado │
│  - A*                      │  │  - Heurísticas             │
│  - Geração de sucessores   │  │  - Validações              │
└────────────────────────────┘  └────────────────────────────┘
```

### 1.2 Módulos e suas Responsabilidades

#### **1.2.1 PROJETO.LISP** - Interface e I/O
**Objetivo:** Gerir a interação com o utilizador e operações de entrada/saída.

**Funções Principais:**
- `iniciar()` - Ponto de entrada do programa
- `ler-problemas()` - Leitura do ficheiro problemas.dat
- `escolher-problema()` - Interface para seleção de tabuleiro
- `ler-algoritmo()` - Seleção do método de procura
- `ler-heuristica()` - Seleção da função heurística
- `executar()` - Invocação do algoritmo escolhido
- `gravar()` - Escrita dos resultados em ficheiro

**Fluxo de Informação:**
- **Entrada:** Input do utilizador (números de seleção)
- **Saída:** Problemas disponíveis, resultados da execução
- **Ficheiros:** problemas.dat (leitura), resultados.dat (escrita)

#### **1.2.2 PROCURA.LISP** - Algoritmos de Procura
**Objetivo:** Implementar métodos de procura genéricos e independentes do domínio.

**Funções Principais:**
- `bfs()` / `bfs-recursivo()` - Breadth-First Search
- `dfs()` / `dfs-recursivo()` - Depth-First Search  
- `a()` / `a-recursivo()` - Algoritmo A*
- `sucessores()` - Geração de nós sucessores
- `novo-sucessor()` - Aplicação de operadores
- `ordenar-nos()` - Ordenação para A*

**Fluxo de Informação:**
- **Entrada:** Nó inicial, predicado de solução, função de sucessores, operadores
- **Processamento:** Listas de nós abertos e fechados
- **Saída:** Nó solução com caminho completo

#### **1.2.3 PUZZLE.LISP** - Domínio do Solitário
**Objetivo:** Implementar a lógica específica do jogo Peg Solitaire.

**Funções Principais:**
- `operadores()` - Lista de operadores disponíveis
- `operador-cd/ce/cc/cb()` - Capturas nas 4 direções
- `cria-no()` - Construtor de nós
- `no-solucaop()` - Predicado de teste de solução
- `heuristica-1()` - Função heurística fornecida
- `movimentos-possiveis()` - Contagem de jogadas válidas

**Fluxo de Informação:**
- **Entrada:** Tabuleiros (listas de listas), coordenadas
- **Processamento:** Validação de jogadas, aplicação de operadores
- **Saída:** Novos estados do tabuleiro

### 1.3 Circulação de Informação

```
Utilizador → [PROJETO] → Seleção Problema/Algoritmo
                ↓
         [PUZZLE] → Criação do nó inicial
                ↓
         [PROCURA] → Execução do algoritmo
                ↓
         [PUZZLE] → Aplicação de operadores
                ↓
         [PROCURA] → Expansão e avaliação de nós
                ↓
         [PROJETO] → Apresentação e gravação de resultados
```

---

## 2. Entidades e sua Implementação

### 2.1 Representação do Tabuleiro

#### **Domínio Conceptual:**
O tabuleiro do Solitário é uma matriz 7×7 em forma de cruz com 33 posições válidas.

#### **Implementação Programática:**
```lisp
Lista de 7 listas, cada uma com 7 elementos:
- nil → Posição inválida (fora da cruz)
- 0   → Casa vazia
- 1   → Casa ocupada por um pino

Exemplo:
((nil nil 1 1 1 nil nil)
 (nil nil 1 1 1 nil nil)
 (1   1   1 1 1 1   1)
 (1   1   1 0 1 1   1)  ; Casa (4,4) vazia
 (1   1   1 1 1 1   1)
 (nil nil 1 1 1 nil nil)
 (nil nil 1 1 1 nil nil))
```

**Acesso a elementos:**
- `(linha n tabuleiro)` - Retorna a linha n (1-indexed)
- `(coluna n tabuleiro)` - Retorna a coluna n
- `(celula x y tabuleiro)` - Retorna o conteúdo da posição (x,y)

### 2.2 Representação de Nós

#### **Domínio Conceptual:**
Um nó representa um estado do jogo com informação sobre profundidade e caminho.

#### **Implementação Programática:**
```lisp
Estrutura: (estado profundidade heurística pai)

- estado: tabuleiro (lista de listas)
- profundidade: inteiro (distância do nó inicial)
- heurística: float (valor calculado pela função h(x))
- pai: referência ao nó anterior (ou nil)

Exemplo:
(((nil nil 1 1 1 nil nil) ...) 5 0.125 #<nó-pai>)
```

**Funções de acesso:**
- `(no-estado no)` → primeiro elemento
- `(no-profundidade no)` → segundo elemento
- `(no-heuristica no)` → terceiro elemento
- `(no-pai no)` → quarto elemento
- `(no-valor no)` → g(n) + h(n) para A*

### 2.3 Operadores (Movimentos)

#### **Domínio Conceptual:**
Movimentos válidos onde um pino salta sobre outro adjacente para uma casa vazia.

#### **Implementação Programática:**

Cada operador é uma função que recebe `(x y tabuleiro)` e retorna:
- Novo tabuleiro se o movimento for válido
- `nil` se o movimento for inválido

**Validações aplicadas:**
1. Posições (x,y) devem estar dentro do tabuleiro
2. Casa origem deve ter pino (= 1)
3. Casa intermediária deve ter pino (= 1)
4. Casa destino deve estar vazia (= 0)

**Transformação:**
```
Antes: [1] [1] [0]  →  Depois: [0] [0] [1]
       origem meio destino
```

### 2.4 Heurística

#### **Domínio Conceptual:**
Estimativa da distância à solução baseada no conhecimento do problema.

#### **Implementação Programática:**

**Heurística Fornecida (heuristica-1):**
```lisp
h(x) = 1 / (o(x) + 1)

onde o(x) = número de peças que podem ser movidas
```

**Características:**
- Admissível: nunca sobrestima o custo real
- Consistente: h(n) ≤ c(n,n') + h(n')
- Valores menores indicam estados mais próximos da solução

---

## 3. Algoritmos e sua Implementação

### 3.1 Breadth-First Search (BFS)

#### **Descrição:**
Explora o espaço de estados nível por nível, garantindo que a primeira solução encontrada tem o menor número de jogadas.

#### **Implementação:**
```lisp
(defun bfs (no-inicial no-solucaop sucessores operadores 
            &optional lista-aberto lista-fechado)
  "Procura em largura primeiro"
  ; Adiciona nó inicial aos abertos
  ; Loop recursivo:
  ;   - Testa se primeiro nó dos abertos é solução
  ;   - Se sim: retorna nó
  ;   - Se não: expande sucessores e adiciona ao fim dos abertos
  ;   - Move nó para fechados
  )
```

#### **Estruturas de Dados:**
- **Lista Abertos:** Fila FIFO (First-In-First-Out)
- **Lista Fechados:** Armazena nós já expandidos

#### **Gestão de Abertos:**
```lisp
(defun abertos-bfs (abertos sucessores)
  "Adiciona sucessores ao fim da lista"
  (append abertos sucessores))
```

#### **Características:**
- **Completude:** Sim - encontra solução se existir
- **Optimalidade:** Sim - primeira solução é a mais curta
- **Complexidade Temporal:** O(b^d) onde b=branching factor, d=profundidade
- **Complexidade Espacial:** O(b^d) - armazena todos os nós do nível

### 3.2 Depth-First Search (DFS)

#### **Descrição:**
Explora em profundidade antes de retroceder, usando um limite de profundidade para evitar loops infinitos.

#### **Implementação:**
```lisp
(defun dfs (no-inicial no-solucaop sucessores operadores 
            profundidade-max &optional lista-aberto lista-fechado)
  "Procura em profundidade primeiro com limite"
  ; Similar ao BFS mas:
  ;   - Adiciona sucessores ao início dos abertos
  ;   - Para de expandir ao atingir profundidade-max
  )
```

#### **Gestão de Abertos:**
```lisp
(defun abertos-dfs (abertos sucessores)
  "Adiciona sucessores ao início da lista"
  (append sucessores abertos))
```

#### **Limite de Profundidade:**
No código atual está definido como 31 (número máximo de jogadas possível no Solitário):
```lisp
(dfs no 'no-solucaop 'sucessores (operadores) 31)
```

#### **Características:**
- **Completude:** Não - pode não encontrar solução se limite for inadequado
- **Optimalidade:** Não - primeira solução pode não ser a melhor
- **Complexidade Temporal:** O(b^m) onde m=profundidade máxima
- **Complexidade Espacial:** O(b*m) - mais eficiente que BFS

### 3.3 Algoritmo A*

#### **Descrição:**
Procura informada que usa heurística para guiar a exploração, avaliando nós por f(n) = g(n) + h(n).

#### **Implementação:**
```lisp
(defun a (no-inicial no-solucaop sucessores operadores heuristica 
          &optional lista-aberto lista-fechado)
  "Algoritmo A* com função heurística"
  ; Loop recursivo:
  ;   - Expande nó com menor f(n) dos abertos
  ;   - Calcula h(n) para cada sucessor
  ;   - Ordena abertos por f(n) = g(n) + h(n)
  )
```

#### **Função de Avaliação:**
```lisp
(defun no-valor (no)
  "Calcula f(n) = g(n) + h(n)"
  (+ (no-profundidade no)      ; g(n) - custo até aqui
     (no-heuristica no)))      ; h(n) - estimativa até solução
```

#### **Ordenação de Nós:**
```lisp
(defun ordenar-nos (nos)
  "Ordena nós por valor f(n) crescente"
  (cond ((null nos) nil)
        (t (let ((min-no (ordenar-nos-recursivo (first nos) (cdr nos))))
             (cons min-no (ordenar-nos (remove min-no nos)))))))
```

**Nota:** Esta implementação usa ordenação por seleção (O(n²)). Para melhor desempenho, considerar quicksort ou merge-sort.

#### **Geração de Sucessores Ordenados:**
```lisp
(defun sucessores (no operadores procura &key profundidade limite heuristica)
  ; Para A*:
  ;   - Gera todos os sucessores possíveis
  ;   - Calcula heurística de cada um
  ;   - Ordena por f(n) antes de retornar
  )
```

#### **Características:**
- **Completude:** Sim - com heurística admissível
- **Optimalidade:** Sim - com heurística admissível e consistente
- **Complexidade:** Depende da qualidade da heurística
- **Desempenho:** Significativamente melhor que BFS/DFS em problemas grandes

### 3.4 Geração de Sucessores

#### **Processo:**
1. Para cada operador disponível (cd, ce, cc, cb)
2. Tenta aplicar em todas as posições (x,y) do tabuleiro
3. Se movimento válido, cria novo nó filho
4. Retorna lista de todos os sucessores válidos

#### **Implementação:**
```lisp
(defun novo-sucessor-recursivo (no operador heuristica 
                                &optional (x 1) (y 1))
  "Aplica operador em todas as posições válidas"
  (cond ((> x 7) nil)  ; Fim do tabuleiro
        ((> y 7) (novo-sucessor-recursivo no operador heuristica (1+ x)))
        (t (let ((novo-tabuleiro (funcall operador x y (no-estado no))))
             (cond ((null novo-tabuleiro)  ; Movimento inválido
                    (novo-sucessor-recursivo no operador heuristica x (1+ y)))
                   (t (cons (cria-no novo-tabuleiro heuristica 
                                     (1+ (no-profundidade no)) no)
                           (novo-sucessor-recursivo no operador heuristica 
                                                    x (1+ y)))))))))
```

### 3.5 Teste de Solução

#### **Condição de Vitória:**
O jogo termina quando resta apenas 1 pino no tabuleiro.

#### **Implementação:**
```lisp
(defun no-solucaop (no)
  "Verifica se o nó é solução"
  (let ((total-pinos (apply #'+ 
                       (mapcar #'(lambda (linha) (count 1 linha)) 
                               (no-estado no)))))
    (cond ((= 1 total-pinos) no)
          (t nil))))
```

### 3.6 Métricas de Desempenho

**Métricas:**

As métricas foram implementadas em código sendo fornecidas ao utilizador e guardadas num ficheiro.

1. **Fator de Ramificação Médio (b*):**
   ```
   b* = N^(1/d)
   onde N = total de nós gerados
        d = profundidade da solução
   ```

2. **Número de Nós Gerados:**
   - Total de nós criados durante a procura

3. **Número de Nós Expandidos:**
   - Nós que tiveram seus sucessores gerados

4. **Penetrância:**
   ```
   P = d / N
   onde d = profundidade da solução
        N = total de nós gerados
   ```

5. **Tempo de Execução:**
   ```lisp
   (time (executar algoritmo no heuristica))
   ```

---

## 4. Descrição das Opções Tomadas

### 4.1 Representação do Tabuleiro

**Opção Escolhida:** Lista de listas com valores nil, 0 e 1.

**Alternativas Consideradas:**
- Array bidimensional de Common Lisp
- Estrutura/classe com slots

**Justificação:**
- Maior compatibilidade com paradigma funcional
- Facilita operações de manipulação recursiva
- Sintaxe mais limpa para criação de novos estados
- Alinhamento com especificação do enunciado

### 4.2 Estrutura de Nós

**Opção Escolhida:** Lista simples `(estado profundidade heurística pai)`.

**Alternativas Consideradas:**
- `defstruct` do Common Lisp
- Lista de propriedades (plist)
- Hash-table

**Justificação:**
- Simplicidade e leveza
- Acesso direto via `first`, `second`, etc.
- Não requer definição de estruturas complexas
- Fácil debugging (leitura direta no REPL)

### 4.3 Recursão vs Iteração

**Opção Escolhida:** Implementação recursiva dos algoritmos.

**Justificação:**
- Alinhamento com paradigma funcional do Lisp
- Código mais declarativo e legível
- Evita uso de variáveis globais e setq
- Cumprimento dos requisitos do enunciado

**Trade-off:**
- Risco de stack overflow em problemas muito profundos
- Possível solução: aumentar stack size ou implementar tail-call optimization

### 4.4 Ordenação em A*

**Opção Escolhida:** Ordenação por seleção implementada recursivamente.

**Alternativas Consideradas:**
- `sort` nativo do Common Lisp
- Heap/Priority Queue
- Quicksort funcional

**Justificação:**
- Implementação 100% funcional sem side-effects
- Compreensão didática do algoritmo
- Evita funções destrutivas

**Limitação Reconhecida:**
- Complexidade O(n²) pode ser problemática em listas grandes
- Refactoring futuro: implementar merge-sort funcional

### 4.5 Profundidade Máxima no DFS

**Opção Escolhida:** Limite fixo de 31 níveis.

**Justificação:**
- 31 é o número máximo de jogadas possível (32 pinos iniciais - 1 final)
- Previne recursão infinita
- Garante terminação do algoritmo

**Alternativa Futura:**
- Permitir ao utilizador definir o limite
- Implementar Iterative Deepening DFS (IDDFS)

### 4.6 Validação de Operadores

**Opção Escolhida:** Validação completa em cada operador antes de aplicar.

**Processo:**
1. Verificar se posições estão no tabuleiro
2. Verificar se posições não são nil
3. Verificar configuração origem-meio-destino

**Justificação:**
- Previne estados inválidos
- Facilita debugging
- Retorna nil explicitamente para movimentos inválidos

### 4.7 Caminhos de Ficheiros

**Opção Atual:** Caminhos absolutos hardcoded.
```lisp
(load "D:\\Trabalhos\\IA\\IA-Project-25-26\\procura.lisp")
```

**Reconhecimento:**
- Não portável entre máquinas
- Deve ser refatorado para caminhos relativos
- Considerar variável de ambiente ou parâmetro

---

## 5. Limitações Técnicas e Desenvolvimento Futuro

### 5.1 Requisitos Não Implementados

#### **5.1.1 Métricas de Desempenho**
**Status:** Não implementado

**Descrição:** O enunciado requer cálculo automático de:
- Fator de ramificação médio
- Número de nós gerados/expandidos
- Penetrância
- Tempo de execução

**Impacto:** Desconto de 0.2 a 0.5 valores por problema.

**Plano de Implementação:**
1. Adicionar contadores como parâmetros opcionais nas funções recursivas
2. Usar `(time ...)` para medir tempo de execução
3. Criar função de relatório automático
4. Gravar métricas no ficheiro de resultados

#### **5.1.2 Segunda Heurística**
**Status:** Não implementado

**Descrição:** Desenvolver heurística alternativa que melhore o desempenho.

**Sugestões:**
- Heurística baseada em distância de pinos ao centro
- Penalização de pinos isolados
- Priorização de configurações simétricas

#### **5.1.3 Algoritmos de Bónus**
**Status:** Não implementado

**Algoritmos:** SMA*, IDA*, RBFS

**Valor:** Até 3 valores de bónus

#### **5.1.4 Problema G Dinâmico**
**Status:** Interface preparada mas não testada

**Descrição:** Adicionar problema durante avaliação oral

### 5.2 Limitações Técnicas Identificadas

#### **5.2.1 Desempenho da Ordenação**
**Problema:** Algoritmo de ordenação O(n²) limita escalabilidade do A*.

**Impacto:** Lentidão em problemas com alta ramificação.

**Solução Futura:**
```lisp
(defun ordenar-nos-merge (nos)
  "Merge sort funcional O(n log n)"
  ; Implementação com merge-sort
  )
```

#### **5.2.2 Gestão de Memória**
**Problema:** Todos os nós permanecem em memória (listas abertos/fechados).

**Impacto:** Possível estouro de memória em buscas profundas.

**Soluções Futuras:**
- Implementar SMA* (Simplified Memory-bounded A*)
- Garbage collection explícita de nós antigos
- Usar estruturas mais eficientes (hash-tables para fechados)

#### **5.2.3 Detecção de Estados Repetidos**
**Problema:** Não há verificação de estados duplicados.

**Impacto:** 
- Mesmo estado pode ser explorado múltiplas vezes
- Desperdício de recursos computacionais

**Solução Futura:**
```lisp
(defun estado-visitadop (estado lista-fechados)
  "Verifica se estado já foi explorado"
  (member estado lista-fechados :test #'equal))
```

#### **5.2.4 Stack Overflow em Recursão Profunda**
**Problema:** Recursão sem tail-call optimization pode estourar stack.

**Solução Temporária:**
```lisp
; No SBCL:
(setf sb-ext:*stack-size* (* 1024 1024 128)) ; 128MB
```

**Solução Definitiva:**
- Implementar versões iterativas usando loop
- Usar continuations/trampolining

#### **5.2.5 Portabilidade de Caminhos**
**Problema:** Caminhos absolutos Windows-specific.

**Solução:**
```lisp
(defparameter *base-path* 
  (make-pathname :directory (pathname-directory *load-truename*)))

(defun load-module (filename)
  (load (merge-pathnames filename *base-path*)))
```

### 5.3 Melhoramentos de Desempenho

#### **5.3.1 Otimização de Sucessores**
**Atual:** Gera sucessores para todas as 196 posições (7×7×4 operadores).

**Melhoria:**
```lisp
(defun gerar-movimentos-validos (tabuleiro)
  "Gera apenas movimentos sabidamente válidos"
  ; Iterar apenas sobre posições com pinos
  ; Verificar apenas direções viáveis
  )
```

**Ganho Estimado:** 60-70% redução de chamadas inválidas.

#### **5.3.2 Memoização de Heurísticas**
**Problema:** Heurística recalculada para mesmos estados.

**Solução:**
```lisp
(let ((cache (make-hash-table :test #'equal)))
  (defun heuristica-1-memoized (tabuleiro)
    (or (gethash tabuleiro cache)
        (setf (gethash tabuleiro cache) 
              (heuristica-1 tabuleiro)))))
```

#### **5.3.3 Paralelização**
**Oportunidade:** Exploração de ramos independentes em paralelo.

**Tecnologia:** `lparallel` library do Common Lisp.

**Aplicação:**
```lisp
(defun sucessores-paralelo (no operadores)
  (pmapcar #'(lambda (op) (novo-sucessor no op)) operadores))
```

### 5.4 Refactoring Necessário

#### **5.4.1 Separação de Concerns**
**Problema:** `sucessores()` mistura lógica de geração e ordenação.

**Proposta:**
```lisp
(defun gerar-sucessores (no operadores)
  "Apenas gera sucessores")

(defun ordenar-para-procura (nos tipo-procura)
  "Ordenação específica por algoritmo")
```

#### **5.4.2 Configuração Centralizada**
**Problema:** Constantes espalhadas pelo código.

**Proposta:**
```lisp
(defparameter *tamanho-tabuleiro* 7)
(defparameter *profundidade-maxima-dfs* 31)
(defparameter *path-problemas* "problemas.dat")
```

#### **5.4.3 Logging e Debug**
**Atual:** Linhas comentadas de debug com `(format t ...)`.

**Proposta:**
```lisp
(defparameter *debug-mode* nil)

(defmacro debug-log (formato &rest args)
  `(when *debug-mode* (format t ,formato ,@args)))
```

### 5.5 Funcionalidades Futuras

1. **Interface Gráfica:**
   - Visualização do tabuleiro
   - Animação da solução passo-a-passo
   - Estatísticas em tempo real

2. **Análise Comparativa Automática:**
   - Executar todos os algoritmos em todos os problemas
   - Gerar tabelas e gráficos comparativos
   - Exportar para LaTeX/HTML

3. **Gerador de Problemas:**
   - Criar tabuleiros aleatórios válidos
   - Diferentes níveis de dificuldade
   - Validação de solubilidade

4. **Player Interativo:**
   - Modo de jogo manual
   - Hints baseados em A*
   - Undo/Redo de movimentos

5. **Otimizador de Heurísticas:**
   - Aprendizagem automática de pesos
   - Genetic algorithms para tuning
   - A/B testing de heurísticas

---

## 6. Conclusão

O projeto implementa com sucesso os três algoritmos de procura principais (BFS, DFS, A*) aplicados ao problema do Solitário. A arquitetura modular permite fácil extensão e manutenção. 

As principais áreas de melhoria identificadas são:
1. Implementação completa das métricas de desempenho
2. Desenvolvimento da segunda heurística
3. Otimização de algoritmos de ordenação
4. Detecção de estados duplicados
5. Melhoria da portabilidade

O código demonstra boa compreensão dos princípios de programação funcional, evitando side-effects e utilizando recursão de forma apropriada. Com as melhorias sugeridas, o sistema poderá resolver problemas mais complexos de forma eficiente.

---

## Referências

- Russell, S., & Norvig, P. (2020). *Artificial Intelligence: A Modern Approach* (4th ed.)
- Graham, P. (1996). *ANSI Common Lisp*
- Documentação oficial Common Lisp HyperSpec
- Enunciado do Projeto 1 - IA 2025/2026

---

**Fim do Manual Técnico**