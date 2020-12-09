#lang racket/base

(require racket/file)
(require racket/list)
(require racket/string)
(require racket/match)
(require racket/vector)

(define input (map (λ (line) (list->vector (filter (λ (s) (not (equal? s ""))) (string-split line ""))))
                   (file->lines "2020/input_day3.txt")))
(println (string-join (vector->list (car input))))
(foldl 
  (λ (v acc) (match acc [(cons column trees)
                               (let*
                                 ([next_move (+ column 3)]
                                  [new_column (if (>= next_move (vector-length v)) (- next_move (vector-length v)) next_move )]
                                  [value (vector-ref v new_column)]
                                  [c (or (and (equal? value "#") "X") "O")]
                                  [new_trees (if (equal? c "X") (add1 trees) trees)])
                                (vector-set! v new_column c)
                                (println (string-join (vector->list v)))
                                (cons new_column new_trees))]))
  (cons 0 0)
  (cdr input))
