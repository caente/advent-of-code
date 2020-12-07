#lang racket/base

(require racket/file)
(require racket/set)

(define input (list->set (file->list "input_day1")))

(define first (for/first ([i input]
             #:when (set-member? input (- 2020 i)))
           (* i (- 2020 i))))

#|
  - i in input
    - o = 2020 - i
    - t in input
      - r = o - t
      - if r in input: r * i * o
|#

(for*/first ([i input]
             [o input]
             [r input]
             #:when (eq? 2020 (+ i o r)))
             (* i o r))
