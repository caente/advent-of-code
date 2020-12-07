#lang racket/base

(require racket/file)
(require racket/list)
(require racket/match)
(require algorithms)
(require racket/string)

(define input (file->list "2020/input_day2.txt"))

(define valid_passwords (count (λ (triplet)
        (match triplet 
          [(list range letter pass)
           (let* ([r (map (λ (s) (string->number s)) (string-split (symbol->string range) "-"))]
                  [lower (car r)]
                  [upper (car (cdr r))]
                  [l (substring (symbol->string letter) 0 1) ]
                  [groups (group-by (λ (s) s) (string-split (symbol->string pass) ""))]
                  [counts (map (λ (group) (cons (car group) (length group))) groups)]
                  [letter_count (filter-map (λ (c) (and (equal? (car c) l) (cdr c) )) counts)])
             (match letter_count 
               [(list c) (and (<= lower c) (>= upper c))]
               [(list) #f]))
           ]))
     (sliding input 3 3)))

(print valid_passwords)

