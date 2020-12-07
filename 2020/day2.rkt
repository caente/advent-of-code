#lang racket/base

(require racket/file)
(require racket/list)
(require racket/match)
(require algorithms)
(require racket/string)

(define input (file->list "2020/input_day2.txt"))

(define valid_passwords (count (λ (triplet)
        (match triplet 
          [(list range letterRAW pass)
           (let* ([range (map (λ (s) (string->number s)) (string-split (symbol->string range) "-"))]
                  [lower (car range)]
                  [upper (car (cdr range))]
                  [letter (substring (symbol->string letterRAW) 0 1) ]
                  [groups (group-by (λ (s) s) (string-split (symbol->string pass) ""))]
                  [counts (map (λ (group) (cons (car group) (length group))) groups)]
                  [letter_count (filter-map (λ (c) (and (equal? (car c) letter) (cdr c) )) counts)])
             (match letter_count 
               [(list c) (and (<= lower c) (>= upper c))]
               [(list) #f]))
           ]))
     (sliding input 3 3)))

(print valid_passwords)

