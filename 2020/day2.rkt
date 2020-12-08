#lang racket/base

(require racket/file)
(require racket/list)
(require racket/match)
(require algorithms)
(require racket/string)
(require racket/bool)

(define input (file->list "2020/input_day2.txt"))

(define valid_passwords_wrong (count (λ (triplet)
        (match triplet 
          [(list range letter password)
           (let* ([range (map (λ (s) (string->number s)) (string-split (symbol->string range) "-"))]
                  [lower (car range)]
                  [upper (car (cdr range))]
                  [letter (substring (symbol->string letter) 0 1) ]
                  [groups (group-by (λ (s) s) (string-split (symbol->string password) ""))]
                  [counts (map (λ (group) (cons (car group) (length group))) groups)]
                  [letter_count (filter-map (λ (c) (and (equal? (car c) letter) (cdr c) )) counts)])
             (match letter_count 
               [(list c) (and (<= lower c) (>= upper c))]
               [(list) #f]))
           ]))
     (sliding input 3 3)))

(define valid_passwords (count (λ (triplet)
        (match triplet
          [(list range letter pass)
           (let* ([range (map (λ (s) (string->number s)) (string-split (symbol->string range) "-"))]
                  [lower (car range)]
                  [upper (car (cdr range))]
                  [letter (substring (symbol->string letter) 0 1) ]
                  [password (list->vector (filter (λ (s) (not (equal? s ""))) (string-split (symbol->string pass) "")))])
             (let* ([a (vector-ref password (sub1 lower))]
                    [pass_ln (vector-length password)]
                    [b (and (<= upper pass_ln) (vector-ref password (sub1 upper)))])
               (xor (equal? a letter) (equal? b letter))))
           ]))
     (sliding input 3 3)))

(print valid_passwords)

