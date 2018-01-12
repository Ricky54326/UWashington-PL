
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (< high low)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))
  
(define (list-nth-mod xs n)
  (cond [(> 0 n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ((x (s)))
        (cons (car x) (stream-for-n-steps (cdr x) (- n 1))))))


(define funny-number-stream
  (letrec ((funny-number
         (lambda(n)
           (lambda()
             (if (= (remainder n 5) 0)
                 (cons (- 0 n) (funny-number (+ n 1)))
                 (cons n (funny-number (+ n 1))))))))
    (funny-number 1)))


(define dan-then-dog
  (lambda()
  (letrec ((dan (lambda() (cons "dan.jpg" dog)))
        (dog (lambda() (cons "dog.jpg" dan))))
    (dan))))
  