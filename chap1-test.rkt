#lang racket/base

(require rackunit "chap1.rkt")

(define (chap1-tests)
  (display "--- chap1-tests ---")
  (newline)
  (check-equal? (+ 1 1) 2 "add")
  (check-equal? (* 1 2) 2 "multi")
  (check-equal? (evaluate* '(+ 1 2) *env-global*)
                (+ 1 2))
  (display "--- done ---")
  (newline))
