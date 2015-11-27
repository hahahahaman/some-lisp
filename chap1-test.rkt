#lang racket/base

(require rackunit "chap1.rkt")

(define (chap1-tests)
  (display "--- chap1-tests ---")
  (newline)

  ;; testing out the testing system
  (check-equal? (+ 1 1) 2)
  (check-equal? (* 1 2) 2)

  (check-equal? (evaluate* '(+ 1 2) *env-global*)
                (+ 1 2))
  (display "--- done ---")
  (newline))
