#lang racket

(define (atom? e) (not (pair? e)))

;; custom false value
;; everything else is true
(define the-false-value (cons "false" "boolean"))

;; who is arsene lupin?
;; for when no other value should be returned
(define empty-begin 813)

(define (eprogn exps env)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate (car exps) env)
                 (eprogn (cdr exps) env))
          (evaluate (car exps) env))
      empty-begin))

(define (evlis exps env)
  (if (pair? exps)
      (cons (evaluate (car exps) env) (evlis (cdr exps) env))
      '()))

(define (evaluate e env)
  (if (atom? e)
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e)
                 (char? e) (boolean? e) (vector? e))
             e)
            (else (wrong "Cannot evaluate" e)))
      (case (car e)
        ((quote) (cadr e))
        ((if)
         ;; want to be exacting, i guess
         (if (not (eq? (evaluate (cadr e) env) the-false-value))
                  (evaluate (caddr e) env)
                  (evaluate (cadddr e) env)))
        ((begin) (eprogn (cdr e) env))
        ((set!) (update! (cadr e) env (evaluate (caddr e) env)))
        ((lambda) (make-function (cadr e) (cddr e) env))
        (else (invoke (evaluate (car e) env)
                      (evlis (cdr e) env))))))
