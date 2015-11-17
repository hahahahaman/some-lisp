#lang r5rs
;; new r6rs racket uses immutable pairs, and other unknown changes

;;;; some kind of scheme like language,
;;;; whose interpreter is implemented in scheme

(define (atom? e) (not (pair? e)))

;; custom false value
;; dotted pair probably won't be confused with anything else
;; everything else is true
(define the-false-value (cons "false" "boolean"))

;; who is arsene lupin?
;; for when no other value should be returned
(define empty-begin 813)

;; i need to define wrong
;; philosophy on errors, rather than always returning some value
;; some things should return an error to point out something wrong
;; immediately, rather than letting the program continue

;; evaluate sequence in order
(define (eprogn exps env)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate (car exps) env)
                 (eprogn (cdr exps) env))
          (evaluate (car exps) env))
      empty-begin))

;; take a list of exprs
;; return the corresponding list of values of those exprs
(define (evlis exps env)
  (if (pair? exps)
      (let ((arg1 (evaluate (car exps) env)))
        ;; explicit order from left to right of list
        (cons arg1 (evlis (cdr exps) env)))
      '()))

(define (lookup id env)
  (if (pair? env)
      (if (eq? (caar env) id)
          (cdar env)
          (lookup id (cdr env)))
      (wrong "No such binding" id)))

(define (update! id env value)
  (if (pair? env)
      (if (eq? (caar env) id)
          (begin (set-mcdr! (car env) value)
                 value)
          (update! id (cdr env) value))))

(define (evaluate e env)
  (if (atom? e)
      ;; so we have an atom
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e) (boolean? e) (vector? e))
             e)
            (else (wrong "Cannot evaluate" e)))

      ;; list
      (case (car e)
        ((quote) (cadr e))
        ((if) (if (not (eq? (evaluate (cadr e) env) the-false-value))
                  (evaluate (caddr e) env)
                  (evaluate (cadddr e) env)))
        ((begin) (eprogn (cdr e) env))
        ((set!) (update! (cadr e) env (evaluate (caddr e) env)))
        ((lambda) (make-function (cadr e) (cddr e) env))
        (else (invoke (evaluate (car e) env)
                      (evlis (cdr e) env))))))
