#lang r5rs
;; new r6rs racket uses immutable pairs, and other unknown changes
;; this book was published in the 1990s

;;;; some kind of minimal scheme like language,
;;;; whose interpreter is implemented in scheme

(define (atom? e) (not (pair? e)))

;; custom false value
;; dotted pair probably won't be confused with anything else
;; everything else is true
(define *the-false-value* (cons "false" "boolean"))

;; for when no other value should be returned
(define *undef* (cons "what" "is"))

;; the environment
;; it is going to be an alist, so key-value pair, this is deep binding
;; i dont really understand shallow binding
;; lookup* search the alist trying to find the key
(define *env-init* '())

;; philosophy on errors, rather than always returning some value
;; some things should return an error to point out something wrong
;; immediately, rather than letting the program continue

;; implementation from http://srfi.schemers.org/srfi-23/srfi-23.html
(define (wrong* reason . args)
  (display "Error: ")
  (display reason)
  (for-each (lambda (arg)
              (display " ")
              (write arg))
            args)
  (newline)
  (scheme-report-environment -1))

;; evaluate sequence in order
(define (eprogn* exps env)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate* (car exps) env)
                 (eprogn* (cdr exps) env))
          (evaluate* (car exps) env))
      *undef*))

;; take a list of exprs
;; return the corresponding list of values of those exprs
(define (evlis* exps env)
  (if (pair? exps)
      (let ((arg1 (evaluate* (car exps) env)))
        ;; explicit order from left to right of list
        (cons arg1 (evlis* (cdr exps) env)))
      '()))

;;search environment for key equal to id
(define (lookup* id env)
  (if (pair? env)
      (if (eq? (caar env) id)
          (cdar env)
          (lookup* id (cdr env)))
      (wrong* "No such binding: " id)))

;; destructive change to value of variable bound in env
(define (update!* id env value)
  (if (pair? env)
      (if (eq? (caar env) id)
          (begin (set-cdr! (car env) value)
                 value)
          (update!* id (cdr env) value))
      (wrong* "No such binding: " id)))

;; binds new variables to the environment
(define (extend* vars vals env)
  (cond ((pair? vars)
         (if (pair? vals)
             (cons (cons (car vars) (car vals))
                   (extend* env (cdr vars) (cdr vals)))
             (wrong* "Not enough values")))
        ((null? vars)
         (if (null? vals)
             env
             (wrong* "Too many values")))
        ((symbol? vars) (cons (cons vars vals) env))))

(define (invoke* fn args)
  (if (procedure? fn)
      (apply fn args)
      (wrong* "Not a function" fn)))

(define (make-function* vars body env)
  (lambda (vals)
    (eprogn* body (extend* vars vals env))))

(define (evaluate* e env)
  (if (atom? e)
      ;; so we have an atom
      (cond ((symbol? e) (lookup* e env))
            ((or (number? e) (string? e) (char? e) (boolean? e) (vector? e))
             e)
            (else (wrong* "Cannot evaluate: " e)))

      ;; list
      (case (car e)
        ((quote) (cadr e))
        ((if) (if (not (eq? (evaluate* (cadr e) env) *the-false-value*))
                  (evaluate* (caddr e) env)
                  (evaluate* (cadddr e) env)))
        ((begin) (eprogn* (cdr e) env))
        ((set!) (update!* (cadr e) env (evaluate* (caddr e) env)))
        ((lambda) (make-function* (cadr e) (cddr e) env))
        (else (invoke* (evaluate* (car e) env)
                      (evlis* (cdr e) env))))))

;; --- end of interpreter

;; run code

(define *env-global* *env-init*)

;; some macros, which are hygenic? (respect lexical binding)
;; hygiene is apparently more useful in Lisp1's like scheme

;; bind new variable to global environment
(define-syntax definitial
  (syntax-rules ()
    ((definitial name)
     (begin (set! *env-global* (extend* 'name 'void *env-global*))
            'name))
    ((definitial name value)
     (begin (set! *env-global* (extend* 'name value *env-global*))
            'name))))

;; bind new function of specific arity to global env
(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name
       (lambda values
         (if (= arity (length values))
             (apply value values)
             (wrong* "Incorrect arity: " (list 'name values))))))))

;; simplified functional version of definitial
;; can be used to simply add functions into the interpreter environment
(define (def* name val)
  (set! *env-global* (extend* name val *env-global*)))

(define (comparison fn)
  (lambda vals
    (if (apply fn vals)
        #t
        *the-false-value*)))

;; dunno if this is necessary
;; but it does help readability when enclosing only r5rs scheme functions
(define (fn* fn) fn)

;; a small library
(definitial t #t)
(definitial f *the-false-value*)
(definitial nil '())

(defprimitive cons (fn* cons) 2)
(defprimitive eq? (comparison (fn* eq?)) 2)

(def* '< (comparison <))
(def* '> (comparison >))
(def* '+ (fn* +))
(def* '- (fn* -))
(def* '* (fn* *))

;; runs, but is shitty
;; if using geiser, place in .emacs file
;; (setq geiser-repl-read-knly-prompt-p nil)
;; to prevent text read only in the geiser repl buffer
;; since the (newline) messes with the next (read)
(define (repl1)
  (define (toplevel)
    (display "works?>> ")
    (let ((expr (read)))
      (if (eq? 'exit expr) ;; right now this quits when "exit" is written (no quotes)
          (begin
            (display "Death is inevitable...")
            (newline))
          (begin
            (display (evaluate* expr *env-global*))
            (newline)
            (toplevel)))))
  (toplevel))
