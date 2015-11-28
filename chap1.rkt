#lang racket
(require scheme/mpair)
(provide (all-defined-out))
;; new r6rs racket uses immutable pairs, and other unknown changes
;; this book was published in the 1990s
;; mutable pairs in seperate library scheme/mpair
;; the environment of the new scheme will be mutable
;; while the expressions passed into the interpreter will be immutable

(define ns (make-base-namespace))

;;;; some kind of minimal scheme like language,
;;;; whose interpreter is implemented in scheme

(define (mcaar e) (mcar (mcar e)))
(define (mcadr e) (mcar (mcdr e)))
(define (mcdar e) (mcdr (mcar e)))
(define (mcddr e) (mcdr (mcdr e)))
(define (mcaddr e) (mcar (mcdr (mcdr e))))
(define (mcadddr e) (mcar (mcdr (mcdr (mcdr e)))))

(define (atom? e)
  (not (or (pair? e)
           (mpair? e))))

;; (define (mlist . args)
;;   (if (null? args)
;;       '()
;;       (mcons (car args) (mlist (cdr args)))))

(define (pair->mpair p)
  (if (pair? p)
      (mcons (if (pair? (car p))
                (pair->mpair (car p))
                (car p))
            (pair->mpair (cdr p)))
      p))

(define (mpair->pair p)
  (if (mpair? p)
      (cons (if (mpair? (mcar p))
                (mpair->pair (mcar p))
                (mcar p))
            (mpair->pair (mcdr p)))
      p))

;; custom false value
;; dotted pair probably won't be confused with anything else
;; everything else is true
(define *the-false-value* (cons "false" "boolean"))

;; for when no other value should be returned
(define *undef* (mcons "what" "is"))

;; the environment
;; it is going to be an alist, so key-value pair, this is deep binding
;; i dont really understand shallow binding
;; lookup* search the alist trying to find the key
(define *env-init* '())

;; philosophy on errors, rather than always returning some value
;; some things should return an error to point out something wrong
;; immediately, rather than letting the program continue

;; implementation from http://srfi.schemers.org/srfi-23/srfi-23.html
;; (define (wrong* reason . args)
;;   (display "Error: ")
;;   (display reason)
;;   (for-each (lambda (arg)
;;               (display " ")
;;               (write arg))
;;             args)
;;   (newline)
;;   ;; bleh, returns a second error message
;;   (scheme-report-environment -1))

(define (wrong* . args)
  (apply error args))

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
;; mapmcar
(define (evlis* exps env)
  (if (pair? exps)
      (cons (evaluate* (car exps) env)
            (if (pair? (cdr exps))
                (evlis* (cdr exps) env)
                '()))
      '()))

;;search environment for key equal to id
(define (lookup* id env)
  (if (atom? env)
      (wrong* "No such binding:" id)
      (let ((env (mpair->pair env)))
        (if (eq? (caar env) id)
           (cdar env)
           (lookup* id (cdr env))))))

;; destructive change to value of variable bound in env
(define (update!* id value env)
  (if (mpair? env)
      (if (eq? (mcaar env) id)
          (begin (set-mcdr! (mcar env) value)
                 value)
          (update!* id value (mcdr env)))
      (wrong* "No such binding: " id)))

;; binds new variables to the environment
(define (extend* vars vals env)
  (cond ((pair? vars)
         (if (pair? vals)
             (mcons (mcons (car vars) (car vals))
                    (extend* (cdr vars) (cdr vals) env))
             (wrong* "Not enough values:" vals)))
        ((null? vars)
         (if (null? vals)
             env
             (wrong* "Too many values:" vals)))
        ((symbol? vars) (mcons (mcons vars vals) env))))

(define (invoke* fn args)
  (if (procedure? fn)
      (let ((result (apply fn args)))
        (display "tracer: ")
        (display args)
        (display " => ")
        (display result)
        (newline)
        result)
      (wrong* "Not a function:" fn)))

(define (make-function* vars body env)
  (lambda vals
    (eprogn* body (extend* vars vals env))))

(define (evaluate* e env)
  ;; e is either atomic or a list
  (if (atom? e)
      (cond ((symbol? e) (lookup* e env))
            ((or (number? e) (string? e) (char? e) (boolean? e) (vector? e))
             e)
            (else (wrong* "Cannot evaluate:" e)))
      (case (car e)
        ((quote) (cadr e))
        ((if) (if (not (eq? (evaluate* (cadr e) env) *the-false-value*))
                  (evaluate* (caddr e) env)
                  (evaluate* (cadddr e) env)))
        ((begin) (eprogn* (cdr e) env))
        ((set!) (update!* (cadr e) (evaluate* (caddr e) env) env))
        ((lambda) (make-function* (cadr e) (cddr e) env))
        ((eval) (evaluate* (cadr e) env))
        (else (invoke* (evaluate* (car e) env)
                       (evlis* (cdr e) env))))))

;; --- end of interpreter

;; run code

(define *env-global* *env-init*)

;; some macros, which are hygenic? (respect lexical binding)
;; hygiene is apparently more useful in Lisp1's like scheme

;; bind new variable to global environment
(define-syntax definitial*
  (syntax-rules ()
    ((definitial* name)
     (begin (set! *env-global* (extend* 'name 'void *env-global*))
            'name))
    ((definitial* name value)
     (begin (set! *env-global* (extend* 'name value *env-global*))
            'name))))

;; bind new function of specific arity to global env
(define-syntax defprimitive*
  (syntax-rules ()
    ((defprimitive* name value arity)
     (definitial* name
       (lambda values
         (if (= arity (length values))
             (apply value values)
             (wrong* "Incorrect arity:" (list 'name values))))))))

(define-syntax defpredicate*
  (syntax-rules ()
    ((defpredicate* name value arity)
     (defprimitive* name
       (lambda values (or (apply value values) *the-false-value*))
       arity))))

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
(definitial* t #t)
(definitial* f *the-false-value*)
(definitial* nil '())

(defprimitive* mcons (fn* mcons) 2)

(defpredicate* eq? (fn* eq?) 2)

(def* '< (comparison <))
(def* '> (comparison >))
(def* '+ (fn* +))
(def* '- (fn* -))
(def* '* (fn* *))
(def* 'call/cc (fn* call-with-current-continuation))

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

;; from the book
(define (chapter1-scheme)
  (define (toplevel)
    (display (evaluate* (read) *env-global*))
    (toplevel))
  (toplevel))

;; 1.1

;; (define (invoke* fn args)
;;   (if (procedure? fn)
;;       (let ((result (apply fn args)))
;;         (display "tracer: ")
;;         (display args)
;;         (display " => ")
;;         (display result)
;;         (newline))
;;       (wrong* "Not a function" fn)))

;; 1.2
;; inline code of another evlis call inside evlis

;; 1.3

;; why would I do this?
;; unbounded names can't be found till lookup is called
;; setting a new value is weird
;; much prefer something simpler with more restrictions, like:
;; (define (extend13 name value env)
;;   (if (mpair? name)
;;       (wrong* "NAME must be atomic: " name)
;;       (mcons (mcons name value) env)))

;; ribcage shape
(define *env13* (mlist (mcons (mlist 'a 'b 'c) (mlist 1 2 3))))

(define (extend13 names values env)
  (mcons (mcons (pair->mpair names) (pair->mpair values)) env))

(define (lookup13 id env)
  (define (lookthrough names vals)
    (let ((names (mpair->pair names))
          (vals (mpair->pair vals)))
      (if (pair? names)
         (if (eq? (car names) id)
             (if (pair? vals)
                 (car vals)
                 (wrong* "No value bound to:" id))
             *undef*)
         (if (eq? names id)
             vals
             *undef*))))
  (if (mpair? env)
      (if (mpair? (mcaar env)) ;; names is a list
          (let ((return-value (lookthrough (mcaar env) (mcdar env))))
            (if (eq? return-value *undef*)
                (lookup13 id (mcdr env))
                return-value))
          (cdar env))
      (wrong* "No such binding:" id)))

(define (update!13 id value env)
  (define (lookthrough names vals)
    (if (mpair? names)
        (if (eq? (mcar names) id)
            (if (mpair? vals)
                (begin (set-mcar! vals value)
                       value)
                (wrong* "No binding to: " id))
            *undef*)
        (if (eq? names id)
            'set
            *undef*)))
  (if (mpair? env)
      (let ((return-value (lookthrough (mcaar env) (mcdar env))))
        (if (eq? return-value *undef*)
            (update!13 id value (mcdr env))
            (if (eq? return-value 'set)
                (begin (set-mcdr! (mcar env) value) value)
                value)))
      (wrong* "No such binding:" id)))
;; 1.4

;; 1.5
;; defprimitive
;; of def*


;; 1.6
(definitial* list (lambda values values))

;; 1.7

;; (define (call/cc* fn))

;; 1.8

;; 1.9 inside repl1

;; 1.10
;; uhhh it's faster?
(define (test-speed)
  (display "(eval) ")
  (newline)
  (time (display (eval (if (> 122412 123421) 1 2)
                       ns))
        (newline))
  (newline)

  (display "(eval (evualate*)) ")
  (newline)
  (time (display (eval (evaluate* (if (> 122412 123421) 1 2)
                                  *env-global*)
                       ns))
        (newline))
  (newline)

  (display "(eval (evalutate* (evaluate*))) ")
  (newline)
  (time (display (eval (evaluate*
                        (evaluate* (if (> 122412 123421) 1 2)
                                   *env-global*)
                        *env-global*)
                       ns))
        (newline)))

