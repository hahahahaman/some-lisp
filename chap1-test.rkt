#lang racket

(require rackunit scheme/mpair "chap1.rkt")

(define (chap1-tests)
  (display "--- chap1-tests ---")
  (newline)

  ;; testing out the testing system
  (check-equal? (+ 1 1) 2 "built-in addition")
  (check-equal? (* 1 2) 2 "built-in multiplication")

  ;; (check-equal? (mlist 1 2 3 4) (mcons 1 (mcons 2 (mcons 3) (mcons 4 '())))
  ;;               "mutable list")

  (check-equal? (atom? 'a) #t)
  (check-equal? (atom? (cons 1 2)) #f)
  (check-equal? (atom? (mcons 1 2)) #f)

  (let ((li (list (list 1 2 3) (list 1 2 4) (list 'a 'b 'c)
                  (list 99 99 99) (list (list 1) (list 2))))
        (mli (mlist (mlist 1 2 3) (mlist 1 2 4) (mlist 'a 'b 'c)
                    (mlist 99 99 99) (mlist (mlist 1) (mlist 2)))))
    (check-equal? (pair->mpair li) mli "convert immutable pair to mutable")
    (check-equal? (mpair->pair mli) li "convert mutable to immutable")

    (check-equal? (mcaar mli) (pair->mpair (caar li)))
    (check-equal? (mcadr mli) (pair->mpair (cadr li)))
    (check-equal? (mcdar mli) (pair->mpair (cdar li)))
    (check-equal? (mcddr mli) (pair->mpair (cddr li)))
    (check-equal? (mcaddr mli) (pair->mpair (caddr li)))
    (check-equal? (mcadddr mli) (pair->mpair (cadddr li))))

  (check-equal? (evaluate* '(+ 1 2) *env-global*)
                (+ 1 2) "interpreted addition")

  (check-equal? (evaluate* '(* 1 2) *env-global*)
                (* 1 2) "interpreted multiplication")

  ;; definitial*
  (check-equal? (evaluate* 't *env-global*) #t)
  (check-equal? (evaluate* 'f *env-global*) *the-false-value*)
  (check-equal? (evaluate* 'nil *env-global*) '())
  (display "--- done ---")
  (newline))
