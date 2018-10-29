#!/bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#


(define eternity
  (lambda (x)
    (eternity x)))

(define add1
  (lambda (n)
    (+ n 1)))

(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 ((lambda (l)
                  (cond
                   ((null? l) 0)
                   (else (add1 ((lambda (l)
                                  (cond
                                   ((null? l) 0)
                                   (else (add1 (eternity (cdr l)))))) (cdr l)))))) (cdr l))))))

(lambda (length)
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (length (cdr l)))))))

((lambda (f)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (g (cdr l)))))))
  eternity))

((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
   eternity)))


(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

((Y
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))) '(o b c))
