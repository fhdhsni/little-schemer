#!/bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

;;* first
(define first (lambda (x) (car x)))

;;* second
(define second (lambda (x) (car (cdr x))))

;;* third
(define third (lambda (l) (car (cdr (cdr l)))))

;;* build
(define build (lambda (x1 x2) (cons x1 (cons x2 '()))))

;; An entry is a pair of lists whose first list is a
;; set. Also, the two lists must be of equal
;; length.
;;* new-entry
(define new-entry build)

;;* lookup-in-entry
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

;;* lookup-in-entry-help
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond ((null? names) (entry-f name))
          ((eq? (car names) name)
           (car values))
          (else (lookup-in-entry-help name
                                      (cdr names)
                                      (cdr values)
                                      entry-f)))))

;; A table (also called an environment) is a list of entries.
;;* extend-table
(define extend-table cons)

;;* lookup-in-table
(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else (lookup-in-entry name
                            (car table)
                            (lambda (x)
                              (lookup-in-table name (cdr table) table-f)))))))
;;* atom-to-action
(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e (quote cons)) *const)
     ((eq? e (quote car)) *const)
     ((eq? e (quote cdr)) *const)
     ((eq? e (quote null?)) *const)
     ((eq? e (quote eq?)) *const)
     ((eq? e (quote atom?)) *const)
     ((eq? e (quote zero?)) *const)
     ((eq? e (quote add1)) *const)
     ((eq? e (quote sub1)) *const)
     ((eq? e (quote number?)) *const)
     (else *identifier))))

;;* list-to-action
(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
     (else *application))))


;;* expression-to-action
(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))


;; (value e)
;; e is car => (primitive car)
;;
;;* value
(define value
  (lambda (e)
    (meaning e (quote ()))))

;;* meaning
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;;* *const
(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build 'primitive e)))))

;;* *quote
(define *quote
  (lambda (e table)
    (text-of e)))

;;* text-of
(define text-of second)

;;* *identifier
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

;; it throws
;;*initial-table
(define initial-table
  (lambda (name)
    (car (quote ()))))

;;**lambda
(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

;; example of *lambda
;; (meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9))))
;; evaluates to =>
;; (non-primitive
;;  ((((y z) ((8) 9)))
;;   (x)
;;   (cons x y)))

;;** table-of
(define table-of first)

;;** formals-of
(define formals-of second)

;;** body-of
(define body-of third)

;;** evcon
(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines))
               table))
     ((meaning (question-of (car lines))
               table)
      (meaning (answer-of (car lines))
               table))
     (else (evcon (cdr lines) table)))))

;;** else?
(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x 'else))
     (else #f))))

;;** question-of
(define question-of first)

;;** answer-of
(define answer-of second)

;;** *cond
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

;;** cond-lines-of
(define cond-lines-of cdr)

;;** evlis
(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else
      (cons (meaning (car args) table)
            (evlis (cdr args) table))))))

;;** function-of
(define function-of car)

;;** arguments-of
(define arguments-of cdr)

;;** *application
(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

;;** primitive?
(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

;;** non-primitive?
(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

;;** apply
(define apply
  (lambda (fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive
       (second fun) vals))
     ((non-primitive? fun)
      (apply-closure
       (second fun) vals)))))

;;** apply-primitive
(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name cons)
      (cons (first vals) (second vals)))
     ((eq? name 'car)
      (car (first vals)))
     ((eq? name 'cdr)
      (cdr (first vals)))
     ((eq? name 'null?)
      (null? (first vals)))
     ((eq? name 'eq?)
      (eq? (first vals) (second vals)))
     ((eq? name 'atom?)
      (:atom? (first vals)))
     ((eq? name 'zero?)
      (zero? (first vals)))
     ((eq? name 'add1)
      (add1 (first vals)))
     ((eq? name 'sub1)
      (sub1 (first vals)))
     ((eq? name 'number?)
      (number? (first vals))))))

;;** :atom?
(define :atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) 'primitive) #t)
     ((eq? (car x) 'non-primitive) #t)
     (else #f))))

;;** apply-closure
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
               (new-entry
                (formals-of closure)
                vals)
               (table-of closure)))))



;; numbers => *const
;; #t => *const
;; #f => *const
;; cons => *const
;; (quote nothing) => *quote
;; nothing => *identifier
;; (lambda (x y) (cons x y)) => *lambda
;; ((lambda (nothing) (cond (nothing (quote something)) (else (quote nothing)))) #t) => *application => An application is a list of expressions whose car position contains an expression whose value is a function.
;;  (cond (nothing (quote something)) (else (quote nothing))) => *cond
