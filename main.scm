;;* lat?
(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

;;* atom?
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;;* eq?-c
(define eq?-c
  (lambda (a)
    (lambda (x) (eq? x a))))


;;* member?
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eqan? (car lat) a)
               (member? a (cdr lat)))))))

;;* rember
(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? (car l) s) (cdr l))
     (else (cons (car l) (rember s (cdr l)))))))

;;* firsts
(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l)) (firsts (cdr l)))))))

;;* seconds
(define seconds
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l)) (seconds (cdr l)))))))

;;* insert-g
(define insert-g
  (lambda (seq)
    (lambda (new old lat)
      (cond ((null? lat) '())
            ((equal? (car lat) old)
             (seq new old (cdr lat)))
            (else
             (cons (car lat)
                   ((insert-g seq)
                    new
                    old
                    (cdr lat))))))))

;;* insertR
(define insertR
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

;;* insertL
(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

;;* subst
(define subst
  (insert-g
   (lambda (new old lat)
     (cons new lat))))

;;* subst2
(define subst2
  (lambda (new o1 o2 lat)
    (cond ((null? lat) '())
          ((or (eq? (car lat) o1)
               (eq? (car lat) o2))
           (cons new (cdr lat)))
          (else
           (cons (car lat)
                 (subst2 new o1 o2 (cdr lat)))))))

;;* multirember
(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eqan? (car lat) a)
           (multirember a (cdr lat)))
          (else
           (cons (car lat)
                 (multirember a (cdr lat)))))))

;;* multirember-f
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) '())
            ((test? (car lat) a)
             ((multirember-f test?)
              a
              (cdr lat)))
            (else
             (cons (car lat)
                   ((multirember-f test?)
                    a
                    (cdr lat))))))))

;;* multirember-eq?
(define multirember-eq? (multirember-f eq?))

;;* multiremberT
(define multiremberT
  (lambda (test? lat)
    (cond ((null? lat) '())
          ((test? (car lat))
           (multiremberT test? (cdr lat)))
          (else
           (cons (car lat)
                 (multiremberT test? (cdr lat)))))))

;;* multiinsertR
(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

;;* multiinsertL
(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

;;* multisubst
(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat)))))))

;;* add1
(define add1
  (lambda (n)
    (+ n 1)))

;;* sub1
(define sub1
  (lambda (n)
    (- n 1)))

;;* ++-
(define ++
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (++ n (sub1 m)))))))

;;* --
(define --
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (-- n (sub1 m)))))))

;;* addtup
(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (++ (car tup) (addtup (cdr tup)))))))

;;* **
(define **
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (++ n (** n (sub1 m)))))))

;;* tup+
(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

;;* >>
(define >>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (>> (sub1 n) (sub1 m))))))

;;* <<
(define <<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (<< (sub1 n) (sub1 m))))))

;;* ==
(define ==
  (lambda (n m)
    (cond
     ((zero? m) (zero? n))
     ((zero? n) #f)
     (else (== (sub1 n) (sub1 m))))))

;;* ===
(define ===
  (lambda (n m)
    (cond
     ((>> n m) #f)
     ((<< n m) #f)
     (else #t))))

;;* pow
(define pow
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (** n (pow n (sub1 m)))))))

;;* //
(define //
  (lambda (n m)
    (cond
     ((<< n m) 0)
     (else (add1 (// (-- n m) m))))))

;;* length_
(define length_
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length_ (cdr lat)))))))

;;* pick
(define pick
  (lambda (n lat)
    (cond
     ((= n 1) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

;;* rempick
(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

;;* no-nums
(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat) (no-nums (cdr lat)))))))

;;* all-nums
(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

;;* eqan?
(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))

;;* occur
(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

;;* one?
(define one? (lambda (n) (eqan? n 1)))

;;* rember*
(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((and (atom? (car l)) (eqan? (car l) a)) (rember* a (cdr l)))
     ((atom? (car l)) (cons (car l) (rember* a (cdr l))))
     (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

;;* insertR*
(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((and (atom? (car l)) (eqan? (car l) old)) (cons old (cons new (insertR* new old (cdr l)))))
     ((atom? (car l)) (cons (car l) (insertR* new old (cdr l))))
     (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

;;* occur*
(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((and (atom? (car l)) (eqan? (car l) a)) (add1 (occur* a (cdr l))))
     ((atom? (car l)) (occur* a (cdr l)))
     (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

;;* subst*
(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((and (atom? (car l)) (eqan? (car l) old)) (cons new (subst* new old (cdr l))))
     ((atom? (car l)) (cons (car l) (subst* new old (cdr l))))
     (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

;;* insertL*
(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((and (atom? (car l)) (eqan? (car l) old))
      (cons new (cons old (insertL* new old (cdr l)))))
     ((atom? (car l))
      (cons (car l) (insertL* new old (cdr l))))
     (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

;;* member*
(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l)) (or (eqan? (car l) a) (member* a (cdr l))))
     (else (or (member* a (car l)) (member* a (cdr l)))))))

;;* leftmost
(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

;;* eqlist?
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))))))

;;* equal?
(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((or (atom? s1) (atom? s2))
      #f)
     (else (eqlist? s1 s2)))))

;;* numbered?
(define numbered?
  (lambda (s)
    (cond
     ((null? s) #t)
     ((atom? s)
      (cond
       ((number? s) #t)
       ((eq? s (quote +)) #t)
       ((eq? s (quote -)) #t)
       ((eq? s (quote *)) #t)
       (else #f)))
     (else (and (numbered? (car s)) (numbered? (cdr s)))))))

;;* numbered?_
(define numbered?_
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else (and (numbered?_ (car aexp)) (numbered?_ (car (cdr (cdr aexp)))))))))


;;* 1st-sub-exp
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

;;* 2nd-sub-exp
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

;;* operator
(define operator
  (lambda (aexp)
    (car aexp)))

;;* atom-to-function
(define atom-to-function
  (lambda (x)
    (cond
     ((eq? '+ x) +)
     ((eq? '- x) -)
     (else *))))

;;* value
(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          (else
           ((atom-to-function
             (operator nexp))
            (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp)))))))

;;* sero?
(define sero?
  (lambda (n)
    (null? n)))

;;* edd1
(define edd1
  (lambda (n)
    (cons '() n)))

;;* zub1
(define zub1
  (lambda (n)
    (cond
     ((null? n) '())
     (else (cdr n)))))

;;* +++
(define +++
  (lambda (n m)
    (cond
     ((sero? m) n)
     (else (edd1 (+++ n (zub1 m)))))))

;;* set?
(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

;;* makeset
(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cons (car lat)
            (makeset
             (multirember
              (car lat) (cdr lat))))))))

;;* subset?
(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

;;* eqset?
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))


;;* intersect?
(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

;;* intersect
(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1)
      '())
     ((member? (car set1) set2)
      (cons (car set1) (intersect (cdr set1) set2)))
     (else
      (intersect (cdr set1) set2)))))

;;* union
(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2) (union (cdr set1) set2))
     (else (cons (car set1) (union (cdr set1) set2))))))

;;* difference
(define difference
  (lambda (set1 set2)
    (cond
     (( null? set1) '())
     ((member? (car set1) set2)
      (difference (cdr set1) set2))
     (else (cons (car set1)
                 (difference (cdr set1) set2))))))

;;* intersectall
(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set) (intersectall (cdr l-set)))))))

;;* a-pair?
(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

;;* first
(define first (lambda (x) (car x)))

;;* second
(define second (lambda (x) (car (cdr x))))

;;* build
(define build (lambda (x1 x2) (cons x1 (cons x2 '()))))

;;* third
(define third (lambda (l) (car (cdr (cdr l)))))

;;* fun?
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

;;* revpair
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

;;* revel
(define revel
  (lambda (rel)
    (cond ((null? rel) '())
          (else
           (cons (revpair (car rel))
                 (revel (cdr rel)))))))

;;* fullfun?
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

;;* one-to-one?
(define one-to-one?
  (lambda (fun)
    (fun? (revel fun))))

;;* rember-f
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) '())
            ((test? (car l) a) (cdr l))
            (else
             (cons (car l)
                   ((rember-f test?) a (cdr l))))))))

;;* seqrem
(define seqrem
  (lambda (new old l) l))

;;* yyy
(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

;;* a-friend
(define a-friend
  (lambda (x y)
    (length_ x)))

;;* multirember&collector
(define multirember&collector
  (lambda (a lat collector)
    (cond ((null? lat)
           (collector '() '()))
          ((eq? (car lat) a)
           (multirember&collector a (cdr lat)
                                  (lambda (newlat seen)
                                    (collector
                                     newlat
                                     (cons (car lat) seen)))))
          (else
           (multirember&collector a (cdr lat)
                                  (lambda (newlat seen)
                                    (collector
                                     (cons (car lat) newlat)
                                     seen)))))))

;;* multiinsertLR
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) '())
          ((eq? (car lat) oldL)
           (cons new
                 (cons oldL
                       (multiinsertLR new oldL oldR (cdr lat)))))
          ((eq? (car lat) oldR)
           (cons oldR
                 (cons new
                       (multiinsertLR new oldL oldR (cdr lat)))))
          (else
           (cons (car lat)
                 (multiinsertLR new oldL oldR (cdr lat)))))))


;;* multiinsertLR&collector
(define multiinsertLR&collector
  (lambda (new oldl oldr lat collector)
    (cond ((null? lat)
           (collector '() 0 0))
          ((eq? (car lat) oldl)
           (multiinsertLR&collector new oldl oldr (cdr lat)
                                    (lambda (newlat left right)
                                      (collector (cons new (cons oldl newlat)) (add1 left) right))))
          ((eq? (car lat) oldr)
           (multiinsertLR&collector new oldl oldr (cdr lat)
                                    (lambda (newlat left right)
                                      (collector (cons oldr (cons new newlat)) left (add1 right)))))
          (else
           (multiinsertLR&collector new oldl oldr
                                    (cdr lat)
                                    (lambda (newlat left right)
                                      (collector (cons (car lat) newlat) left right)))))))

(define even?
  (lambda (n)
    (== (** (// n 2) 2) n)))

;;* evens-only*
(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((and (atom? (car l)) (even? (car l)))
      (cons (car l) (evens-only* (cdr l))))
     ((atom? (car l)) (evens-only* (cdr l)))
     (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

;;* evens-only*&co
(define evens-only*&co
  (lambda (l co)
    (cond
     ((null? l) (co '() 1 0))
     ((and (atom? (car l)) (even? (car l)))
      (evens-only*&co (cdr l)
                      (lambda (newl p s)
                        (co (cons (car l) newl)
                            (** (car l) p)
                            s))))
     ((atom? (car l))
      (evens-only*&co (cdr l) (lambda (newl p s)
                                (co newl p (++ s (car l))))))

     (else
      (evens-only*&co (car l)
                      (lambda (al ap as)
                        (evens-only*&co (cdr l)
                                        (lambda (dl dp ds)
                                          (co (cons al dl)
                                              (** ap dp)
                                              (++ as ds))))))))))

;;* keep-looking
(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn)
      (keep-looking a (pick sorn lat) lat))
     (else (eq? sorn a)))))

;;* looking
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))


;;* eternity
(define eternity
  (lambda (x)
    (eternity x)))

;;* shift
(define shift
  (lambda (pair)
    (build
     (first (first pair))
     (build
      (second (first pair))
      (second pair)))))

;;* align
;; pora stands for pair or atom
(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else (build (first pora)
                  (align (second pora)))))))

;;* length*
(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (+ (length* (first pora)) (length* (second pora)))))))

;;* weight*
(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (+ (* (weight* (first pora)) 2)
         (weight* (second pora)))))))

;;* shuffle
(define shuffle
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (shuffle (revpair pora)))
     (else (build (first pora)
                  (shuffle (second pora)))))))

;;* C
;; Lothar Collatz
(define C
  (lambda (n)
    (cond
     ((one? n) 1)
     (else
      (cond
       ((even? n) (C (// n 2)))
       (else (C (add1 (** 3 n)))))))))

;;* A
;; Wilhelm Ackermann
(define A
  (lambda (n m)
    (cond
     ((zero? n) (add1 m))
     ((zero? m) (A (sub1 n) 1))
     (else (A (sub1 n) (A n (sub1 m)))))))
