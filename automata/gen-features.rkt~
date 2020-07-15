
#lang racket

(require
  "basics.rkt"
  "queries.rkt"
  "G-to-M.rkt"
  "grammars.rkt"
  csv-writing
  csv-reading)


(provide (all-defined-out)
         CNF->PDA
         CFG->CNF
         symbol-append
         set-cons
         take-words
         find-words
         display-table
         set-difference)


(define make-food-csv-reader
  (make-csv-reader-maker
   '((separator-chars            #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define (format-element e ty)
  (match ty
    ['symbol (string->symbol e)]
    ['number (/ (string->number e) 1.0)]
    ['string e]
    [else (error "unexpected type")]))


;; provided a table description and a filename,
;; reads the csv file at filename and makes a Racket list of lists
(define (read-logs desc filename)
  (let ((id (car desc))
        (next-row (make-food-csv-reader
                   (open-input-file filename))))
    (next-row)
    (csv-map (λ (x) (cons id (map format-element x (map cadr (cdr desc))))) next-row)))

(define (hash-logs keys k logs H)
  (let loop ((logs logs)
             (keys keys)
             (ans H))
(cond
  [(null? keys) ans]
  [else (let ((i (car keys))
              (keys (cdr keys)))
          (let ((e (hash-ref ans i (λ () #f)))
                (logs-with (filter (λ (x) (equal? i (list-ref x k))) logs))
                (logs-without (filter (λ (x) (not (equal? i (list-ref x k)))) logs)))
            (let ((ans (loop logs-without keys ans)))
              (if e
                  (hash-set ans i (append e logs-with))
                  (hash-set ans i logs-with)))))])))
#|

In order to define how to read a datatype,
you need to make a list that gives the typ
of the values in each column. Here is an example for
the garbage call-log-data.

The value labeled with #t is the field that must be filtered by at the start,
and which determines the meaning for the rows of the final CSV.

|#

;; functions to get info from descriptions
(define (get-key-loc desc)
  (let loop ((k 0)
             (ls (cdr desc)))
    (cond
      [(null? ls) 0]
      [(member #t (car ls)) k]
      [else (loop (add1 k) (cdr ls))])))

(define (get-loc eid desc)
  (let loop ((k 0)
             (ls (cdr desc)))
    (cond
      [(null? ls) 0]
      [(member eid (car ls)) k]
      [else (loop (add1 k) (cdr ls))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions to help define Grammar (constant for all datasets) 

;;; reductions w/ type info

(define (count-occurrences x xs)
  (length (filter (λ (y) (equal? x y)) xs)))

(define (mode xs)
  (if (null? xs)
      0
      (caar (sort (map (λ (x) (cons x (count-occurrences x xs)))
                       xs)
                  (λ (x y)
                    (> (cdr x) (cdr y)))))))

(define (median xs)
  (let ((k (length xs))
        (xs (sort xs <)))
    (if (null? xs) 0(list-ref xs (floor (/ k 2))))))

(define (mean xs)
  (if (null? xs) 0 (/ (foldr + 0 xs) (length xs))))

(define (min-ls xs)
  (if (null? xs) 0 (foldr min 0 xs)))

(define (max-ls xs)
  (if (null? xs) 0 (foldr max 0 xs)))

(define (to-set xs)
  (foldr set-cons '() xs))

(define (mapquote ls) (map (λ (x) `',x) ls))

(define (cast x)
  (match x
    [(? number?) x]
    [(? string?) (foldr + 0 (map char->integer (string->list x)))]
    [(? symbol?) (foldr + 0 (map char->integer (string->list (symbol->string x))))]
    [else 0]))

(define (get-range xs)
  (let ((min (min-ls xs))
        (max (min-ls xs)))
    (- max min)))

(define (k-unique-elems xs)
  (length (to-set xs)))

(define (std-dev xs)
  (let ((k (length xs))
        (xbar (mean xs)))
    (if (<= k 1)
        0
        (sqrt (/ (foldr + 0 (map (λ (x) (sqr (- x xbar))) xs))
                 (sub1 k))))))



(define (tag->function Reduce t)
  (let ((f (assv t Reduce)))
    (if f (cadr f) #f)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a handful of grammars
;; the ones we aren't using anymore are commented out
#;
(define (make-grammar-full desc table i)
  (let ((NatFields (map car (filter (λ (x) (equal? (cadr x) 'number)) (cdr desc))))
        (Fields (map car (cdr desc))))
    (CNF->PDA
     (CFG->CNF
      `((Feature ->
                 (GNats ReduceNats->Nat)
                 (GSet ReduceSet->Nat))
        (GNats -> SelectNats
               (Map GNats ReduceNats->Nat)
               (Map GSet ReduceSet->Nat))
        (GSet -> Select
              (GSet ReduceSet->Set)
              (GNats ReduceSet->Set))
        (SelectNats -> . ,(mapquote (map (λ (x) (symbol-append 'select x)) NatFields)))
        (Select -> . ,(mapquote (map (λ (x) (symbol-append 'select x)) Fields)))
        (Map -> . ,(mapquote (map (λ (x) (symbol-append 'map x)) Fields)))
        (ReduceNats->Nat -> . ,(map (λ (x) `',(symbol-append 'reduce x)) REDUCE-NATS->NAT-OPS))
        (ReduceSet->Nat -> . ,(map (λ (x) `',(symbol-append 'reduce x)) REDUCE-SET->NAT-OPS))
        (ReduceSet->Set -> . ,(map (λ (x) `',(symbol-append 'reduce x)) REDUCE-SET->SET-OPS)))))))

#;
(define (make-grammar-micro desc table i)
  (let ((NatFields (map car (filter (λ (x) (equal? (cadr x) 'number)) (cdr desc)))))
    (CNF->PDA
     (CFG->CNF
      `((Feature
         -> (Select ReduceNats->Nat))
        (Select
         -> . ,(mapquote (map (λ (x) (symbol-append 'select x))
                              NatFields)))
        (ReduceNats->Nat
         -> . ,(map (λ (x) `',(symbol-append 'reduce x))
                    REDUCE-NATS->NAT-OPS)))))))



(define (set-features-to-key features name)
  features)



(define pr?
  (λ (x)
    (and (cons? x)
         (number? (car x))
         (number? (cdr x)))))

(define Value?
  (λ (x)
    (and (not (Label? x))
         (or (number? x)
             (symbol? x)
             (string? x)
             (pr? x)
             (and (list? x)
                  (or (andmap number? x)
                      (andmap symbol? x)
                      (andmap pr? x)))))))

(struct Label [v] #:transparent)

(define CDR?
  (λ (x)
    (and (cons? x)
         (eqv? (car x) 'CDR)
         (andmap cons? (cdr x)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation of CDRs using Features

;; helpers for map
(define (add-to-group v a l)
  (match l
    ['() `((,(Label v) ,a))]
    [`((,(Label v2) . ,es) . ,l)
     (cond
       [(equal? v v2) `((,(Label v) ,a . ,es) . ,l)]
       [else `((,(Label v2) . ,es) . ,(add-to-group v a l))])]))

(define (group-by Log k o)
  (match o
    ['() `()]
    [`(,a . ,o)
     (add-to-group (list-ref a k) (cons Log a) (group-by Log  k o))]))


;; Filter : Eid x CDRs -> CDRs
(define (Filter k t)
  (let (#;(k (assv f FILTER-FNS)))
    (if k
        (let ((f (cadr k)))
          (filter f t))
        (error (format "unknown function: ~s" k)))))

;; Map : CDRTree -> CDRTree
(define ((Map Log desc eid) o)
  (match o
    ['() '()]
    [`((,(? (λ (x) (eqv? x Log))) . ,vs) ...)
     `(,(Label eid) . ,(group-by Log (get-loc eid desc) vs))]
    [`(,(? Label? e) . ,T)
     `(,e . ,((Map Log desc eid) T))]
    [`(,T ...) (map (Map Log desc eid) T)]))


;; Select : CDRTree -> ValTree
(define (((Select Log? desc) eid) o)
  (match o
    ['() '()]
    [`((,(? (λ (x) (eqv? x Log?))) . ,vs) ...)
     (let ((k (get-loc eid desc)))
       (map (λ (c) (list-ref c k)) vs))]
    [`(,(? Label? e) . ,T) `(,e . ,(((Select Log? desc) eid) T))]
    [`(,T ...) (map ((Select Log? desc) eid) T)]))



;; Reduce : ValTree -> ValTree
(define ((Reduce f) o)
  (match o
    ['() (f '())]
    [(? Value? v) (if (not (cons? v)) (f `(,v)) (f v))]
    [`(,(? Value? v) ...) (f v)]
    [`(,(? Label? e) ,(? Value? v) ...) (f v)]
    [`(,(? Label? e) . ,(? Value? v)) (f `(,v))]
    [`(,(? Label? e) ,(? Label? e2) ,(? Value? v) ...) (f v)]
    [`(,(? Label? e)
       (,(? Label? l) ,(? Value? v) ,vs ...))
     (f (cons v vs))]
    [`(,(? Label? e)
       (,(? Label? l) ,(? Value? v) ,vs ...)
       ...)
     `(,e . ,(map (λ (x) (f (cdr x))) (cdr o)))]
    [`(,(? Label? e)
       (,(? Label? l1) ,(? Label? l2) ,(? Value? vs) ...))
     `(,e . ,(f vs))]
    [`(,(? Label? e)
       (,(? Label? l1) ,(? Label? l2) ,(? Value? vs) ...)
       ...)
     `(,e . ,(map (λ (x) (f (cddr x))) (cdr o)))]
    [`(,(? Label? e) . ,T) `(,e . ,((Reduce f) T))]
    [`(,T ...) (map (Reduce f) T)])) 



;; putting it all together

(define (filterword? x)
  (and (symbol? x)
       (let ((x (symbol->string x)))
         (and (>= (string-length x) 6)
              (string=? (substring x 0 6) "filter")))))
(define (reduceword? x)
  (and (symbol? x)
       (let ((x (symbol->string x)))
         (and (>= (string-length x) 6)
              (string=? (substring x 0 6) "reduce")))))

(define (selectword? x)
  (and (symbol? x)
       (let ((x (symbol->string x)))
         (and (>= (string-length x) 6)
              (string=? (substring x 0 6) "select")))))
(define (mapword? x)
  (and (symbol? x)
       (let ((x (symbol->string x)))
         (and (>= (string-length x) 3)
              (string=? (substring x 0 3) "map")))))

(define ((apply-word Reduces Filters Log desc) w ls)
  (match w
    ['() ls]
    [`(,(? mapword? f) . ,w)
     (let ((t (string->symbol (substring (symbol->string f) 3))))
       ((apply-word Reduces Filters Log desc) w ((Map Log desc t) ls)))]
    [`(,(? selectword? f) . ,w)
     (let ((t (string->symbol (substring (symbol->string f) 6))))
       ((apply-word Reduces Filters Log desc) w (((Select Log desc) t) ls)))]
    [`(,(? reduceword? f) . ,w)
     (let ((f (tag->function Reduces (string->symbol (substring (symbol->string f) 6)))))
       ((apply-word Reduces Filters Log desc) w ((Reduce f) ls)))]
    [`(,(? filterword? f) . ,w)
     (let ((f (string->symbol (substring (symbol->string f) 6))))
       ((apply-word Reduces Filters Log desc) w (Filter (assv f Filters) ls)))]
    [else ((apply-word Reduces Filters Log desc) (cdr w) ls)]))

(define (apply-words Reduces Filters desc Table ws)
  (map (λ (x) ((apply-word Reduces Filters (car desc) desc) x Table)) ws))

(define (make-table Reduces Filters keys desc table features H)
  (map
   (λ (key)
     (let ((fs (set-features-to-key features key)))
       (cons key (apply-words Reduces Filters desc (hash-ref H key (λ () '())) fs))))
   keys))

(define (test-apply-words Log desc Table ws)
  (foldr
   (λ (x a)
     (let ((v ((apply-word Log desc) x Table)))
       (if (not (number? v))
           (displayln x)
           a)))
   #t
   ws))

(define (remove-inf table)
  (map
   (λ (x)
     (map (λ (x)
              (if (or (and (not (number? x))
                           (not (string? x))
                           (not (boolean? x))
                           (not (symbol? x)))
                      (equal? x +inf.0))
                  9999999
                  x))
            x))
   table))




