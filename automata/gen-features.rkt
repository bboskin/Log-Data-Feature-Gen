#lang racket

(require
  "basics.rkt"
  "queries.rkt"
  "G-to-M.rkt"
  "grammars.rkt"
  csv-writing
  csv-reading)


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

(define REDUCE-FNS
  `((+ ,(λ (x) (foldr + 0 x)) Nats Nat)
    (* ,(λ (x) (foldr * 1 x)) Nats Nat)
    (mean ,(λ (xs)
             (if (null? xs) 0 (/(foldr + 0 xs) (length xs))))
          Nats Nat)
    (length ,length Set Nat)
    (set ,(λ (x) (foldr set-cons '() x)) Set Set)))

(define REDUCE-NATS->NAT-OPS
  (map car
       (filter (λ (x) (equal? (cddr x) `(Nats Nat))) REDUCE-FNS)))
(define REDUCE-SET->NAT-OPS
  (map car
       (filter (λ (x) (equal? (cddr x) `(Set Nat))) REDUCE-FNS)))
(define REDUCE-SET->SET-OPS
  (map car
       (filter (λ (x) (equal? (cddr x) `(Set Set))) REDUCE-FNS)))

(define (tag->function t)
  (let ((f (assv t REDUCE-FNS)))
    (if f (cadr f) #f)))

(define (mapquote ls) (map (λ (x) `',x) ls))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a handful of grammars

(define (make-grammar-micro desc table i)
  (let ((NatFields (map car (filter (λ (x) (equal? (cadr x) 'number)) (cdr desc)))))
    (CNF->PDA
     (CFG->CNF
      `((Feature
         -> (',i Select ReduceNats->Nat))
        (Select
         -> . ,(mapquote (map (λ (x) (symbol-append 'select x))
                              NatFields)))
        (ReduceNats->Nat
         -> . ,(map (λ (x) `',(symbol-append 'reduce x))
                    REDUCE-NATS->NAT-OPS)))))))


(define (make-grammar-short desc table i)
  (let ((NatFields (map car (filter (λ (x) (equal? (cadr x) 'number)) (cdr desc))))
        (Fields (map car (cdr desc))))
    (CNF->PDA
     (CFG->CNF
      `((Feature ->
                 (',i GNats ReduceNats->Nat))
        (GNats -> SelectNats
               (Map GNats ReduceNats->Nat))
      (SelectNats -> . ,(mapquote (map (λ (x) (symbol-append 'select x)) NatFields)))
      (Select -> . ,(mapquote (map (λ (x) (symbol-append 'select x)) Fields)))
      (Map -> . ,(mapquote (map (λ (x) (symbol-append 'map x)) Fields)))
      (ReduceNats->Nat -> . ,(map (λ (x) `',(symbol-append 'reduce x)) REDUCE-NATS->NAT-OPS)))))))


(define (make-grammar-full desc table i)
  (let ((NatFields (map car (filter (λ (x) (equal? (cadr x) 'number)) (cdr desc))))
        (Fields (map car (cdr desc))))
    (CNF->PDA
     (CFG->CNF
      `((Feature ->
                 (',i GNats ReduceNats->Nat)
                 (',i GSet ReduceSet->Nat))
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

(define (gen-player-automaton desc table name)
  (make-grammar-full desc table name))

(define (set-features-to-key features name)
  (map (λ (x) (cons name (cdr x))) features))



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
(define ((Filter desc) f t)
  (let ((k (add1 (get-key-loc desc))))
    (filter (λ (x) (equal? (list-ref x k) f)) t)))

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

(define ((apply-word Log desc) w ls)
  (match w
    ['() ls]
    [`(,(? mapword? f) . ,w)
     (let ((t (string->symbol (substring (symbol->string f) 3))))
       ((apply-word Log desc) w ((Map Log desc t) ls)))]
    [`(,(? selectword? f) . ,w)
     (let ((t (string->symbol (substring (symbol->string f) 6))))
       ((apply-word Log desc) w (((Select Log desc) t) ls)))]
    [`(,(? reduceword? f) . ,w)
     (let ((f (tag->function (string->symbol (substring (symbol->string f) 6)))))
       ((apply-word Log desc) w ((Reduce f) ls)))]
    [`(,id . ,w)
     ((apply-word Log desc) w ((Filter desc) id ls))]
    [else ((apply-word Log desc) (cdr w) ls)]))

(define (apply-words Log desc Table ws)
  (map (λ (x) ((apply-word Log desc) x Table)) ws))

(define ((eval desc Table) w) ((apply-word desc) w Table))

(define (make-table log keys desc table features)
  (map
   (λ (key)
     (let ((fs (set-features-to-key features key)))
       (cons key (apply-words log desc table fs))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; different log data descriptions, readers, tables, keys, and features


;;;;;;;;;;;;;;;;;;;;;; DATASET 2
;; nasa shit with websites idk i just do what jack tells me
(define NASA-DESC
  '(NASA
    (host symbol #t)
    (bytes number)
    (time string)
    (day string)))

(define NASA-DATA (read-logs NASA-DESC "../input-automata-csv/nasa_input.csv"))
(define NASA-KEYS (foldr (λ (x a) (set-cons (list-ref x 1) a)) '() NASA-DATA))
(define NASA-AUTOMATON
  (gen-player-automaton NASA-DESC NASA-DATA (car NASA-KEYS)))

#;
(define NASA-FEATURES (time (take-words NASA-AUTOMATON 1000)))

#;
(define NASA-TABLE
  `((name . ,(build-list (length NASA-FEATURES) (λ (x) (string->symbol (string-append "F" (number->string x))))))
    . ,(remove-inf (make-table 'NASA NASA-KEYS NASA-DESC NASA-DATA NASA-FEATURES))))

;; use display-table expressions to write output to csv file
#;
(display-table NASA-TABLE)

;;;;;;;;;;;;;;;;;;;;;; DATASET 2
;; NASA-edu dataset
;; "host","bytes","date","time","day"

(define NASA-EDU-DESC
  '(NASA-EDU
    (host symbol #t)
    (bytes number)
    (date number)
    (time number)
    (day string)))

(define NASA-EDU-DATA
  (time
   (read-logs
    NASA-EDU-DESC
    "../input-automata-csv/small-nasa-edu-net.csv")))
(define NASA-EDU-KEYS
  (time
   (foldr (λ (x a) (set-cons (cadr x) a)) '() NASA-EDU-DATA)))
(define NASA-EDU-AUTOMATON
  (time (gen-player-automaton NASA-EDU-DESC NASA-EDU-DATA (car NASA-EDU-KEYS))))
(define NASA-EDU-FEATURES (time (take-words NASA-EDU-AUTOMATON 50)))


(define NASA-EDU-TABLE
  (time
   `((name . ,(build-list (length NASA-EDU-FEATURES) (λ (x) (string->symbol (string-append "F" (number->string x))))))
     . ,(remove-inf (make-table 'NASA-EDU NASA-EDU-KEYS NASA-EDU-DESC NASA-EDU-DATA NASA-EDU-FEATURES)))))


(display-table NASA-EDU-TABLE)
