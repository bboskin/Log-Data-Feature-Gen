#lang racket
(require "gen-features.rkt")

(define args (current-command-line-arguments))
(define K
  (if (zero? (vector-length args))
      3
      (string->number (vector-ref args 0))))

(define FILENAME
  (if (< (vector-length args) 2)
      "../covid-input-automata-csv/covid-counties-55k-input.csv"
      (vector-ref args 1)))

(define WEEKEND '("Saturday" "Sunday"))
(define NORMAL-FORMATS '(gif html jpg))
(define FILTER-FNS
  `((few-deaths ,(λ (x) (<= (list-ref x 4) 40)))
    (many-deaths ,(λ (x) (>= (list-ref x 4) 40)))
    (few-cases ,(λ (x) (<= (list-ref x 3) 500)))
    (many-cases ,(λ (x) (>= (list-ref x 3) 500)))


    #;(morning ,(λ (x) (<= (list-ref x 4) 12)))
    #;(evening ,(λ (x) (>= (list-ref x 4) 12)))
    #;(weekday ,(λ (x) (not (member (list-ref x 5) WEEKEND))))
    #;(weekend ,(λ (x) (member (list-ref x 5) WEEKEND)))
    #;(large ,(λ (x) (>= (list-ref x 2) 10000)))
    #;(small ,(λ (x) (<= (list-ref x 2) 1000)))
    #;(gif ,(λ (x) (eqv? (list-ref x 6) 'gif)))
    #;(html ,(λ (x) (eqv? (list-ref x 6) 'html)))
    #;(jpg ,(λ (x) (eqv? (list-ref x 6) 'jpg)))
    #;(other ,(λ (x) (not (memv (list-ref x 6) NORMAL-FORMATS))))))

(define REDUCE-FNS
  `((max ,max-ls Nats Nat)
    (min ,min-ls Nats Nat)
    (mean ,mean Nats Nat)
    (median ,median Nats Nat)
    (mode ,mode Nats Nat)
    (range ,get-range Nats Nat)
    (std-dev ,std-dev Nats Nat)
    
    (length ,length Set Nat)
    (k-unique-elems ,k-unique-elems Set Nat)
    (cast ,cast Set Nats)))

(define REDUCE-FNS-FORMATTED
  (map (λ (x) `(,(symbol-append 'reduce (car x)) . ,(cdr x)))
       REDUCE-FNS))
(define REDUCE-NATS->NAT-OPS
  (mapquote (map car (filter (λ (x) (equal? (cddr x) `(Nats Nat)))
                   REDUCE-FNS-FORMATTED))))
(define REDUCE-SET->NAT-OPS
  (mapquote (map car (filter (λ (x) (equal? (cddr x) `(Set Nat)))
                   REDUCE-FNS-FORMATTED))))
(define REDUCE-SET->NATS-OPS
  (mapquote (map car (filter (λ (x) (equal? (cddr x) `(Set Nats)))
                   REDUCE-FNS-FORMATTED))))
(define REDUCE-SET->SET-OPS
  (mapquote (map car (filter (λ (x) (equal? (cddr x) `(Set Set)))
                   REDUCE-FNS))))

(define (SELECT-OPS Fields)
  (mapquote (map (λ (x) (symbol-append 'select x)) Fields)))

(define FILTER-OPS
  (mapquote (map (λ (x) (symbol-append 'filter (car x))) FILTER-FNS)))

(define (MAP-OPS Fields) (mapquote (map (λ (x) (symbol-append 'map x)) Fields)))
;; we can make a recursive one
(define (make-grammar-infinite desc table i)
  (let ((NatFields (map car (filter (λ (x) (equal? (cadr x) 'number)) (cdr desc))))
        (Fields (map car (cdr desc))))
    (CNF->PDA
     (CFG->CNF
      `((Feature ->
                 (FilterOp GNats ReduceNats->Nat)
                 (GNats ReduceNats->Nat))
        (FilterOp -> . ,(mapquote (map car FILTER-FNS)))
        (GNats ->
               SelectNats
               (SelectNonNats ReduceSet->Nats)
               (Map GNats ReduceNats->Nat)
               (Map Select ReduceSet->Nat))
        #;(GSet -> Select (GSet ReduceSet->Set))
        (Map -> . ,(mapquote (map (λ (x) (symbol-append 'map x)) Fields)))
        (SelectNats -> . ,(mapquote (map (λ (x) (symbol-append 'select x)) NatFields)))
        (SelectNonNats -> . ,(mapquote (map (λ (x) (symbol-append 'select x)) (set-difference Fields NatFields))))
        (Select -> . ,(SELECT-OPS Fields))
        (ReduceNats->Nat -> . ,REDUCE-NATS->NAT-OPS)
        (ReduceSet->Nat -> . ,REDUCE-SET->NAT-OPS)
        #;(ReduceSet->Set -> . ,REDUCE-SET->SET-OPS)
        (ReduceSet->Nats -> . ,REDUCE-SET->NATS-OPS))))))

;; we can make a new finite one up
(define (make-grammar-finite desc table i)
  (let* ((NatFields (map car (filter (λ (x) (equal? (cadr x) 'number)) (cdr desc))))
         (Fields (map car (cdr desc)))
         (NonNatFields (set-difference Fields NatFields)))
    (CNF->PDA
     (CFG->CNF
      `((Feature ->
                 (FilterOp GNats ReduceNats->Nat)
                 (GNats ReduceNats->Nat))
        
        (GNats ->
               SelectNats
               (SelectNonNats ReduceSet->Nats)
               (Map SelectNats ReduceNats->Nat)
               (Map Select ReduceSet->Nat))
        (FilterOp -> . ,FILTER-OPS)
        (Map -> . ,(MAP-OPS Fields))
        (SelectNats -> . ,(SELECT-OPS NatFields))
        (SelectNonNats -> . ,(SELECT-OPS NonNatFields))
        (Select -> . ,(SELECT-OPS Fields))
        (ReduceNats->Nat -> . ,REDUCE-NATS->NAT-OPS)
        (ReduceSet->Nat -> . ,REDUCE-SET->NAT-OPS)
        (ReduceSet->Nats -> . ,REDUCE-SET->NATS-OPS))))))

(define (make-grammar-micro desc table i)
  (let* ((NatFields (map car (filter (λ (x) (equal? (cadr x) 'number)) (cdr desc))))
         (Fields (map car (cdr desc)))
         (NonNatFields (set-difference Fields NatFields)))
    (CNF->PDA
     (CFG->CNF
      `((Feature ->
                 (FilterOp GNats ReduceNats->Nat)
                 (GNats ReduceNats->Nat))
        (GNats ->
               SelectNats
               (Select ReduceSet->Nats))
        (FilterOp -> . ,FILTER-OPS)
        (SelectNats -> . ,(SELECT-OPS NatFields))
        (Select -> . ,(SELECT-OPS Fields))
        (ReduceNats->Nat -> . ,REDUCE-NATS->NAT-OPS)
        (ReduceSet->Nats -> . ,REDUCE-SET->NATS-OPS))))))

(define (gen-player-automaton G desc table name)
  (G desc table name))


(define COVID-DESC
  '(COVID
    (flips number #t)
    (date number)
    (cases number)
    (deaths number)))

(define COVID-DATA
  (read-logs COVID-DESC FILENAME))
(define COVID-KEYS
  (foldr (λ (x a) (set-cons (cadr x) a)) '() COVID-DATA))
(define COVID-AUTOMATON
  (gen-player-automaton
   (if (zero? K) make-grammar-finite (if (= K -1) make-grammar-micro make-grammar-infinite))
   COVID-DESC
   COVID-DATA
   (car COVID-KEYS)))
(define COVID-FEATURES
  (time (reverse (take-words COVID-AUTOMATON (if (< K 1) 20000 (* 1000 K))))))
(define COVID-HASH (hash-logs COVID-KEYS 1 COVID-DATA (make-immutable-hash)))


(define COVID-TABLE
   (time `((name . ,(build-list (length COVID-FEATURES) (λ (x) (string->symbol (string-append "F" (number->string x))))))
    . ,(remove-inf
        (make-table
         REDUCE-FNS
         FILTER-FNS
         COVID-KEYS
         COVID-DESC
         COVID-DATA
         COVID-FEATURES
         COVID-HASH)))))

(display-table COVID-TABLE)