#lang racket
(require "gen-features.rkt")


(define FILTER-FNS
  `((filtermorning ,(λ (x) (let ((time (list-ref x 4)))
                             (<= time 12))))
    (filterevening ,(λ (x) (let ((time (list-ref x 4)))
                             (>= time 12))))
    (filterweekday ,(λ (x) (let ((day (list-ref x 5)))
                             (not (member day '("Saturday" "Sunday"))))))
    (filterweekend ,(λ (x) (let ((day (list-ref x 5)))
                             (member day '("Saturday" "Sunday")))))
    (filterlarge ,(λ (x) (let ((size (list-ref x 2)))
                           (>= size 10000))))
    (filtersmall ,(λ (x) (let ((size (list-ref x 2)))
                           (<= size 1000))))
    (filtergif ,(λ (x) (let ((format (list-ref x 6)))
                           (eqv? format 'gif))))
    (filterhtml ,(λ (x) (let ((format (list-ref x 6)))
                           (eqv? format 'html))))
    (filterjpg ,(λ (x) (let ((format (list-ref x 6)))
                           (eqv? format 'jpg))))
    (filterotherformat ,(λ (x) (let ((format (list-ref x 6)))
                                 (not (memv format '(jpg html gif))))))))

(define REDUCE-FNS
  `(#;(+ ,(λ (x) (foldr + 0 x)) Nats Nat)
    #;(* ,(λ (x) (foldr * 1 x)) Nats Nat)
    (max ,max-ls Nats Nat)
    (min ,min-ls Nats Nat)
    (mean ,mean Nats Nat)
    (median ,median Nats Nat)
    (mode ,mode Nats Nat)
    (range ,get-range Nats Nat)
    (std-dev ,std-dev Nats Nat)
    
    (length ,length Set Nat)
    (k-unique-elems ,k-unique-elems Set Nat)
    
    #;(set ,to-set Set Set)
    
    (cast ,cast Set Nats)))

(define REDUCE-NATS->NAT-OPS
  (map car (filter (λ (x) (equal? (cddr x) `(Nats Nat))) REDUCE-FNS)))
(define REDUCE-SET->NAT-OPS
  (map car (filter (λ (x) (equal? (cddr x) `(Set Nat))) REDUCE-FNS)))
(define REDUCE-SET->NATS-OPS
  (map car (filter (λ (x) (equal? (cddr x) `(Set Nats))) REDUCE-FNS)))
(define REDUCE-SET->SET-OPS
  (map car (filter (λ (x) (equal? (cddr x) `(Set Set))) REDUCE-FNS)))

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
        (Select -> . ,(mapquote (map (λ (x) (symbol-append 'select x)) Fields)))
        (ReduceNats->Nat -> . ,(map (λ (x) `',(symbol-append 'reduce x)) REDUCE-NATS->NAT-OPS))
        (ReduceSet->Nat -> . ,(map (λ (x) `',(symbol-append 'reduce x)) REDUCE-SET->NAT-OPS))
        #;(ReduceSet->Set -> . ,(map (λ (x) `',(symbol-append 'reduce x)) REDUCE-SET->SET-OPS))
        (ReduceSet->Nats -> . ,(map (λ (x) `',(symbol-append 'reduce x)) REDUCE-SET->NATS-OPS)))))))

;; we can make a new one up
(define (make-grammar-finite desc table i)
  (let ((NatFields (map car (filter (λ (x) (equal? (cadr x) 'number)) (cdr desc))))
        (Fields (map car (cdr desc))))
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
        (FilterOp -> . ,(mapquote (map car FILTER-FNS)))
        (Map -> . ,(mapquote (map (λ (x) (symbol-append 'map x)) Fields)))
        (SelectNats -> . ,(mapquote (map (λ (x) (symbol-append 'select x)) NatFields)))
        (SelectNonNats -> . ,(mapquote (map (λ (x) (symbol-append 'select x)) (set-difference Fields NatFields))))
        (Select -> . ,(mapquote (map (λ (x) (symbol-append 'select x)) Fields)))
        (ReduceNats->Nat -> . ,(map (λ (x) `',(symbol-append 'reduce x)) REDUCE-NATS->NAT-OPS))
        (ReduceSet->Nat -> . ,(map (λ (x) `',(symbol-append 'reduce x)) REDUCE-SET->NAT-OPS))
        (ReduceSet->Nats -> . ,(map (λ (x) `',(symbol-append 'reduce x)) REDUCE-SET->NATS-OPS)))))))

(define (gen-player-automaton desc table name)
  (make-grammar-finite desc table name))


(define NASA-EDU-DESC
  '(NASA-EDU
    (host symbol #t)
    (bytes number)
    (date number)
    (time number)
    (day string)
    (format symbol)))

(define NASA-EDU-DATA
  (read-logs
    NASA-EDU-DESC
   "../input-automata-csv/new/small-nasa-edu-net-fiveK_withURL.csv"))
(define NASA-EDU-KEYS
  (foldr (λ (x a) (set-cons (cadr x) a)) '() NASA-EDU-DATA))
(define NASA-EDU-AUTOMATON
  (gen-player-automaton
   NASA-EDU-DESC
   NASA-EDU-DATA
   (car NASA-EDU-KEYS)))
(define NASA-EDU-FEATURES (reverse (take-words NASA-EDU-AUTOMATON 20000)))
(define NASA-EDU-HASH (hash-logs NASA-EDU-KEYS 1 NASA-EDU-DATA (make-immutable-hash)))

#|

;; non-recursive grammar
270 total features generated (asked for 3000)
takes less than a second to find, 2 secs to apply.



;; recursive grammar
finding features (on 5k dataset)
  just finding          | finding / applying)
  2000 took 3 seconds   | 1 second / 63 seconds
  3000 took 7 seconds   | 4 seconds /  38 seconds
  4000 took 12 seconds  | 11 seconds / 56 seconds ?
  5000 took 13 seconds  | 35 seconds / 124 seconds  ? 
  6000 took 30 seconds  | 19 seconds / 142 seconds
  7000 took 24 seconds  | 44 seconds / 88 seconds
  8000 took 34 seconds  | 38 seconds / 174 seconds
  9000 took 37 seconds  | 19 seconds / 105 seconds
  10K  took 59 seconds  | 61 seconds / 153 seconds
  20K  took 107 seconds | 92 seconds / 219 seconds

|#

#;
(define NASA-EDU-TABLE
  `((name . ,(build-list (length NASA-EDU-FEATURES) (λ (x) (string->symbol (string-append "F" (number->string x))))))
    . ,(remove-inf
        (make-table
         REDUCE-FNS
         FILTER-FNS
         'NASA-EDU
         NASA-EDU-KEYS
         NASA-EDU-DESC
         NASA-EDU-DATA
         NASA-EDU-FEATURES
         NASA-EDU-HASH))))


#;



(display-table NASA-EDU-TABLE)