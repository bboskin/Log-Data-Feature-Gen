#lang racket
(require "gen-features.rkt")

(define args (current-command-line-arguments))
(define K (if (zero? (vector-length args))
              -1
              (string->number (vector-ref args 0))))
(define FILENAME
  (if (< (vector-length args) 2)
      "../input-automata-csv/5k-with-url.csv"
      (vector-ref args 1)))


(define FILTER-FNS
  `((morning ,(λ (x) (let ((time (list-ref x 4)))
                             (<= time 12))))
    (evening ,(λ (x) (let ((time (list-ref x 4)))
                             (>= time 12))))
    (weekday ,(λ (x) (let ((day (list-ref x 5)))
                             (not (member day '("Saturday" "Sunday"))))))
    (weekend ,(λ (x) (let ((day (list-ref x 5)))
                             (member day '("Saturday" "Sunday")))))
    (large ,(λ (x) (let ((size (list-ref x 2)))
                           (>= size 10000))))
    (small ,(λ (x) (let ((size (list-ref x 2)))
                           (<= size 1000))))
    (gif ,(λ (x) (let ((format (list-ref x 6)))
                           (eqv? format 'gif))))
    (html ,(λ (x) (let ((format (list-ref x 6)))
                           (eqv? format 'html))))
    (jpg ,(λ (x) (let ((format (list-ref x 6)))
                           (eqv? format 'jpg))))
    (otherformat ,(λ (x) (let ((format (list-ref x 6))) (not (memv format '(jpg html gif))))))))

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
  (mapquote(map car (filter (λ (x) (equal? (cddr x) `(Set Nats)))
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


(define NASA-EDU-DESC
  '(NASA-EDU
    (host symbol #t)
    (bytes number)
    (date number)
    (time number)
    (day string)
    (format symbol)))

(define NASA-EDU-DATA
  (read-logs NASA-EDU-DESC FILENAME))
(define NASA-EDU-KEYS
  (foldr (λ (x a) (set-cons (cadr x) a)) '() NASA-EDU-DATA))
(define NASA-EDU-AUTOMATON
  (gen-player-automaton
   (if (zero? K) make-grammar-finite (if (= K -1) make-grammar-micro make-grammar-infinite))
   NASA-EDU-DESC
   NASA-EDU-DATA
   (car NASA-EDU-KEYS)))
(define NASA-EDU-FEATURES
  (reverse (take-words NASA-EDU-AUTOMATON
                       (if (< K 1) 20000 (* 1000 K)))))
(define NASA-EDU-HASH (hash-logs NASA-EDU-KEYS 1 NASA-EDU-DATA (make-immutable-hash)))

#|

;; non-recursive grammar
270 total features generated (asked for 3000)
takes less than a second to find, 2 secs to apply.

;; big finite grammar (15,708 features)
 26 seconds to find / 167 seconds to apply
 21                 / 108 seconds to apply

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


(define NASA-EDU-TABLE
   `((name . ,(build-list (length NASA-EDU-FEATURES) (λ (x) (string->symbol (string-append "F" (number->string x))))))
    . ,(remove-inf
        (make-table
         REDUCE-FNS
         FILTER-FNS
         NASA-EDU-KEYS
         NASA-EDU-DESC
         NASA-EDU-DATA
         NASA-EDU-FEATURES
         NASA-EDU-HASH))))
#;
(display-table NASA-EDU-TABLE)
