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
             (if (null? xs) 0 (/ (foldr + 0 xs) (length xs))))
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

(define (make-grammar desc table i)
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
  (make-grammar desc table name))

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
    [(? Value? v) (if (not (list? v)) (f `(,v)) (f v))]
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
     `(,e . ,(map f (map cdr (cdr o))))]
    [`(,(? Label? e)
       (,(? Label? l1) ,(? Label? l2) ,(? Value? vs) ...))
     `(,e . ,(f vs))]
    [`(,(? Label? e)
       (,(? Label? l1) ,(? Label? l2) ,(? Value? vs) ...)
       ...)
     `(,e . ,(map f (map cddr (cdr o))))]
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
    [else (error (format "Invalid word ~s" w))]))

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
   (λ (x) (map (λ (x) (if (equal? x +inf.0) 9999999 x)) x))
   table))


#|
1032015.ksc.nasa.gov,1,2508.473159,99317214888580944,12542.365797,1179457804470039125445135076391143994403110627418513788456134577524638244013809020625088048594070799228916758321596922998804055100897898775328271771811119887225184444738610069701004227360529741974150020951330506312401484979997608462907084084418546495389696,9999999,5897289022350195627225675381955719972015553137092568942280672887623191220069045103125440242970353996144583791607984614994020275504489493876641358859055599436125922223693050348505021136802648709870750104756652531562007424899988042314535420422092732476948480,138015.6,37481840176318991109718016,690078,3620.399767,9999999,517717.166667,4756448523.86014,9999999,680172138912,4825.72028,9999999,690078,4786.333333,232238736134129352321748855215859255214080,57436,18388802635447378049177619468170671792414102623181179689336039511619522357723372523121778740287627089857194873149603761703366399883385593064122696679970312083662660849106944,9999999,220665631625368522860536113356828631545167633315387721733161873852823449503753551906374978088139420833167057154886135185847774015638911042525496360726356119855895347959169024,57506.5,778631235435087256512516213052311127667532079671803904,690078,2509.374545,2509.374545,2509.374545,9999999,9999999,9999999,690078,690078,690078,55,377395200,275,55,377395200,275,55,377395200,275,55,377395200,275,1.923077,52750291955504401406433398095872,275,1.923077,52750291955504401406433398095872,275,1.923077,52750291955504401406433398095872,275,1.923077,52750291955504401406433398095872,275,22.916667,3352719052800,275,22.916667,3352719052800,275,22.916667,3352719052800,275,22.916667,3352719052800,275,275,275,275,275,275,275,275,275,275,275,275,275,5,143,1,12,5,143,1,12,2509.374545,9999999,690078,275,275,275,275
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; different log data descriptions, readers, tables, keys, and features

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

(define NASA-FEATURES (time (take-words NASA-AUTOMATON 1000)))

(define NASA-TABLE
  (cons (cons 'name(build-list
         (length NASA-FEATURES)
         (λ (x) (string->symbol (string-append "F" (number->string x))))))
        (remove-inf
   (make-table 'NASA NASA-KEYS NASA-DESC NASA-DATA NASA-FEATURES))))


(display-table NASA-TABLE)




