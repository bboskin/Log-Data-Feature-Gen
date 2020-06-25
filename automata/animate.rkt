#lang racket

(require "features.rkt"
         )



(struct Label [v] #:transparent)

;;;;;;;;;;;;;;;;
;; animation

(require 2htdp/image)
(require 2htdp/universe)

(define SQUARE-SIZE 5)
(define SCREEN-SIZE 1000)
(define (draw-value v)
  (cond
    [(number? v) (square SQUARE-SIZE "solid" "yellow")]
    [(symbol? v) (square SQUARE-SIZE "solid" "red")]
    [else (square SQUARE-SIZE "solid" "blue")]))

(define (draw-label _)
  (rectangle SQUARE-SIZE (* 2 SQUARE-SIZE) "solid" "brown"))

(define (draw-blank)
  (square SQUARE-SIZE "solid" "white"))
(define (draw-CDR v)
  (square SQUARE-SIZE "solid" "black"))

(define (draw-tree T)

  (match T
    [#f empty-image]
    ['() empty-image]
    [(? Value? v) (draw-value v)]
    [(? CDR? c) (draw-CDR c)]
    [`(,(? Label? e) . ,T)
     (above (draw-label e)
            (draw-tree T))]
    [`(,T ...)
     (foldr (λ (x a)
              (beside (draw-tree x) (draw-blank) a))
            empty-image
            T)]))

(define (step/draw W)
  (match W
    [`(,ls ()) (list ls '())]
    [`(,ls (,(? filterword? f) . ,w))
     (let ((t (string->symbol (substring (symbol->string f) 6))))
       (list (Filter t ls) w))]
    [`(,ls (,(? mapword? f) . ,w))
     (let ((t (string->symbol (substring (symbol->string f) 3))))
       (list ((Map t) ls) w))]
    [`(,ls (,(? selectword? f) . ,w))
     (let ((t (string->symbol (substring (symbol->string f) 6))))
       (list ((Select t) ls) w))]
    [`(,ls (,(? reduceword? f) . ,w))
     (let ((f (tag->function (string->symbol (substring (symbol->string f) 6)))))
       (list ((Reduce f) ls) w))]
    [else (error "Invalid word")]))


(define (draw-word w)
  (text
   (foldr (λ (x a) (string-append (symbol->string x) "" a)) "" w)
   12
   "black"))

(define (animate-eval w)
  (car (big-bang `(,CDRs ,w)
    [on-key (λ (x i) (step/draw x))]
    [to-draw (λ (x)
               (overlay
                (above
                 (draw-word (cadr x))
                 (rectangle 50 50 "solid" "white")
                 (draw-tree (car x)))
                (empty-scene SCREEN-SIZE SCREEN-SIZE)))])))