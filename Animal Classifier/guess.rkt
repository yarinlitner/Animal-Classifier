;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname guess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Yarin Litner (Student ID: 20909301)
;; CS 135 Fall 2020
;; Assignment 07, Problem 2
;; ***************************************************
(require "animals.rkt")
;; An Example is a (cons Sym (listof Sym))
;; Requires: each attribute in the rest is unique

;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)

;; Helpers
;; (find sym lst) consumes a Symbol and produces true if that symbol exists in
;;    a (listof Sym), lst.
;; Examples:
(check-expect (find 'b (list 'a 'b 'c)) true)

;; find: Sym (listof Sym) -> Bool 
(define (find sym lst)
            (cond [(empty? lst) false]
                  [(symbol=? sym (first lst)) true]
                  [else (find sym (rest lst))]))

;; (flatten lst) consumes a (nested-listof Example) and produces a (listof Sym)
;;     with all labels removed
;; Examples:
(check-expect (flatten (list (list 'ape 'big 'strong))) (list 'big 'strong))

;; flatten: (nested-listof Examples) -> (listof Sym)
(define (flatten lst)
            (cond [(empty? lst) empty]
                  [else (append (rest (first lst)) (flatten (rest lst)))]))

;; Constants for tests
(define seen
(list
(list 'squirrel 'small 'angry)
(list 'goose 'large 'swims 'flies 'angry)
(list 'goose 'large 'swims 'flies 'angry)
(list 'crow 'medium 'flies 'angry)))

(define seen2
(list
(list 'squirrel)))

(define seen3
(list
(list 'ape 'big 'strong)
(list 'penguin 'big 'strong)))

(define seen4
(list
(list 'pig)
(list 'cat 'small)))

;; a)

;; (collect-attributes lst) consumes a (listof Example) and produces a list
;;     of attributes contained in the examples with no duplicates.
;; Examples:
(check-expect (collect-attributes seen)
(list 'medium 'flies 'swims 'large 'angry 'small))
(check-expect (collect-attributes seen4) (list 'small))

;; collect-attributes: (listof Example) -> (listof Sym)  
(define (collect-attributes lst)
  (local [(define (remove-dup lst1 lst2)
            (cond [(empty? lst1) lst2]
                  [(find (first lst1) lst2) (remove-dup (rest lst1) lst2)]
                  [else (remove-dup (rest lst1) (cons (first lst1) lst2))]))]
    (remove-dup (flatten lst) empty)))

;; Tests:
(check-expect (collect-attributes seen2) empty)
(check-expect (collect-attributes seen3) (list 'strong 'big))

;; b)

;; (split-examples examples symbol) splits the (listof Example), examples, on
;;    the given Symbol, symbol, and produces a list of two lists of examples,
;;    with the first containing the examples containing the symbol and the
;;    second containing the examples not containing the symbol.
;; Examples:
(check-expect (split-examples seen 'goose) ; splitting on a label
(list
 (list
  (list
   'goose
   'large
   'swims
   'flies
   'angry)
  (list
   'goose
   'large
   'swims
   'flies
   'angry))
 (list
  (list 'squirrel 'small 'angry)
  (list 'crow 'medium 'flies 'angry))))
(check-expect (split-examples seen 'small) ; splitting on an attribute
(list
 (list (list 'squirrel 'small 'angry))
 (list
  (list
   'goose
   'large
   'swims
   'flies
   'angry)
  (list
   'goose
   'large
   'swims
   'flies
   'angry)
  (list 'crow 'medium 'flies 'angry))))

;; split-examples: (listof Example) Sym ->
;;                                      (list (listof Example) (listof Example))
(define (split-examples examples symbol)
  (local [(define (contains-sym examples symbol containing)
            (cond [(empty? examples) empty]
                  [(equal? (find symbol (first examples)) containing)
                   (cons (first examples)
                         (contains-sym (rest examples) symbol containing))]
                  [else (contains-sym (rest examples) symbol containing)]))]
    (list (contains-sym examples symbol true)
          (contains-sym examples symbol false))))

;; Tests:
(check-expect (split-examples seen 'angry)
(list seen empty))
(check-expect (split-examples empty 'angry) (list empty empty))
(check-expect (split-examples seen 'elephant) (list empty seen))

;; c)

;; (histogram examples) consumes a (listof Example), examples, and produces a
;;     list of attribute/count pairs, with each pair indicating how many times
;;     that attribute appears in the examples
;; Examples:
(check-expect (histogram seen)
(list
 (list 'medium 1)
 (list 'flies 3)
 (list 'swims 2)
 (list 'large 2)
 (list 'angry 4)
 (list 'small 1)))
(check-expect (histogram seen3) (list (list 'strong 2) (list 'big 2)))

;; histogram: (listof Example) -> Histogram
(define (histogram examples)
  (local [(define (count sym lst)
            (cond [(empty? lst) 0]
                  [(equal? sym (first lst)) (add1 (count sym (rest lst)))]
                  [else (count sym (rest lst))]))
          (define (pair lst)
            (cond [(empty? lst) empty]
                  [else (cons (list (first lst)
                                    (count (first lst) (flatten examples)))
                              (pair (rest lst)))]))]
    (pair (collect-attributes examples))))

;; Tests:
(check-expect (histogram empty) empty)
(check-expect (histogram seen2) empty)
(check-expect (histogram seen4) (list (list 'small 1)))

;; d)

;; (augment-histogram histogram attributes total) consumes a Histogram,
;;    histogram, a (listof Sym), attributes, and Num, total. It produces
;;    an AH with any missing elements.
;; Examples:
(check-expect
(augment-histogram
(list (list 'a 100) (list 'c 50))
(list 'a 'b 'c)
200)
(list (list 'a 100 100) (list 'b 0 200) (list 'c 50 150)))
(check-expect
(augment-histogram
(list (list 'a 100))
(list 'a 'b)
150)
(list (list 'a 100 50) (list 'b 0 150)))

;; augment-histogram: Histogram (listof Sym) Num -> AH
;; Requires:  total will be greater than or equal to any
;;    value in the histogram. Also, all attributes in histogram should appear
;;    attributes.
(define (augment-histogram histogram attributes total)
  (local [(define (search-hist item histogram total)
  (cond [(empty? histogram) (list item 0 total)]
        [(equal? (first (first histogram)) item)
         (list item
               (second (first histogram))
               (- total (second (first histogram))))]
        [else (search-hist item (rest histogram) total)]))]
    (cond [(empty? attributes) empty]
          [else (cons (search-hist(first attributes) histogram  total)
                      (augment-histogram histogram (rest attributes) total))])))

;; Tests:
(check-expect
(augment-histogram empty (list 'x 'y) 10)
(list (list 'x 0 10) (list 'y 0 10)))
(check-expect
(augment-histogram
(list
(list 'a 100) (list 'b 200) (list 'c 300))
(list 'a 'b 'c)
300)
(list (list 'a 100 200) (list 'b 200 100) (list 'c 300 0)))

;; e)
;; (entropy positive-counts negative-counts) consumes two elements from
;;     augmented histograms, (positive-counts) and (negative-counts), and
;;     produces their entropy. 
;; Examples:
(check-within
(entropy (list 'large 126 59) (list 'large 146 669)) 0.5663948489858 0.001)
(check-within
(entropy (list 'small 17 168) (list 'small 454 361)) 0.5825593868115 0.001)
(check-within (entropy (list 'a 0 100) (list 'b 100 0)) 0.0 0.001)

;; entropy: (list Sym Nat Nat) (list Sym Nat Nat) -> Num
;; Requires: Symbols in both arguments should be the same
(define (entropy positive-counts negative-counts)
  (local [(define a (second positive-counts))
          (define b (second negative-counts))
          (define c (third positive-counts))
          (define d (third negative-counts))
          (define (P n m)
            (cond [(> (+ n m) 0) (/ n (+ n m))]
                  [(= (+ n m) 0) 0.5]))
          (define (e p)
            (cond [(and (< 0 p) (<= p 1)) (* -1 p (log-v2 2 p))]
                  [(= p 0) 0]))
          (define (log-v2 base num)
            (/ (log num) (log base)))]
    (+ (* (P (+ a b) (+ c d)) (+ (e (P a b)) (e (P b a))))
       (* (P (+ c d) (+ a b)) (+ (e (P c d)) (e (P d c)))))))

; f)

;; (entropy-attributes positive negative) consumes two augmented histograms,
;;    positive and negative, and computes the entropy of each attribute,
;;    producing a list of attribute/entropy pairs.
;; Examples:
(check-within 
(entropy-attributes
(list
(list 'large 126 59) (list 'angry 161 24)
(list 'small 17 168) (list 'flies 170 15)
(list 'swims 162 23) (list 'medium 42 143))
(list
(list 'large 146 669) (list 'angry 469 346)
(list 'small 454 361) (list 'flies 615 200)
(list 'swims 365 450) (list 'medium 215 600)))
(list
(list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
(list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
(list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677)) 0.001)

;; entropy-attributes: AH AH -> EAL
;; Requires: attributes should be included in both augmented histograms and
;;      should appear in the same order
(define (entropy-attributes positive negative)
  (cond [(empty? positive) empty]
        [else (cons (list (first (first positive))
                          (entropy (first positive) (first negative)))
                    (entropy-attributes (rest positive) (rest negative)))])) 

;; g)

;; (best-attribute entropies) consumes a non-empty list of attribute/entropy
;;     pairs, entropies, and produces the attribute with the minimum entropy,
;;     or any such attribute if somehow there’s a tie
;; Examples:
(check-expect (best-attribute
(list
(list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
(list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
(list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677))) 'large)
(check-expect (best-attribute
(list
(list 'large #i0.5663948489858) (list 'angry #i0.5663948489858)
(list 'small #i0.5825593868115))) 'large)

;; best-attribute: EAL -> Sym
;; Requires: entropies must be non-empty
(define (best-attribute entropies)
  (local [(define (find-smallest line al)
            (cond[(empty? al) line]
                 [(smaller-line line (first al))
                  (find-smallest line (rest al))]
                 [else (find-smallest (first al) (rest al))]))
          (define (smaller-line line1 line2)
            (cond[(<= (second line1) (second line2)) true]
                 [else false]))]
    (first (find-smallest (first entropies) (rest entropies)))))

;; Tests:
(check-expect (best-attribute (list (list 'a 100))) 'a)
(check-expect (best-attribute (list (list 'a 100) (list 'b 100))) 'a)
(check-expect (best-attribute (list (list 'a 100) (list 'b 200) (list 'c 300)))
              'a)
(check-expect (best-attribute (list (list 'a 300) (list 'b 200) (list 'c 100)))
              'c)
(check-expect (best-attribute (list (list 'a 300) (list 'b 200) (list 'c 300)))
              'b)

; h)
;; (build-dt examples label) consumes a (listof Example), examples, and a
;;    Sym, label, and produces a DT based on these arguments

;; build-dt: (listof Example) Sym -> DT
(define (build-dt examples label)
  (local [(define lst-attribute (collect-attributes examples))
          (define split-list (split-examples examples label))
          (define positive (first split-list))
          (define negative (second split-list))
          (define pos-length (length positive))
          (define neg-length (length negative))]
    (cond [(empty? positive) false]
          [(empty? negative) true]
          [(and (empty? lst-attribute) (> pos-length neg-length))
           true]
          [(empty? lst-attribute) false]
          [else
           (local [(define root (best-attribute
                                 (augment-histogram examples
                                                    lst-attribute
                                                    (+ pos-length
                                                       neg-length))))
                   (define split-lst-by-attr (split-examples examples root))
                   (define list-with-root (first split-lst-by-attr))
                   (define list-without-root (second split-lst-by-attr))
                   (define (remove-root example attr)
                     (cond [(empty? example) empty]
                           [(symbol=? (first example) attr) (rest example)]
                           [else 
                            (cons (first example)
                                  (remove-root (rest example) attr))]))
                   (define (remove-root/list lst attr)
                     (cond
                       [(empty? lst) empty]
                       [(member? attr (first lst))
                        (cons (remove-root (first lst) attr)
                              (remove-root/list (rest lst) attr))]
                       [else
                        (cons (first lst)
                              (remove-root/list (rest lst) attr))]))]
             (local [(define left
                       (build-dt (remove-root/list list-with-root root) label))
                     (define right
                       (build-dt list-without-root label))]
               (cond [(equal? left right) left]
                     [else
                      (list root left right)])))])))
;; i)
;; (train-classifier examples label) consumes a (listof Example), examples and
;;    a Sym, label, and produces a predicate to recognize items matching the
;;    label based on their attributes. 

;; train-classifier: (listof Example) Sym -> ((listof Sym) -> Bool)
(define (train-classifier examples label)
  (local [(define d-tree (build-dt examples label))
          (define (decision? attributes) (find-in-tree d-tree attributes))
          (define (find-in-tree tree attributes)
            (cond [(boolean? tree) tree]
                  [(find (first tree) attributes)
                   (find-in-tree (second tree) attributes)]
                  [else (find-in-tree (third tree) attributes)]))] decision?))

;; Tests:
(define goose? (train-classifier (random-animals 1000) 'goose))
(check-expect (goose? (list 'large 'angry 'flies 'swims)) true)
(check-expect (goose? (list 'small 'angry)) false)
(define squirrel? (train-classifier (random-animals 1000) 'squirrel))
(check-expect (squirrel? (list 'large 'angry 'flies 'swims)) false)
(check-expect (squirrel? (list 'small 'angry)) true)
(define crow? (train-classifier (random-animals 1000) 'crow))
(check-expect (crow? (list 'angry 'flies 'medium)) true)

;; Bonus:
(define (performance classifier? examples label)
  (local
    [(define split (split-examples examples label))
     (define pos-split (first split))
     (define neg-split (second split))
     (define pos-examples pos-split)
     (define neg-examples neg-split)
     (define pos-count (length pos-split))
     (define neg-count (length neg-split))
     (define (calc-percent num den)
      (round (* (/ num den) 100)))
     (define (sensitivity lst) (local
                       [(define (sym-classifier? element)
                          (classifier? (rest element)))]
                       (length (filter sym-classifier? lst))))
     (define (specificity lst)
                     (local
                       [(define (not-classifier? element)
                          (not (classifier? (rest element))))]
                       (length (filter not-classifier? lst))))]
    (cond
       [(empty? examples) (list label 0 0)]
       [(zero? pos-count) (list label 0 (calc-percent (specificity
                                                       neg-examples)
                                                      neg-count))]
       [(zero? neg-count) (list label (calc-percent (sensitivity pos-examples)
                                                    pos-count) 0)] 
       [else (list label (calc-percent (sensitivity pos-examples)
                                       pos-count) (calc-percent
                                     (specificity neg-examples) neg-count))])))