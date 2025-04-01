;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname politz_event) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))


;; A tree-node is either
;; leaf, or
;; (node value children) where children is a list of tree-nodes


(define (tree-fun tn)
  (cond
    [(leaf? tn) ..]
    [(node? tn)
     ... (value tn) ... (children-fun (children tn)) ... 
     ]))

(define (children-fun tn)
  (cond
    [(empty? tn) ...]
    [(cons? tn)
     ...(tree-fun (first tn)) ... (children-fun (rest tn)) ...]))



;; A list is either:
;; - empty, or
;; - (cons element list)

;; (first l) gets the first element of the list l
;; (rest l) gets the remainder of the list l
;; (cons? l) returns true if l is a cons
;; (empty? l) returns true if l is a empty

#;(define (list-fun lst ...)
  (cond
    [(empty? lst) ...]
    [(cons? lst) ... (first lst) ... (list-fun (rest lst)) ...]))

;; write a function sum that takes a list of numbers
;; and returns their sum
(define (sum lst)
  (cond
    [(empty? lst) 0]
    [(cons? lst) (+ (first lst) (sum (rest lst)))]))

;; write a function largest that takes a list of positive numbers
;; and returns the largest one
(define (largest lst)
  (cond
    [(empty? lst) ...]
    [(cons? lst) (bigger (first lst) (largest (rest lst))]))

(define (bigger num1 num2)
  (cond
    [(< num1 num2) num2]
    [else num1]))

(check-expect (sum (cons 3 (cons 2 empty))) 5)
(check-expect (sum empty) 0)
(check-expect (sum (cons 2 (cons -1 empty))) 1)
(check-expect (sum (cons 3 (cons 2 (cons 3 (cons 2 empty))))) 10)


(define x 10)

(cond
  [(< x 10) (* x 2)]
  [(= x 10) (+ x 1)]
  [(> x 10) (* x 1000)])

;; Write a function pay-with-overtime that takes a number of hours
;; and an hourly rate and produces pay assuming 1.5x for hours over 40
(define (pay-with-overtime hours rate)
  (cond
    [(<= hours 40) (* rate hours)]
    [else (+ (* 1.5 rate (- hours 40)) (* rate 40))]))

(check-expect (pay-with-overtime 0 0) 0)
(check-expect (pay-with-overtime 40 10) 400)
(check-expect (pay-with-overtime 10 10) 100)
(check-expect (pay-with-overtime 60 10) 700)


