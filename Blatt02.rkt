#lang racket
; Markus Lange 6542577
; Tino Raupp 6536321
; Christoph Jahnke 6524846

 
2.1 Fakultät einer Zahl

(define (fakultaet n)
  (if (= n 0)
      (+ 1)
      (* (fakultaet (- n 1)) n)))


; 2.2 Potenzen von Rationalzahlen

(define (power r n)
  (if  (= n 0)
       (+ 1)
       (if (even? n)
           (sqr (power r (/ n 2)))          
           (if (odd? n)
               (* r (power r (- n 1)))
               (error "Iwas falsch gemacht")))))
