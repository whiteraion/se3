#lang racket
; Aufgabe 2.3
; Übergebe Gradzahl und erhalte Himmelsrichtung
(define (grad->himmelsrichtung x)
  (cond
    ((<= x 11.25) "N")
    ((<= x 33.75) "NNE")
    ((<= x 56.25) "NE")
    ((<= x 78.75) "ENE")
    ((<= x 101.25) "E")
    ((<= x 123.75) "ESE")
    ((<= x 146.25) "SE")
    ((<= x 168.75) "SSE")
    ((<= x 191.25) "S")
    ((<= x 213.75) "SSW")
    ((<= x 236.25) "SW")
    ((<= x 258.75) "WSW")
    ((<= x 281.25) "W")
    ((<= x 303.75) "WNW")
    ((<= x 326.25) "NW")
    ((<= x 348.75) "NNW")
    ((<= x 360) "N")
     ))

(define (himmelsrichtung->grad x)
    (cond
    ((equal? x "N") 0)
    ((equal? x "NNE") 22.5)
    ((equal? x "NE") 45)
    ((equal? x "ENE") 67.5)
    ((equal? x "E") 90)
    ((equal? x "ESE") 112.5)
    ((equal? x "SE") 135)
    ((equal? x "SSE") 157.5)
    ((equal? x "S") 180)
    ((equal? x "SSW") 202.5)
    ((equal? x "SW") 225)
    ((equal? x "WSW") 247.5)
    ((equal? x "W") 270)
    ((equal? x "WNW") 292.5)
    ((equal? x "NW") 315)
    ((equal? x "NNW") 337.5)
    ((equal? x "N") 360)
     ))
     

(define directions '("N" "NNE" "NE" "ENE" "E" "ESE" "SE" "SSE" "S" "SSW" "SW" "WSW" "W" "WNW" "NW" "NNW"))

(define (deg->direction deg) ; Grad / 22,5 = Index
  (list-ref directions (inexact->exact (round (/ deg 22.5)))))

(define (direction->deg dir) ; Index * 22,5 = Grad
  (~a (* 22.5
     (let loop ((dict directions)
                (index 0))
      (cond ((equal? (first dict) dir) index)
             (else (loop (rest dict) (+ index 1)))))) "°"))
