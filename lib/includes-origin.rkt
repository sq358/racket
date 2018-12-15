#lang racket

; Write a function to calculate if the center of C lies within
; the triangle formed by P, Q and R. The input are three real numbers
; between 0 and 360, corresponding to the degrees of each point
; P, Q, and R, around the perimeter of C, you can assume input is
; sorted in increasing order The output is a Boolean corresponding
; to the answer the answer is False if center of C lies in the perimeter
; of the triangle.

(require racket/math)
(require rackunit)

(define-struct posn [x y])
; Posn is a structure:
;  (make-posn Number Number)
; interpretation:
;  (make-posn 10 12) is a point in cartesian plane
;   with x as 10 and y as 12 coordinates


(define-struct angles [a b c])
; Angels is a structure:
;  (make-angels Number Number Number)
; interpretation:
;  (make-angels 0 45 90) represents 3 angels in degrees

(define-struct points [a b c])
; Points is a structure:
;  (make-points Posn Posn Posn)
; interpretation:
;  (make-points (make-posn 3 6)
;               (make-posn 10 4)
;               (make-posn 5 10))
;  is a triangle represented by the 3 points

(define-struct sides [a b c])
; Sides is a structure:
;  (make-sides Number Number Number)
; interpretation:
;  (make-sides 3 4 5)
;  is a triangle represented by the 3 vectrors with magnitudes
;  of 3, 4, and 5

; Number Number Number -> Boolean
; determins if a triangle made from points includes
; surrounding circle's origin


(define (includes-origin? a b c)
  (local (; Sides -> Number
          ; produces Heron's formula's S value
          (define (heron-s s)
            (/ (+ (sides-a s) (sides-b s) (sides-c s))
               2))

          ; Number Number Number -> Points
          ; produces points p with trigonometric representation
          ; of vectors       
          (define (create-points p1 p2 p3)
            (make-points (make-posn (cos p1) (sin p1))
                         (make-posn (cos p2) (sin p2))
                         (make-posn (cos p3) (sin p3))))

          ; Points -> Sides
          ; for the gives points produces the corresponding triangle
          ; sides
          (define (points->sides p)
            (make-sides (side-magnitude (points-a p) (points-b p))
              (side-magnitude (points-a p) (points-c p))
              (side-magnitude (points-b p) (points-c p))))

          ; Number Number -> Number
          ; for the given points produces the side magnitude
          (define (side-magnitude p1 p2)
            (sqrt (+ (abs (- (posn-x p1) (posn-x p2)))
                     (abs (- (posn-y p1) (posn-y p2))))))
          
          (define triangle-sides
            (points->sides (create-points a b c)))
          (define side-a
            (points->sides (create-points 0 a b)))
          (define side-b
            (points->sides (create-points 0 a c)))
          (define side-c
            (points->sides (create-points 0 b c)))
          (define triangle-s
            (heron-s triangle-sides))
          (define s-a
            (heron-s side-a))
          (define s-b
            (heron-s side-b))
          (define s-c
            (heron-s side-b)))
    (= (floor (triangle-area triangle-sides triangle-s))
       (floor (+ (triangle-area side-a s-a)
                          (triangle-area side-b s-b)
                          (triangle-area side-c s-c))))))

; Sides Number -> Number
; For the given sides and Heron's formula's s
; computes the area of a triangle made from the
; the given points

(define (triangle-area t s)
  (sqrt (* s (* (- s (sides-a t))
                (- s (sides-b t))
                (- s (sides-c t))))))

; tests
(check-equal? (includes-origin? 0 90 220) #true)
(check-equal? (includes-origin? 200 270 360) #false)
(check-equal? (includes-origin? 90 180 360) #false)