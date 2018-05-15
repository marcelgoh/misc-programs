#|
Work-in-progress as of 15 May 2018
Written by Marcel Goh
Solutions to Chapter 2.1 of SICP (Abelson and Sussman)
|#

; useful procedures
(define (abs n) (if (< n 0) (* n -1) n))
(define (remainder a b) (- a (* b (floor (/ a b)))))
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (average x y) (/ (+ x y) 2))
(define (pow x n)
  (if (= n 1)
      x
      (* x (pow x (- n 1)))))

; Exercise 2.1
(define (make-rat n d)
  (let ((g (abs (gcd n d)))
        (sign (if (< (/ n d) 0) -1 1)))
    (cons (* sign (/ (abs n) g)) (/ (abs d) g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer x) (denom x))))

; Exercise 2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment s)
  (define x
    (average (x-point (start-segment s))
             (x-point (end-segment s))))
  (define y
    (average (y-point (start-segment s))
             (y-point (end-segment s))))
  (make-point x y))

; Exercise 2.3: create rectangles using 2 points as diagonals
(define (make-rect p1 p2)
  (let ((xsmall (if (< (x-point p1) (x-point p2)) (x-point p1) (x-point p2)))
        (xlarge (if (< (x-point p1) (x-point p2)) (x-point p2) (x-point p1)))
        (ysmall (if (< (y-point p1) (y-point p2)) (y-point p1) (y-point p2)))
        (ylarge (if (< (y-point p1) (y-point p2)) (y-point p2) (y-point p1))))
    (cons (cons xsmall ysmall) (cons xlarge ylarge))))
(define (small-diag r) (car r))
(define (large-diag r) (cdr r))

(define (print-rect r)
  (print-point (small-diag r))
  (print-point (large-diag r)))
(define (width-rect r)
  (- (x-point (large-diag r))
     (x-point (small-diag r))))
(define (height-rect r)
  (- (y-point (large-diag r))
     (y-point (small-diag r))))
(define (perim-rect r)
  (* 2 (+ (width-rect r) (height-rect r))))
(define (area-rect r)
  (* (width-rect r) (height-rect r)))

; Exercise 2.4: should be equivalent to cons, car, cdr
(define (p-cons x y)
  (lambda (m) (m x y)))
(define (p-car z)
  (z (lambda (p q) p)))
(define (p-cdr z)
  (z (lambda (p q) q)))

; Exercise 2.5: pair = cons, first = car, second = cdr
(define (pair a b)
  (* (pow 2 a) (pow 3 b)))
; keep dividing by 3 until no longer divisible by 3
(define (isolate-two-a n)
  (if (= (remainder n 3) 0)
      (isolate-two-a (/ n 3))
      n))
; keep dividing by 2 until even
(define (isolate-three-b n)
  (if (= (remainder n 2) 0)
      (isolate-three-b (/ n 2))
      n))
; usage: initialize ans at 0, only perfect powers work
(define (logb base x ans)
  (if (< (/ x base) 1)
      ans
      (logb base (/ x base) (+ ans 1))))
(define (first n)
  (logb 2 (isolate-two-a n) 0))
(define (second n)
  (logb 3 (isolate-three-b n) 0))

; Exercise 2.6
(define (church-to-int c)
  ((c (lambda (x) (+ x 1))) 0))
(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define add
  (lambda (f g) (lambda (n) (lambda (x) ((f n) ((g n) x))))))
(define multiply
  (lambda (f g) (lambda (x) (f (g x)))))

; Exercise 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound interval)
  (car interval))
(define (upper-bound interval)
  (cdr interval))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
#|
Omitted portion of 2.11 as it is unwieldy and probably works slower.
These are the 9 test cases and their values for x * y
 x    y         x * y
l u  l u       l     u
- -  - -     lx*ly ux*uy
- +  - -     ux*uy lx*uy
+ +  - -     ux*uy lx*ly

- -  - +     ux*uy ux*ly
- +  - +         ***
+ +  - +     ux*ly ux*uy

- -  + +     ux*uy lx*ly
- +  + +     lx*uy ux*uy
+ +  + +     lx*ly ux*uy

*** min(ux*ly lx*uy) max(lx*ly ux*uy)
|#
(define (div-interval x y)                             ; Exercise 2.10
  (if (and (equal? (> (upper-bound y) 0)
                        (> (lower-bound y) 0))
           (and (not (equal? (upper-bound y) 0))
                (not (equal? (lower-bound y) 0))))
      (mul-interval
        x
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y))))
      ((display "Error.")
       (newline))))

; Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; Exercise 2.9
(define (width-interval interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

; Exercise 2.11
(define (make-centre-width c w)
  (make-interval (- c w) (+ c w)))
(define (centre i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; Exercise 2.12
(define (make-centre-percent c p)
  (define decimal (/ p 100.0))
  (make-interval (- c (* decimal c))
                 (+ c (* decimal c))))
(define (percent i)
  (* (/ (- (centre i) (lower-bound i)) (centre i)) 100))
