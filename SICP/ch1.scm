#|
Chapter 1
Created 15 May 2018 as a compilation of my solutions to some of the problems
in SICP by Abelson and Sussman. Previously spread out over multiple files.
Written by Marcel Goh
|#

; useful procedures
(define (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))
(define (average x y)
  (/ (+ x y) 2))
(define (double x)
  (+ x x))
(define (halve x)
  (/ x 2))

; Exercise 1.3
(define (sum-of-squares a b c)
  (cond ((and (< c a) (< c b))
         (+ (square a) (square b)))
        ((and (< b a) (< b c))
         (+ (square a) (square c)))
        ((and (< a b) (< a c))
         (+ (square b) (square c)))))

; Exercise 1.7
(define (newton-cb x)
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (define (improve guess)
    (/ (+ (/ x
             (square guess))
          (* guess 2))
       3))
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  (try 1.0))

; Exercise 1.10
; ack(1,10) = 1024, ack(2,4) = 65536, ack(3,3) = 65536
(define (ack x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (ack (- x 1) (ack x (- y 1))))))

; Exercise 1.11
(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))
(define (f-iter n)
  (define (iter a b c count)
    (if (< count 3)
        c
        (iter b
              c
              (+ c (* 2 b) (* 3 a))
              (- count 1))))
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 2)
        (else (iter 0 1 2 n))))

; Exercise 1.17
(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? a) (double (fast-mult (halve a) b)))
        (else (+ a (fast-mult a (- b 1))))))

; Exercise 1.18
(define (russ-peas a b)
  (define (mult-iter total a count)
    (cond ((= count 0) total)
          ((even? count) (mult-iter total (double a) (halve count)))
          (else (mult-iter (+ total a)
                           a
                           (- count 1)))))
  (mult-iter 0 a b))

; Exercise 1.19 (This one was pretty hard!)
(define (log-fib n)
  (define (iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (iter a
                 b
                 (+ (square p) (square q))
                 (+ (* p q) (* q (+ p q)))
                 (/ count 2)))
          (else (iter (+ (* b q) (* a q) (* a p))
                      (+ (* b p) (* a q))
                      p
                      q
                      (- count 1)))))
  (iter 1 0 0 1 n))

; Chapter 1.2.6 TESTING FOR PRIMALITY EXAMPLES
(define (divides? a b) (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
           (square (expmod base (/ exp 2) m))
           m))
        (else
          (remainder
            (* base (expmod base (- exp 1) m))
            m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; Chapter 1.3.1 PROCEDURES AS ARGUMENTS EXAMPLES
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (identity x) x)
(define (inc n) (+ n 1))

(define (sum-integers a b)
  (sum identity a inc b))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (pi-term a)
  (/ 1.0 (* a (+ a 2))))
(define (pi-next a)
  (+ a 4))
(define (pi-sum a b)
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx)) 
          b)
     dx))

; Exercise 1.29
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((odd? k) 4)
             ((or (= k 0) (= k n)) 1)
             ((even? k) 2))
       (y k)))
  (/ (* h (sum term 0 inc n)) 3))

; Exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

; Exercise 1.31
(define (product term a next b)
  (if (> a b)
    0
    (* (term a)
       (product term (next a) next b))))

; FIXED POINT EXAMPLES
(define tolerance 0.00001) 
(define dx 0.00001)

(define (average-damp f)
  (lambda (x) (average x (f x))))

; Exercise 1.46 START
(define (iterative-improve good-enough? improve-guess)
  (define iter-imp
    (lambda (guess)
      (if (good-enough? guess)
          guess
          (iter-imp (improve-guess guess)))))
  (lambda (y) (iter-imp y)))

(define (close-enough? f)
  (lambda (x) (< (abs (- x (f x))) tolerance)))

(define (fixed-point f)
  (iterative-improve (close-enough? f)
                     (lambda (x) (f x))))

(define (sqrt x)
  ((fixed-point (average-damp (lambda (y) (/ x y)))) 
  1.0))
; END

(define (cube-root x)
  ((fixed-point (average-damp (lambda (y) (/ x (square y)))))
  1.0))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; Exercise 1.35
(define golden
  (fixed-point (lambda (y) (+ 1 (/ 1 y)))))

; Exercise 1.37
; n and d are functions, k is a constant
(define (cont-frac n d k)
  (if (= k 0)
      0
      (/ (n k)
         (+ (d k)
            (cont-frac n d (- k 1))))))

(define (recip-phi k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

; Exercise 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

; Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

; Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

; Exercise 1.43
(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (lambda (x) ((compose f (repeated f (- n 1))) x))))

; Exercise 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3.0)))
(define (n-smoothed f n)
  (lambda (x) ((repeated (smooth f) n) x)))

; Exercise 1.45
(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))
(define (root x n)
  ((fixed-point ((repeated average-damp (floor (/ n 2))) (lambda (y) (/ x (pow y (- n 1))))))
  1.0))

