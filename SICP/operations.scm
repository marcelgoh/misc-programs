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
  
(define (newton-sq x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  (try 1.0))

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

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define (ack x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (ack (- x 1) (ack x (- y 1))))))

(define (expnt b n)
  (if (= n 0)
      1
      (* b (expnt b (- n 1)))))

(define (fast-expnt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expnt b (/ n 2))))
        (else (* b (fast-expnt b (- n 1))))))

(define (succ-sq b n) (expnt-iter 1 b n))

(define (expnt-iter total base power)
  (cond ((= power 0) total)
        ((even? power) (expnt-iter total (square base) (halve power)))
        (else (expnt-iter (* total base)
                          base
                          (- power 1)))))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? a) (double (fast-mult (halve a) b)))
        (else (+ a (fast-mult a (- b 1))))))

(define (russ-peas a b) (mult-iter 0 a b))

(define (mult-iter total a count)
  (cond ((= count 0) total)
        ((even? count) (mult-iter total (double a) (halve count)))
        (else (mult-iter (+ total a)
                         a
                         (- count 1)))))

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


