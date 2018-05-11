(define (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define tolerance 0.00001) 
(define (average x y)
  (/ (+ x y) 2))
(define dx 0.00001)

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

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))))

(define golden
  (fixed-point (lambda (y) (+ 1 (/ 1 y)))))

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

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

(define (inc x) (+ x 1))
(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (lambda (x) ((compose f (repeated f (- n 1))) x))))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3.0)))
(define (n-smoothed f n)
  (lambda (x) ((repeated (smooth f) n) x)))

(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))
(define (root x n)
  (fixed-point ((repeated average-damp (floor (/ n 2))) (lambda (y) (/ x (pow y (- n 1)))))))

