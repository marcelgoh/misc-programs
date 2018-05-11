(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (identity x) x)
(define (inc n) (+ n 1))
(define (cube n) (* n n n))

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

;term and next are both functions
(define (product term a next b)
  (if (> a b)
    0
    (* (term a)
       (product term (next a) next b))))

