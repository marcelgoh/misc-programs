#|
Written by Marcel Goh on 7 Sep 2018.
Uses grade-school arithmetic (carrying and the like) to add and multiply numbers.
Example usage: $ scheme load grade-school.scm
               1 ]=> (add '(7 9 8) '(5 1 4))
               ;Value 1: (1 3 1 2)
               1 ]=> (multiply '(1 0 1) '(9 9))
               ;Value 2: (9 9 9 9)
|#

(define (beautify answer) ; removes leading 0s
  (cond ((null? answer) '())
        ((= (car answer) 0) (beautify (cdr answer)))
        (else answer)))

(define (add addend augend)
  (define (iterate acc carry a1 a2)
    (cond ((null? a1)
           (if (null? a2)
               (cons carry acc)
               (iterate (cons (+ (car a2) carry) acc) 0 a1 (cdr a2))))
          ((null? a2)          ; here we know a1 is not null
           (iterate (cons (+ (car a1) carry) acc) 0 (cdr a1) a2))
          (else (let ((sum (+ (car a1) (car a2))))
                  (if (> sum 9)
                      (iterate (cons (+ (- sum 10) carry) acc) 1 (cdr a1) (cdr a2))
                      (iterate (cons (+ sum carry) acc) 0 (cdr a1) (cdr a2)))))))
  (let ((a1 (reverse addend))   ; we reverse the lists before starting
        (a2 (reverse augend)))
    (beautify (iterate '() 0 a1 a2))))

(define (multiply multiplicand multiplier)
  (define (iterate acc line place-value carry m1 m2)
    ; line is a dumb name but it's the current multiplication we're doing
    ; acc is the overall answer so far (the sum of the lines)
    ; place-value is the next place value (it adds 0s on to line)
    (cond ((null? m2) acc)
          ((null? m1)
           ; we use the multiplicand value from outside the iterate procedure
           ; which feels hacky but works. we also use the add procedure from above
           (iterate (add acc (cons carry line))
                    place-value
                    (cons 0 place-value)
                    0
                    (reverse multiplicand)
                    (cdr m2)))
          (else (let ((prod (+ (* (car m1) (car m2)) carry)))
                  (iterate acc
                           (cons (modulo prod 10) line)
                           place-value
                           (floor (/ prod 10))
                           (cdr m1)
                           m2)))))
  (let ((m1 (reverse multiplicand)) ; reverse before starting
        (m2 (reverse multiplier)))
    (let ((answer (beautify (iterate '() '() '(0) 0 m1 m2))))
      (if (null? answer) '(0) answer))))
