#|
Written by Marcel Goh on 11 May 2018.
From Reddit's Daily Programmer Challenge #239 [Easy]: A Game of Threes
Usage: $ scheme load game-of-threes.scm
       1 ]=> (threes <number>)
Procedure: Add or subtract 1 to number to make it divisible, then divide by 3
           Repeat until 1
           (If <number> <= 1, the program simply returns the value.)
|#

(load-option 'format)

; define helper variables
(define (rem n) 
  (remainder n 3))
(define (diff rem)
  (if (= rem 2)
      1
      (- rem)))

; always return n, display depends on whether we have reached final value
(define (show n)
  (if (<= n 1)
      (display n)
      (format
        #t
        "~10A ~@2A~%" n (diff (rem n))))
  n)

; recursively work until 1 is reached
(define (threes n)
  (if (<= n 1)
      (show n)
      (threes (/ (+ (show n) (diff (rem n))) 3))))
