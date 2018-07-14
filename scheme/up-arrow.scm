#| r/dailyprogrammer Challenge #365 [Easy] Up-arrow Notation
 | Written by Marcel Goh on 14 July 2018
 |#

; computes a [up-arrow]^n b where n = number of arrows
; a, n and b are non-negative integers
(define (up-arrow a n b)
  ; exponentiation helper function: initialize acc to 1
  (define (pow a b acc)
    (if (= b 0)
        acc
        (pow a (- b 1) (* acc a))))
  (cond ((= n 1) (pow a b 1))
        ((= b 0) 1)
        (else
          (up-arrow a (- n 1) (up-arrow a n (- b 1))))))

