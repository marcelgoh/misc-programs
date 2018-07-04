; BINARY SEARCH: Written by Marcel Goh on 3 July 2018.
; Defective: does not run in logarithmic time.

(define (binary item list)        ; returns index of item in list
  (define (nth n list)            ; returns element of list at index n
    (if (= n 0) (car list) (nth (- n 1) (cdr list))))
  (define (before n list)         ; returns sublist before index n
    (if (= n 0) '() (cons (car list) (before (- n 1) (cdr list)))))
  (define (after n list)          ; returns sublist after index n
    (if (= n 0) (cdr list) (after (- n 1) (cdr list))))
  (define (search item list add)  ; add is offset from start of original list
    (let ((mid (floor (/ (length list) 2))))
      (cond ((null? list) false)
            ((= item (nth mid list))
             (+ add mid))
            ((< item (nth mid list))
             (search item (before mid list) add))
            ((> item (nth mid list))
             (search item (after mid list) (+ add mid 1))))))
  (search item list 0))
