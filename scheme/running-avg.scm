; Given:    An integer n and a list of numbers l
; Output:   A list of the running averages of the last n
;           numbers at each point.
; Example:  (run-avg 3 '(2 3 4 5 6)) gives '(2 2.5 3 4 5)

; a queue of numbers that is capped at n values
; all operations in constant time
(define (capped-queue n)
  (let ((cap n)  ; cap must be strictly greater than 0
        (front-ptr '())
        (rear-ptr '())
        (size 0)
        (sum 0.0))
    (define (dispatch m)
      (cond ((eq? m 'empty-capped?)
             (null? front-ptr))
            ((eq? m 'avg-capped)
             (/ sum size))
            ((eq? m 'insert-capped!)
             (lambda (item)
               (let ((new-item (cons item '())))
                 (cond ((null? front-ptr)
                        (set! front-ptr new-item)
                        (set! rear-ptr new-item)
                        (set! size (+ size 1))
                        (set! sum (+ sum item)))
                       ((= size cap)
                        (set! sum (- sum (car front-ptr)))
                        (set! front-ptr (cdr front-ptr))
                        (set-cdr! rear-ptr new-item)
                        (set! rear-ptr new-item)
                        (set! sum (+ sum item)))
                       (else
                        (set-cdr! rear-ptr new-item)
                        (set! rear-ptr new-item)
                        (set! sum (+ sum item))
                        (set! size (+ size 1)))))))))
    dispatch))
(define (empty-capped? queue)
  (queue 'empty-capped?))
(define (avg queue)
  (if (empty-capped? queue)
      0
      (queue 'avg-capped)))
(define (insert-capped! queue item)
  ((queue 'insert-capped!) item))

; O(m) where m is the size of list l
(define (run-avg n l)
  (let ((queue (capped-queue n)))
    (define (iter acc left)
      (if (null? left)
          (reverse acc)
          (begin (insert-capped! queue (car left))
                 (iter (cons (avg queue) acc) (cdr left)))))
    (iter '() l)))
