#| Solutions to some exercises from Chapter 2.2 of SICP by Abelson and Sussman
 | Written by Marcel Goh, last updated 16 May 2018
 |#

; Exercise 2.17
(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

; Exercise 2.18 (iterative)
(define (my-reverse items)
  (define (rev-iter new old)
    (if (null? (cdr old))
        (cons (car old) new)
        (rev-iter (cons (car old) new)
                  (cdr old))))
  (rev-iter '() items))

; Exercise 2.19
(define (cc amount coin-values)
  (define (first-denomination coins) (car coins))
  (define (except-first-denomination coins) (cdr coins))
  (define (no-more? coins) (null? coins))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))

; Exercise 2.20 (iterative)
(define (same-parity n . rest)
  (define (same-iter n new rest)
    (if (null? rest)
        new
        (same-iter n
                   (if (equal? (even? n) (even? (car rest)))
                       (append new (list (car rest)))
                       new)
                   (cdr rest))))
  (same-iter n (list n) rest))

; Exercise 2.21
(define (cond-square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (cond-square-list (cdr items)))))
(define (map-square-list items)
  (map square items))

#| Exercise 2.25:
 | 1. (car (cdr (car (cdr (cdr first)))))
 | 2. (car (car second))
 | 3. (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr third))))))))))))
 |#

; Exercise 2.27
(define (deep-reverse items)
  (cond ((null? items) '())
        ((pair? (car items))
         (append (deep-reverse (cdr items)) (list (deep-reverse (car items)))))
        (else (append (deep-reverse (cdr items)) (list (car items))))))

; Exercise 2.28
(define (fringe items)
  (cond ((null? items) '())
        ((pair? (car items))
         (append (fringe (car items)) (fringe (cdr items))))
        (else
         (append (list (car items)) (fringe (cdr items))))))

; Exercise 2.29 (What a mess! But it works :P)
(define (make-mobile left right)
  (list left right))
; length is scalar value, structure may be a scalar weight or another mobile
(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))
(define (left-weight mobile)
   (if (pair? (branch-structure (left-branch mobile)))
       (weight-mobile (branch-structure (left-branch mobile)))
       (branch-structure (left-branch mobile))))
(define (right-weight mobile)
   (if (pair? (branch-structure (right-branch mobile)))
       (weight-mobile (branch-structure (right-branch mobile)))
       (branch-structure (right-branch mobile))))
(define (weight-mobile mobile)
  (+ (left-weight mobile) (right-weight mobile)))
(define (balanced? mobile)
  (and (= (* (left-weight mobile) (branch-length (left-branch mobile)))
          (* (right-weight mobile) (branch-length (right-branch mobile))))
       (if (pair? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           #t)
       (if (pair? (branch-structure (right-branch mobile)))
           (balanced? (branch-structure (right-branch mobile)))
           #t)))
; d) if the structure because cons instead of list, only the selectors would change

; Exercise 2.30 (without the use of map)
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree))
         (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

; Exercise 2.31
(define (tree-map f tree)
  (cond ((null? tree) '())
        ((not (pair? tree))
         (f tree))
        (else (cons (tree-map f (car tree))
                    (tree-map f (cdr tree))))))

; Exercise 2.32 (This one made me feel smart!)
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
                          rest)))))
#| (We are reducing the list each time, but before we can do that, we must stick the car
 | of the list onto the reduced subsets list. You can see this visually: Moving rightwards,
 | each new number is applied to the full list before it.
 |#

; Exercise 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (my-length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

; Exercise 2.34 WIP
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) ())
              0
              coefficient-sequence))
