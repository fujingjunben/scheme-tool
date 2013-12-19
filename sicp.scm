;;;;;;;;;;;;;;;;;;;;;;;;
; 1.3
; Newton-method
;;;;;;;;;;;;;;;;;;;;;;;;

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
(try first-guess))

(define (cube x) (* x x x))

((deriv cube) 5)

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(newtons-method (cubic 2 1 1) 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.41
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (double f)
  (lambda (x)
    (f (f x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.42
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compose f g)
  (lambda (x)
    (f (g x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.43 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (repeated f n)
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeated f (dec n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.44
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.18 reverse
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (my-reverse lat)
  (define (iter lat r)
    (cond
     ((null? lat) r)
     (else
      (iter
       (cdr lat)
       (cons (car lat) r)))))
  (iter lat '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.27 deep-reverse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deep-reverse l)
  (define (iter l r)
    (cond
     ((null? l) r)
     (else
      (iter
       (cdr l)
       (cons (reverse (car l)) r)))))
  (iter l '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.28 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (atom? a)
  (cond
   ((null? a) #f)
   ((pair? a) #f)
   (else #t)))

(define (fringe l)
  (cond
   ((null? l) '())
   ((not (pair? (car l)))
    (cons (car l)
            (fringe (cdr l))))
   (else
    (append (fringe (car l))
            (fringe (cdr l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.29
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cadr m))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (mobile? m)
  (cond
   ((not (pair? m))
    #f)
   ((and (structure? (car m))
         (structure? (cadr m)))
    #t)
   (else  #f)))

(define (structure? s)
  (if (pair? s)
      (if (and (= (length s) 2) (atom? (car s)))
          #t
          #f)
      #f))

(define (structure-is-mobile? s)
  (if (mobile? (branch-structure s))
      #t
      #f))

(define (total-weight m)
  (cond
   ((mobile? m)
    (+ (total-weight (left-branch m))
       (total-weight (right-branch m))))
   ((structure-is-mobile? m)
    (total-weight (right-branch m)))
   (else
    (branch-structure m))))

(define (balance? m)
  (let ((l (left-branch m))
        (r (right-branch m)))
      (and
       (= (* (branch-length l)
               (total-weight l))
            (* (branch-length r)
               (total-weight r)))
       (cond 
        ((and (not (mobile? l)) (mobile? r))
         (balance? r))
        ((and (mobile? l) (not (mobile? r)))
         (balance? l))
        ((and (mobile? l) (mobile? r)) 
         (and (balance? l)
              (balance? r)))
        (else #t)))))

;test
 (define level-1-mobile (make-mobile (make-branch 2 1) 
                                     (make-branch 1 2))) 
 (define level-2-mobile (make-mobile (make-branch 3 level-1-mobile) 
                                     (make-branch 9 1))) 
 (define level-3-mobile (make-mobile (make-branch 4 level-2-mobile) 
                                     (make-branch 8 2))) 
  
 (total-weight level-1-mobile)
 (total-weight level-2-mobile) 
 (total-weight level-3-mobile) 

 (balance? (make-mobile (make-branch 2 3) 
                         (make-branch 3 2))) 

 (balance? level-1-mobile) 
 (balance? level-2-mobile) 
 (balance? level-3-mobile) 
  
 (balance? (make-mobile (make-branch 10 1000) 
                         (make-branch 1 level-3-mobile))) 
  
(define n 1)

(if (> n 0)
    (begin
      (newline)
      (display "nihao")
      (newline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.30
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square-tree tree)
  (cond
   ((null? tree) '())
   ((not (pair? tree)) (square tree))
   (else
    (cons (square-tree (car tree))
          (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree subtree)
             (square subtree)))
       tree))

(define (tree-map proc tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map proc subtree)
             (proc subtree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.23
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map-me p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append-me seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-me sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.34
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2. 35
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-leaves t)
  (accumulate 
   +
   0
   (map (lambda (x)
          (if (pair? x)
              (count-leaves x)
              1))
        t)))
