;;;;;;;;;;;;;;;;;;;;;;;;
; 1.3
; Newton-method
;;;;;;;;;;;;;;;;;;;;;;;;

(define (square x)
  (* x x))

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
; 2.32
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.36
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.37
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define m '((1 2 3 4) (4 5 6 8) (6 7 8 9)))
(define v '(1 2 3 4))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.39
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reverse-foldr sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-foldl sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Nested Mappings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (enumerate-interval start end)
  (if (> start end)
      '()
      (cons start
            (enumerate-interval (+ start 1) end))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.40
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(define (unique-triple n)
  (flatmap (lambda (i)
             (map (lambda (j) (cons j i))
                  (enumerate-interval 1 (- (cadr i) 1))))
           (unique-pairs n)))

(define (sum-is? s)
  (lambda (triple)
    (= s (accumulate + 0 triple))))

(define (sum-is-s-triples s n)
  (filter (sum-is? s)
          (permutations (enumerate-interval 1 n))))

;---------------------------------------


(define (make-unique-tuple size max-number)
  (if (= size 0)
      (list '())
      (flatmap (lambda (i)
                 (map (lambda (t) (cons i t))
                      (make-unique-tuple (- size 1) (- i 1))))
               (enumerate-interval 1 max-number))))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.42 eight queens puzzle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-board '())

(define (make-pos col row)
  (list col row))

(define (select-kth positions)
  (car positions))

(define (select-rest positions)
  (cdr positions))

(define (get-col coordinate)
  (car coordinate))

(define (get-row coordinate)
  (cadr coordinate))

(define (safe? k positions)
  (let ((kth (select-kth positions))
        (rest (select-rest positions)))
    (let ((kth-row (get-row kth)))
      (accumulate (lambda (x y) (and x y))
                  #t
                  (map (lambda (p)
                         (let ((rest-col (get-col p))
                               (rest-row (get-row p)))
                           (not (or (= kth-row rest-row)
                                   (= (abs (- k rest-col))
                                      (abs (- kth-row rest-row)))))))
                       rest)))))


(define (adjoin-position new-row k rest-of-queens)
  (cons (make-pos k new-row) rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

