(define (fib n)
  (fib-iter 1 2 n))

(define (fib-iter a b n)
  (if (= n 1)
      a
      (fib-iter b (+ a b) (- n 1))))

(define (fib-max limit)
  (fib-v 1 limit))

(define (fib-v n max)
  (if (> (fib n) max)
      (list (- n 1) (fib (- n 1)))
      (fib-v (+ n 1) max)))

(define (p)  (fib-r 28 big-num))

(define (fib-r n numb)
  (if (= n 29)
      (begin
        (display "done!")
        (newline))
      (if (= 0 (remainder numb (fib n)))
          (fib n)
          (fib-r (- n 1) numb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projecteuler 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (divide? a b)
  (= (remainder b a) 0))

(define (square n)
  (* n n))

(define (prime? n)
  (define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) #t)
		  ((divide? test-divisor n) #f)
		  (else (find-divisor n (+ test-divisor 2)))))
  (if (even? n)
	  (= 2 n)
	  (find-divisor n 3)))

(define (demp n)
  (let ((lat (find-divisor n 3)))
        (cond 
         ((null? (cdr lat)) lat)
         (else 
          (cons (car lat)
                (demp (cadr lat)))))))

(define (find-divisor n test-divisor)
  (cond
   ((prime? n) (list n))
   ((even? n) (list 2 (/ n 2)))
   ((divide? test-divisor n) (list test-divisor (/ n test-divisor)))
   (else (find-divisor n (+ test-divisor 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projecteuler 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-list-range 
  (lambda (start end)
	(if (> start end) '()
		(cons start (make-list-range (+ start 1) end)))))

(define (natural-flow start end)
 (define (flow start end step)
  (cond ((> start  end) '())
		(else (cons start (flow (+ start step) end step)))))
 (flow start end 1))


(define (list->integer lat)
  (cond ((= (length lat) 0) 0)
		(else (+ 
			   (* (expt 10 (- (length lat) 1))
					(car lat))
				 (list->integer (cdr lat))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eula6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (a_1 + a_n) * a_ (n+1) * n
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (diff-sum-square lat)
  (* 2 (gather-sum 0 la)))

(define (gather-sum sum lat)
  (cond ((null? lat) sum)
		(else (gather-sum
			   (+ sum (* (car lat) (list-sum (cdr lat))))
			   (cdr lat)))))

(define (list-sum lat)
  (apply + lat))

(define (list-product lat)
  (apply * lat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; euler 7
;
; just test odd number. Because prime 
; can't be even number.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (euler7 order)
  (find-prime 1 1 order))

(define (find-prime n m order)
  (cond ((= 1 order) 2)
        ((= n order) m)
        ((prime? (+ m 2)) 
         (find-prime (+ n 1) (+ m 2) order))
        (else (find-prime n (+ m 2) order))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; euler 8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (str->integer str)
  (map char->int (string->list str)))

(define (char->int c)
  (- (char->integer c) 48))

(define (chip-dec nth lat)
  (cond ((null? lat) '())
        ((> nth (length lat)) '())
          (else (cons (chip-list nth lat)
                      (chip-dec nth (cdr lat))))))

(define (list-product lat)
  (apply * lat))

(define (euler8 nth lat)
  (define (helper max lat)
    (if (null? lat) max
        (let ((product (list-product (chip-list nth lat))))
          (if (> max product)
              (helper max (cdr lat))
              (helper product (cdr lat))))))
  (helper 0 lat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; euler 9
;
; a^2 + b^2 = c^2 a+b+c=1000
; b=1000*(500-a)/(1000-a) 
; find a number "a" between 1 and 499 such as "b" is an integer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (find-triplet a)
  (let ((b (/ (* 1000 (- 500 a))
              (- 1000 a))))
    (if (< a 500) 
        (if (and (integer? b) (> b a))
            (list a b (- 1000 a b))
            (find-triplet (+ a 5)))
        0)))

(define (square-sum lat)
  (apply + (map square lat)))

(define (euler9)
  (let ((trip (find-triplet 100)))
    (apply * trip)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; euler 10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum-of-prime n)
  (define (prime-sum sum nth limit)
	(if (> nth limit)
		sum
		(let ((a (- (* nth 6) 1))
			  (b (+ (* nth 6) 1)))
		  (prime-sum 
		   (+ sum (cond ((and (prime? a) (prime? b)) (+ a b))
                        ((prime? a) a)
                        ((prime? b) b)
                        (else 0)))
           (+ nth 1)
           limit))))

  (let ((limit (quotient n 6)))
    (+ 5 (prime-sum 0 1 limit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; euler 11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (chip-list nth lat)
  (cond  ((null? lat) lat)
         ((> nth (length lat)) 
         (displayln "ERROR: Number of rows out of range"))
         ((= nth 0) '())
        (else (cons (car lat)
                    (chip-list (- nth 1) (cdr lat))))))

(define (row-chip nth lat)
  (chip-list nth lat))

(define (col-car lat)
 (cond ((null? lat) lat)
       (else (cons (caar lat)
                   (col-car (cdr lat))))))

(define (col-cdr lat)
  (cond ((null? lat) lat)
        (else (cons (cdar lat)
                    (col-cdr (cdr lat))))))

(define (col-chip-list nth lat)
  (cond ((null? lat) lat)
        ((> nth (length (car lat)))
         (displayln "ERROR: number of columns out of range"))
        ((= nth 0) '())
        (else (cons (col-car lat)
                    (col-chip-list (- nth 1) (col-cdr lat))))))

(define (col-chip nth lat)
  (cond ((null? lat) lat)
        (else (cons (chip-list nth (car lat))
                    (col-chip nth (cdr lat))))))

(define (diag-chip start end lat)
  (cond ((null? lat) 
         (displayln "ERROR: '()"))
        ((> start end) '())
        (else (cons (row-select start (row-select start lat))
                    (diag-chip (+ start 1) end lat)))))

(define (reverse-diag-chip start end lat)
  (let ((la (map reverse lat)))
    (diag-chip start end la)))

(define (frame-chip row col la)
  (row-chip row (col-chip col la)))


(define (row-select nth lat)
  (cond ((null? lat) lat)
        ((> nth (length lat))
         (displayln "ERROR: number of rows out of range"))
        ((= nth 1) (car lat))
        (else (row-select (- nth 1) (cdr lat)))))

(define (col-select nth lat)
  (cond ((null? lat) lat)
        ((> nth (length (car lat)))
         (displayln "ERROR: number of columns out of range"))
        ; (x-cdr '((1) (2)))
        ; '(() ())
       ; ((null? (car lat)) lat)
        ((= nth 1) (col-car lat))
        (else (col-select (- nth 1) (col-cdr lat)))))

(define (product-of-col lat)
  (list-product (col-car lat)))

(define (product-of-row lat)
  (list-product (car lat)))

(define (product-of-diag lat)
  (list-product (diag-chip 1 4 lat)))

(define (product-of-reverse-diag lat)
  (list-product (reverse-diag-chip 1 4 lat)))

(define (square-chip lat)
  (frame-chip 4 4 lat))

(define (col-null? lat)
  (null? (car lat)))

(define (bang? n lat)
  (or (< (length lat) n)
      (< (length (car lat)) n)))

(define (right-end? lat)
  (< (length (car lat)) 4))

(define (bottom? lat)
  (= (length lat) 4))

(define (product-right-end lat)
  (max (apply max (map *
                       (row-select 1 lat)
                       (row-select 2 lat)
                       (row-select 3 lat)
                       (row-select 4 lat)
                       ))))

(define (product-bottom lat)
  (max (apply max  (map *
                        (col-select 1 lat)
                        (col-select 2 lat)
                        (col-select 3 lat)
                        (col-select 4 lat)
                        ))))



(define (right max-value lat)
  (if (right-end? lat) 
      (begin
       ; (displayln (list max-value (product-right-end lat)))
        (max max-value (product-right-end lat)))
      (let ((product-row (product-of-row (square-chip lat)))
            (product-col (product-of-col (square-chip lat)))
            (product-diag (product-of-diag (square-chip lat)))
            (product-reverse-diag (product-of-reverse-diag (square-chip lat))))
        (begin
        ;  (displayln (list max-value product-row product-col product-diag product-reverse-diag))
          (right
           (max max-value product-row product-col product-diag product-reverse-diag)
           (col-cdr lat))))))

(define (down max-value lat)
  (cond ((bottom? lat) (max (right max-value lat) (product-bottom lat)))
        (else (down (right max-value lat) (cdr lat)))))

(define (test proc)
  (let ((start (current-inexact-milliseconds))
        (result proc)
        (end (current-inexact-milliseconds)))
    (begin
      (display "The result is ")
      (displayln result)
      (display "Consume time is ")
      (display (/ (- end start) 1000))
      (displayln " seconds"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; euler 12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (divisor-pairs n)
  (define (find-divisor n test-divisor)
    (cond
     ((> (square test-divisor) n) '())
     ((divide? test-divisor n)
      (cons (list  test-divisor (/ n test-divisor))
            (find-divisor n (+ test-divisor 1))))
     (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 1))

(define (divisor-list n)
  (define (find-divisor n test-divisor)
    (cond
     ((> (square test-divisor) n) '())
     ((divide? test-divisor n)
      (append (list  test-divisor (/ n test-divisor))
            (find-divisor n (+ test-divisor 1))))
     (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 1))

(define (dup-nums n)
  (- (expt 2 (length (demp n)))
     (* 2 (length (divisor-list n))))) 


(define (num-of-value n lat)
  (define (num-iter m lat)
    (cond 
     ((null? lat) m)
     ((= (car lat) n) 
      (num-iter (+ 1 m) (cdr lat)))
     (else (num-iter m (cdr lat)))))
  (num-iter 0 lat))

(define (num-search lat)
  (define (num-iter m lat)
    (cond
     ((null? lat) m)
     ((= (car lat) 2) 
      (num-iter (+ 1 m) (cdr lat)))
     (else m)))
  (num-iter 0 lat))

(define multirember
;  "remove all occurrences of a"
  (lambda (a lat)
    (cond 
      ((null? lat) '())
      ((eq? a (car lat))
        (multirember a (cdr lat)))
      (else (cons (car lat)
              (multirember a (cdr lat)))))))

(define latunique
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat)
              (latunique
                (multirember (car lat)
                  (cdr lat))))))))

(define (unique lat)
  (map latunique lat))

(define (arrange lat)
  (latunique (sort lat <)))

(define (euler n m)
  (let ((v (* n (+ n 1)))
        (l1 (length (arrange (divisor-list n))))
        (l2 (length (arrange (divisor-list (+ n 1))))))
    (let ((i (num-search (demp v))))
      (cond 
       ((= i 0) (list n v l1 l2 i))
       ((> n 30000) (list n v l1 l2 i))
       ((= (* l1 l2) (* m (/ (+ i 1) i)))
        (/ v 2))
       (else (euler (+ n 1) m))))))

