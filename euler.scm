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
	(cond ((> (square test-divisor) n) (list n))
		  ((divide? test-divisor n) (list  test-divisor (/ n test-divisor)))
		  (else (find-divisor n (+ test-divisor 2)))))
  (if (even? n)
	  (= 2 n)
	  (= n (car (find-divisor n 3)))))

(define (demp n)
  (let ((lat (find-divisor n 2)))
	(cond ((null? (cdr lat)) lat)
		  (else (cons (car lat)
			 (demp (cadr lat)))))))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) (list n))
		((divide? test-divisor n) (list  test-divisor (/ n test-divisor)))
		(else (find-divisor n (+ test-divisor 1)))))

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

(define (euler10)
  (let ((start (time))
        (sum (sum-of-prime 2000000))
        (end (time)))
    (display "sum is ")
    (displayln sum)
    (display "comsume time is ")
    (display (/ (- end start) 1000))
    (displayln " seconds")))

(define time current-inexact-milliseconds)

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
  (cond ((null? lat) lat)
        ((> start end) '())
        (else (cons (col-select start (row-select start lat))
                    (diag-chip (+ start 1) end lat)))))

(define (frame-chip row col la)
  (row-chip row (col-chip col la)))


(define (col-select nth lat)
  (cond ((null? lat) lat)
        ((= nth 1) (car lat))
        (else (col-select (- nth 1) (cdr lat)))))

(define (row-select nth lat)
  (cond ((null? lat) lat)
        ; (x-cdr '((1) (2)))
        ; '(() ())
       ; ((null? (car lat)) lat)
        ((= nth 1) (col-car lat))
        (else (row-select (- nth 1) (col-cdr lat)))))

(define (product-of-y lat)
  (list-product (col-car lat)))

(define (product-of-x lat)
  (list-product (car lat)))

(define (product-of-diag lat)
  (list-product (diag-chip 1 4 lat)))

(define (square-chip lat)
  (frame-chip 4 4 lat))

(define (tup-value lat)
  (list (product-of-x lat) (product-of-y lat) (product-of-diag)))

(define (x-null? lat)
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
                       (x-select 1 lat)
                       (x-select 2 lat)
                       (x-select 3 lat)
                     ; (x-select 4 lat)
                       ))
(define (product-bottom lat)
  (apply max  (map *
                   (y-select 1 lat)
                   (y-select 2 lat)
                   (y-select 3 lat)
                                        ; (y-select 4 lat)
                   )))))

(define la '(
(08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08)
(49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00)
(81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65)
(52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91)
(22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80)
(24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50)
(32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70)
(67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21)
(24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72)
(21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95)
(78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92)
(16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57)
(86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58)
(19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40)
(04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66)
(88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69)
(04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36)
(20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16)
(20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54)
(01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48)))


(define la '(
(50 77 91 08)
(4 56 62 00)
(49 13 36 65)
(37 02 36 91)
(66 33 13 80)
(35 17 12 50)
(18 38 64 70)
(66 49 94 21)
(34 89 63 72)
(34 31 33 95)
(09 53 56 92)
(36 29 85 57)
(51 54 17 58)
(04 89 55 40)
(33 27 98 66)
(63 93 53 69)
(04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36)
(20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16)
(20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54)
(01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48)))


(define (right max-value lat)
  (if (right-end? lat) (max max-value (product-right-end lat))
      (let ((product-row (product-of-x (square-chip lat)))
            (product-col (product-of-y (square-chip lat)))
            (product-diag (product-of-diag (square-chip lat))))
        (right
         (max max-value product-row product-col product-diag)
         (x-cdr lat)))))

(define (down max-value lat)
  (cond ((bottom? lat) (right max-value lat))
        (else (down (right max-value lat) (cdr lat)))))

(define (test proc)
  (let ((start (time))
        (result proc)
        (end (time)))
    (begin
      (display "The result is ")
      (displayln result)
      (display "Consume time is ")
      (display (/ (- end start) 1000))
      (displayln " seconds"))))

(define la '(
(1 1 1 1 1 1 2 1 1)
(2 2 2 2 2 2 2 9 8)
(5 3 3 3 3 3 4 4 5)
(4 4 4 4 4 4 1 4 6)
))
