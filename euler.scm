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

(define (chip-list nth lat)
  (define (chip n lat)
    (cond ((null? lat) lat)
          ((= n nth) '())
          (else (cons (car lat)
                      (chip (+ n 1) (cdr lat))))))
  (chip 0 lat))

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

(define (prime-sum sum nth limit)
 (if (> nth limit)
	 sum
	 (let ((a (- (* nth 6) 1))
		   (b (+ (* nth 6) 1)))
	   (if (> nth limit)
		   sum
		   (cond ((and (prime? a) (prime? b))
				  (prime-sum (+ sum a b) (+ 1 nth) limit))
				 ((prime? a)
				  (prime-sum (+ sum a) (+ 1 nth) limit))
				 ((prime? b)
				  (prime-sum (+ sum b) (+ 1 nth) limit))
				 (else (prime-sum sum (+ 1 nth) limit)))))))


(define (euler10)
  (let ((start (current-inexact-milliseconds))
		(sum (+ 5 (prime-sum 0 1 (quotient (* 2 (expt 10 6)) 6))))
		(end (current-inexact-milliseconds)))
	(displayln "sum is")
	(displayln sum)
	(displayln "comsume time is")
	(displayln (/ (- end start) 1000))))

(define (prime-1? n)
  (define (helper nth limit)
	(if (> nth limit)
		#t
		(let ((a (- (* 6 nth) 1))
			  (b (+ (* 6 nth) 1)))
		  (cond ((or (divide? a n) (divide? b n)) #f)
				(else (helper (+ 1 nth) limit))))))

  (cond ((or (= 2 n) (= 3 n) (= 5 n)))
		((or (divide? 2 n) (divide? 3 n) (divide? 5 n)) #f)
		(helper 1 (quotient n 36))))

(define (time)
  (current-inexact-milliseconds))

(define (benchmark n)
  (let ((start1 (time))
		(r1 (prime? n))
		(end1 (time))
		(start2 (time))
		(r2 (prime-3? n))
		(end2 (time)))
	(displayln "prime-sum consume time is")
	(displayln (/ (- end1 start1) 1000))
	(displayln r1)
	(displayln "prime-sum-3? consume time is")
	(displayln (/ (- end2 start2) 1000))
	(displayln r2)))

