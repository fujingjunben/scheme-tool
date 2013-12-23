
(define (divide? b a)
  (= (remainder b a) 0))

(define (square n)
  (* n n))

(define (prime? n)
  (define (traverse lat) 
    (cond
     ((null? lat)
      #t)
     ((divide? n (car lat))
      #f)
     (else
      (traverse (cdr lat)))))
  (let-values (((s r) (exact-integer-sqrt n)))
    (let ((prime-list (soe s)))
      (traverse prime-list))))

(define (demp n)
  (define (find-divisor n lat)
    (cond
     ((null? lat)
      (cond
       ((= n 1) 
        '())
       (else
        (list n))))
     ((divide? n (car lat))
      (cons
       (car lat)
       (find-divisor (/ n (car lat)) lat)))
     (else
      (find-divisor n (cdr lat)))))
  
  (let ((prime-list (soe (integer-sqrt n))))
    (find-divisor n prime-list)))

(define (soe n)
  (define (compute-soe i j prime-vector)
    (cond 
     ((> (square i) n)
      prime-vector)
     (else
      (let ((idx (* i (+ i j))))
        (cond
         ((and (vector-ref prime-vector i) (<= idx n))
          (begin
            (vector-set! prime-vector idx #f)
            (compute-soe i (+ j 1) prime-vector)))
         (else (compute-soe (+ i 1) 0 prime-vector)))))))

  (define (print-soe i prime-list)
    (cond
     ((> i n) '())
     ((car prime-list)
      (cons i (print-soe (+ i 1) (cdr prime-list))))
     (else (print-soe (+ i 1) (cdr prime-list)))))
  
  (let ((prime-vector (make-vector (+ n 1) #t)))
    (compute-soe 2 0 prime-vector)
    (print-soe 2 (cddr (vector->list prime-vector)))))


(define (number->list number)
  (map
   (lambda (x) (string->number (string x)))
   (string->list (number->string number))))

(define (str_int->list str)
  (map 
   (lambda (x) (string->number (string x)))
   (string->list str)))

(define (char->int c)
  (- (char->integer c) 48))

(define (int->list int)
  (str_int->list (str)))


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

(define (make-unique-tuple size start end)
  (if (= size 0)
      (list '())
      (flatmap (lambda (i)
                 (map (lambda (t) (cons i t))
                      (make-unique-tuple (- size 1) start (- i 1))))
               (enumerate-interval start end))))

(define (make-tuple size start end)
  (if (= size 0)
      (list '())
      (flatmap (lambda (i)
                 (map (lambda (t) (cons i t))
                      (make-tuple (- size 1) start end)))
               (enumerate-interval start end))))

