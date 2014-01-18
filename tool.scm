
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



(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponent? exp)
         (let ((b (base exp))
               (n (exponent exp)))
           (make-product n
                         (make-product (make-exponentiation b (- n 1))
                                       (deriv b var)))))
        (else
         (error "unkown expression type --DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product-revised m1 . m2)
  (define (iter m1 m2)
    (display m2)
    (newline)
    (if (null? (cdr m2))
        (simplify-product m1 (car m2))
        (list '* m1 (iter (car m2) (cdr m2)))))
  (iter m1 m2))

(define (simplify-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2) (* m1 m2)))
        (else (list '* m1 m2))))

(define (prefix-make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2) (+ a1 a2)))
        (else (list '+ a1 a2))))

(define (prefix-sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (prefix-addend s) (cadr s))

(define (prefix-augend s)
  (let ((rest (cddr s)))
    (if (null? (cdr rest))
        (car rest)
        (cons '* rest))))

(define (prefix-make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2) (* m1 m2)))
        (else (list '* m1 m2))))

(define (prefix-product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (prefix-multiplier p) (cadr p))

(define (prefix-multiplicand p)
  (let ((rest (cddr p)))
    (if (null? (cdr rest))
        (car rest)
        (cons '* rest))))

(define (prefix-exponent? x)
  (and (pair? x) (eq? (car x) 'expt)))

(define (prefix-base s)
  (cadr s))

(define (prefix-exponent s)
  (caddr s))

(define (prefix-make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list 'expt base exponent))))

(define (sum-2.58? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend-2.58 s)
  (car s))

(define (augend-2.58 s)
  (let ((rest (cddr s)))
    (if (null? (cdr rest))
        (car rest)
        (cons (car rest) (cons '+ (cdr rest))))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p)
  (car p))

(define (multiplicand-2.58 p)
  (let ((rest (cddr p)))
    (if (null? (cdr rest))
        (car rest)
        (cons (car rest) (cons '* (cdr rest))))))

(define (multiplicand p)
  (let ((rest (cddr p)))
    (if (null? (cdr rest))
        (car rest)
        rest)))



(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2) (* m1 m2)))
        (else (list m1 '* m2))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2) (+ a1 a2)))
        (else (list a1 '+ a2))))

(define (memb item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) '())
        (else (cons (car x) (memb item (cdr x))))))

(define (mema item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) (cdr x))
        (else (mema item (cdr x)))))

(define (addend s)
  (let ((a (memb '+ s)))
    (if (null? (cdr a))
        (car a)
        a)))

(define (augend s)
  (let ((rest (mema '+ s)))
    (if (null? (cdr rest))
        (car rest)
        rest)))

(define (sum? x)
  (if (memq '+ x)
      #t
      #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.3.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
