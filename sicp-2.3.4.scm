;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.3.4 Huffman encode tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.67
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))


(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.68
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (encode-symbol symbo tree)
  (let ((left (left-branch tree))
        (right (right-branch tree)))
    (cond ((element-of-set? symbo (symbols left))
           (left-symbols symbo left))
          ((element-of-set? symbo (symbols right))
           (right-symbols symbo right))
          (else (error 'encode-symbol "no symbol." symbo)))))

(define (left-symbols symbo tree)
  (if (leaf? tree)
      (list 0)
      (cons 0 (encode-symbol symbo tree))))

(define (right-symbols symbo tree)
  (if (leaf? tree)
      (list 1)
      (cons 1 (encode-symbol symbo tree))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol-revised (car message) tree)
              (encode (cdr message) tree))))

(define message '(A D A B B C A))

(define (encode-symbol-revised symbo tree)
  (if (leaf? tree)
      '()
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (cond ((element-of-set? symbo (symbols left))
               (cons 0 (encode-symbol-revised symbo left)))
              ((element-of-set? symbo (symbols right))
               (cons 1 (encode-symbol-revised symbo right)))
              (else (error 'encode-symbol-revised "no symbol." symbo))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.69
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sym-pairs '((A 4) (B 2) (C 1) (D 1)))

(define x (make-code-tree (make-leaf 'E 1)
                          (make-leaf 'F 1)))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (null? (cdr set))
      (car set)
      (let ((left (car set))
            (right (cadr set)))
        (successive-merge 
         (adjoin-set (make-code-tree left right)
                     (cddr set))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.70
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lyric-pairs 
  '((WAH 1) (BOOM 1) (A 2) (GET 2) (JOB 2) (SHA 3) (YIP 9) (NA 16)))

(define lyric
  '(GET A JOB
        SHA NA NA NA NA NA NA NA NA
        GET A JOB
        SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
        SHA BOOM))

(define lyric-tree (generate-huffman-tree lyric-pairs))

(define lyric-encoding (encode lyric lyric-tree))

(define lyric-decoding (decode lyric-encoding lyric-tree))
