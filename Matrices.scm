(define (make-matrix m n)
  (let ((d (* m n)))
    (let ((mat (make-f64vector d)))
      (list m n mat))))


(define (make-random-matrix m n range)
  (let ((matrix (random-f64vector (* m n))))
    (matrix-scalar-mul! (list m n matrix) range)
    (list m n matrix)))


(define (matrix-copy matrix)
  (let ((M (matrix-m matrix))
        (N (matrix-n matrix)))
    (let ((mat (make-matrix M N)))
      (letrec ((loop (lambda (m n)
                       (cond ((= m M)
                              mat)
                             ((= n N)
                              (loop (+ m 1) 0))
                             (else
                              (matrix-set! mat
                                           m
                                           n
                                           (matrix-ref matrix m n))
                              (loop m (+ n 1)))))))
        (loop 0 0)))))


(define (make-matrix-validator m n predicate)
  (lambda (matrix)
    (and (matrix? matrix)
         (= (matrix-m matrix) m)
         (= (matrix-n matrix) n)
         (predicate matrix))))


(define (make-identity-matrix m)
  (let ((matrix (make-matrix m m)))
    (letrec ((loop (lambda (n)
                     (cond ((= n m)
                            matrix)
                           (else
                            (matrix-set! matrix n n 1.)
                            (loop (+ n 1)))))))
      (loop 0))))


(define matrix-m car)
(define matrix-n cadr)
(define matrix-vector caddr)


(define (mn->index matrix m n)
  (let ((N (matrix-n matrix)))
    (+ (* m N) n)))


(define (square-matrix? matrix)
  (and (matrix? matrix)
       (= (matrix-m matrix) (matrix-n matrix))))


(define (matrix? matrix)
  (and (list? matrix)
       (= (length matrix) 3)
       (number? (matrix-m matrix))
       (number? (matrix-n matrix))
       (f64vector? (matrix-vector matrix))))


(define (identity-matrix? matrix m)
  (let ((predicate (lambda (matrix)
                     (letrec ((loop (lambda (n)
                                      (if (= n m)
                                          #t
                                          (and
                                           (= (matrix-ref matrix n n) 1.)
                                           (loop (+ n 1)))))))
                       (loop 0)))))
    (let ((validator (make-matrix-validator m m predicate)))
      (validator matrix))))


(define (matrix-transform! matrix application)
  (let ((M (matrix-m matrix))
        (N (matrix-n matrix)))
    (letrec ((loop (lambda (m n)
                     (cond ((= m M)
                            matrix)
                           ((= n N)
                            (loop (+ m 1) 0))
                           (else
                            (matrix-set! matrix
                                         m
                                         n
                                         (application m n (matrix-ref matrix m n)))
                            (loop m (+ n 1)))))))
      (loop 0 0))))


(define (matrix-mul matrixA matrixB)
  (let ((matrix (make-matrix (matrix-m matrixA) (matrix-n matrixB))))
    (letrec ((K (matrix-n matrixA))
             (M (matrix-m matrix))
             (N (matrix-n matrix))
             (loop (lambda (m n k acc)
                     (cond ((= m M)
                            matrix)
                           ((= n N)
                            (loop (+ m 1) 0 0 acc))
                           ((= k K)
                            (matrix-set! matrix m n acc)
                            (loop m (+ n 1) 0 0))
                           (else
                            (loop m n (+ k 1) (+ acc (* (matrix-ref matrixA m k)
                                                        (matrix-ref matrixB k n)))))))))
      (time (loop 0 0 0 0)))))


(define (matrix-add matrixA matrixB)
  (let ((matrix (make-matrix (matrix-m matrixA) (matrix-n matrixA))))
    (letrec ((M (matrix-m matrix))
             (N (matrix-n matrix))
             (loop (lambda (m n)
                     (cond ((= m M)
                            matrix)
                           ((= n N)
                            (loop (+ m 1) 0))
                           (else
                            (matrix-set! matrix m n (+ (matrix-ref matrixA m n)
                                                       (matrix-ref matrixB m n)))
                            (loop m (+ n 1)))))))
      (loop 0 0))))
  
(define (matrix-scalar-mul! matrix scalar)
  (matrix-transform! matrix (lambda (m n e) (* e scalar))))


(define (matrix-add! matrixA matrixB)
  (matrix-transform! matrixA
                     (lambda (m n e)
                       (+ e (matrix-ref matrixB m n)))))


(define (matrix-set! matrix m n val)
  (f64vector-set! (matrix-vector matrix)
                  (mn->index matrix m n)
                  val))


(define (matrix-ref matrix m n)
  (f64vector-ref (matrix-vector matrix)
                 (mn->index matrix m n)))


;;;
;;;; Row Echelon Form
;;;


(define (matrix-swap-lines! matrix i j)
  (letrec ((N (matrix-n matrix))
           (loop (lambda (n)
                   (cond ((= n N)
                          matrix)
                         (else
                          (let ((tmp (matrix-ref matrix i n)))
                            (matrix-set! matrix i n (matrix-ref matrix j n))
                            (matrix-set! matrix j n tmp)
                            (loop (+ n 1))))))))
    (loop 0)))


(define (matrix-swap-columns! matrix i j)
  (letrec ((M (matrix-m matrix))
           (loop (lambda (m)
                   (cond ((= m M)
                          matrix)
                         (else
                          (let ((tmp (matrix-ref matrix m i)))
                            (matrix-set! matrix m i (matrix-ref matrix m j))
                            (matrix-set! matrix m j tmp)
                            (loop (+ m 1))))))))
    (loop 0)))


;; L_row <- scalar * L_row
(define (matrix-row-mul! matrix row scalar)
  (letrec ((N (matrix-n matrix))
           (loop (lambda (n)
                   (cond ((= n N)
                          matrix)
                         (else
                          (matrix-set! matrix row n (* (matrix-ref matrix row n) scalar))
                          (loop (+ n 1)))))))
    (loop 0)))


;; L_row <- L_row + scalar * L_row1
(define (matrix-row-add-mul! matrix row row1 scalar)
  (letrec ((N (matrix-n matrix))
           (loop (lambda (n)
                   (cond ((= n N)
                          matrix)
                         (else
                          (matrix-set! matrix row n (+ (matrix-ref matrix row n)
                                                       (* scalar (matrix-ref matrix row1 n))))
                          (loop (+ n 1)))))))
    (loop 0)))


(define (matrix-gauss-reduction matrix)
  (let ((M (matrix-m matrix))
        (N (matrix-n matrix)))
    )


;;;
;;;; Decomposition
;;;

(define (lu-decomposition! matrix pivoting-strategy)
  (define (lu-decomposition-partial-pivoting!)
    (define (ludpp-aux! pivot-line pivot-column pivot-max?)
      #f)
    
    (ludpp-aux 0 0))

  (define (lu-decomposition-full-pivoting!)
    #f)
  
  (if (eq? pivoting-strategy 'partial)
      (lu-decomposition-partial-pivoting!)
      (lu-decomposition-full-pivoting!)))


;;;
;;;; Misc
;;;


(define (matrix->mathematica matrix)
  (let ((M (matrix-m matrix))
        (N (matrix-n matrix))
        (data (matrix-vector matrix)))
    (letrec ((loop (lambda (m n linepos out)
                     (cond ((= m M)
                            (reverse (cons #\} out)))
                           ((eq? linepos 'startmatrix)
                            (loop m
                                  n
                                  'startline
                                  (cons #\{ out)))
                           ((eq? linepos 'startline)
                            (loop m
                                  n
                                  'val
                                  (cons #\{ out)))
                           ((= n N)
                            (loop (+ m 1)
                                  0
                                  'eolcomma
                                  (append '(#\}) out)))
                           ((eq? linepos 'comma)
                            (loop m
                                  n
                                  'val
                                  (append '(#\space #\,) out)))
                           ((eq? linepos 'eolcomma)
                            (loop m
                                  0
                                  'startline
                                  (append '(#\newline #\,) out)))
                           (else
                            (loop m
                                  (+ n 1)
                                  'comma
                                  (cons (f64vector-ref data (+ (* m N) n)) out)))))))
      (loop 0 0 'startmatrix '()))))


;;;;
;;;; Tests
;;;;
;; (println (matrix->mathematica (make-matrix 10 10)))
;; (println (matrix->mathematica (make-random-matrix 3 3 100)))
;; (println (matrix->mathematica (make-random-matrix 10 10 100)))
;; (println (matrix->mathematica (make-identity-matrix 10)))
;; (pp (identity-matrix? (make-identity-matrix 10) 10))
;; (let ((m1 (make-identity-matrix 4))
;;       (m2 (make-identity-matrix 4)))
;;   (pp '===)
;;   (println (matrix->mathematica m1))
;;   (pp '===)
;;   (println (matrix->mathematica m2))
;;   (pp '===)
;;   (matrix-scalar-mul! m1 12)
;;   (matrix-scalar-mul! m2 16)
;;   (pp '===)
;;   (println (matrix->mathematica m1))
;;   (pp '===)
;;   (println (matrix->mathematica m2))
;;   (pp '===)
;;   (println (matrix->mathematica (matrix-mul m1 m2)))
;;   (pp '===)
;;   (println (matrix->mathematica m1))
;;   (pp '===)
;;   (println (matrix->mathematica m2)))
;; (let ((m1 (make-random-matrix 3 3 8))
;;       (m2 (make-random-matrix 3 3 8)))
;;   (pp '===)
;;   (println (matrix->mathematica m1))
;;   (pp '===)
;;   (println (matrix->mathematica m2))
;;   (pp '===)  
;;   (println (matrix->mathematica (matrix-add m1 m2)))
;;   (pp '===)
;;   (println (matrix->mathematica m1))
;;   (pp '===)
;;   (println (matrix->mathematica m2))
;;   (pp '===))
;; (let ((m1 (make-random-matrix 1000 1000 1000))
;;       (m2 (make-random-matrix 1000 1000 1000)))
;;   (matrix-mul m1 m2))

(let ((m1 (make-identity-matrix 10)))
  (println (matrix->mathematica m1))
  (println (matrix->mathematica (matrix-swap-lines! m1 3 7)))
  (println (matrix->mathematica (matrix-swap-columns! m1 3 7))))

