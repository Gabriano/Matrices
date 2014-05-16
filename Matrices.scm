(define (make-matrix m n)
  (let ((d (* m n)))
    (let ((mat (make-f64vector d)))
      (list m n mat))))


(define (make-random-matrix m n range)
  (let ((matrix (random-f64vector (* m n))))
    (matrix-scalar-mul! (list m n matrix) range)
    (list m n matrix)))

(define (make-identity-matrix m)
  #f)


(define matrix-m car)
(define matrix-n cadr)
(define matrix-vector caddr)


(define (square-matrix? matrix)
  (= (matrix-m matrix) (matrix-n matrix)))


(define (matrix? matrix)
  (and (list? matrix)
       (= (length matrix) 3)
       (number? (matrix-m matrix))
       (number? (matrix-n matrix))
       (f64vector? (matrix-vector matrix))))


(define (matrix-transform! matrix application)
  (let ((M (matrix-m matrix))
        (N (matrix-n matrix))
        (v (matrix-vector matrix)))
    (letrec ((loop (lambda (m n)
                     (cond ((= m M)
                            matrix)
                           ((= n N)
                            (loop (+ m 1) 0))
                           (else
                            (let ((index (+ (* m N) n)))
                              (f64vector-set! v index (application m n (f64vector-ref v index)))
                              (loop m (+ n 1))))))))
      (loop 0 0))))


(define (matrix-mul matrixA matrixB)
  (let ((matrix (make-matrix (matrix-n matrixA) (matrix-m matrixB))))
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
      (loop 0 0 0 0))))
  
(define (matrix-scalar-mul! matrix scalar)
  (matrix-transform! matrix (lambda (m n e) (* e scalar))))


(define (matrix-add! matrixA matrixB)
  (matrix-transform! matrixA (lambda (m n e) (+ e (f64vector-ref matrixB (* m n))))))


(define (matrix-set! matrix m n val)
  (f64vector-set! (matrix-vector matrix)
                  (+ (* m (matrix-n matrix)) n)
                  val))


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
;;;;
;;;;
(println (matrix->mathematica (make-matrix 10 10)))
;;(println (matrix->mathematica (make-random-matrix 3 3 100)))
(println (matrix->mathematica (make-random-matrix 10 10 100)))
