(define-syntax c-for
  (lambda (x)
    (syntax-case x ()
      [(k e ...)
       (with-syntax ([break (datum->syntax #'k 'break)])
                    #'(call/cc
                       (lambda (break)
                         e ...)))])))

(define make-matrix
  (lambda (n m val)
    (let ([vec (make-vector n #f)])
      (do ([i 0 (+ i 1)])
          ((= i n) vec)
        (vector-set! vec i (make-vector m val))))))

(define-syntax v-r
  (syntax-rules ()
    [(_ vec i)
     (vector-ref vec i)]))
(define-syntax v-s!
  (syntax-rules ()
    [(_ vec i newval)
     (vector-set! vec i newval)]))
(define-syntax v-s+!
  (syntax-rules ()
    [(_ vec i val)
     (vector-set! vec i (+ val (vector-ref vec i)))]))

(define-syntax m-r
  (syntax-rules ()
    [(_ mat n m)
     (vector-ref (vector-ref mat n) m)]))
(define-syntax m-s!
  (syntax-rules ()
    [(_ mat n m newval)
     (vector-set! (vector-ref mat n) m newval)]))

(define-syntax m-s+!
  (syntax-rules ()
    [(_ mat n m val)
     (m-s! mat n m (+ val (m-r mat n m)))]))
(define-syntax m-s*!
  (syntax-rules ()
    [(_ mat n m val)
     (m-s! mat n m (* val (m-r mat n m)))]))

(define gaussj
  (lambda (ma n mb m)
    (let ([indxc (make-vector n 1)]
          [indxr (make-vector n 1)]
          [ipiv  (make-vector n 0)])
      (let ([icol 0]
            [irol 0]
            [big 0]
            [dum 0]
            [pivinv 0])
        (do ([i 0 (+ i 1)])
            ((= i n))
          (set! big 0)
          (do ([j 0 (+ j 1)])
              ((= j n))
            (cond
             ((= (v-r ipiv j) 1))
             (else
              (do ([k 0 (+ k 1)])
                  ((= k n))
                (cond
                 ((zero? (v-r ipiv k))
                  (if (>= (abs (m-r ma j k)) big)
                      (begin
                        (set! big (abs (m-r ma j k)))
                        (set! irow j)
                        (set! icol k))))
                 ((> (v-r ipiv k) 1)
                  (assertion-violation 'gaussj "singular Matrix-1" 0)))))))
          (v-s+! ipiv icol 1)
          (if (not (= irow icol))
              (begin
                (let ([temp (v-r ma irow)])
                  (v-s! ma irow (v-r ma icol))
                  (v-s! ma icol temp))
                (let ([temp (v-r mb irow)])
                  (v-s! mb irow (v-r mb icol))
                  (v-s! mb icol temp))))
          (v-s! indxr i irow)
          (v-s! indxc i icol)
          (if (zero? (m-r ma icol icol))
              (assertion-violation 'gaussj "singluar Matrix-2" 0))
          (set! pivinv (/ 1 (m-r ma icol icol)))
          (m-s! ma icol icol 1)
          (do ([i 0 (+ i 1)])
              ((= i n))
            (m-s*! ma icol i pivinv))
          (do ([i 0 (+ i 1)])
              ((= i m))
            (m-s*! mb icol i pivinv))
          (do ([i 0 (+ i 1)])
              ((= i n))
            (if (not (= i icol))
                (begin
                  (set! dum (m-r ma i icol))
                  (m-s! ma i icol 0)
                  (do ([j 0 (+ j 1)])
                      ((= j n))
                    (m-s+! ma i j (- (* (m-r ma icol j) dum))))
                  (do ([j 0 (+ j 1)])
                      ((= j m))
                    (m-s+! mb i j (- (* (m-r mb icol j) dum))))))))
        (do ([i (- n 1) (- i 1)])
            ((< i 0))
          (if (not (= (v-r indxr i) (v-r indxc i)))
              (do ([k 0 (+ k 1)])
                  ((= k n))
                (let ([swap (m-r ma k (v-r indxr i))])
                  (m-s! ma k (v-r indxr i) (m-r ma k (v-r indxc i)))
                  (m-s! ma k (v-r indxc i) swap)))))))))

(define n10
  (lambda (n)
    (+ 1 (- n) (expt n 2) (- (expt n 3)) (expt n 4)
       (- (expt n 5)) (expt n 6) (- (expt n 7))
       (expt n 8) (- (expt n 9)) (expt n 10))))

(define make-b
  (lambda (n)
    (let ([mb (make-matrix n 1 0)])
      (do ([i 1 (+ i 1)])
          ((> i n) mb)
        (m-s! mb (- i 1) 0 (n10 i))))))

(define make-A
  (lambda (n)
    (let ([ma (make-matrix n n 0)])
      (do ([i 0 (+ i 1)])
          ((= i n) ma)
        (do ([j 0 (+ j 1)])
            ((= j n))
          (m-s! ma i j (expt (+ i 1) (- n j 1))))))))

(define poly-gen
  (lambda (mat num)
    (lambda (n)
      (do ([i 0 (+ i 1)] [sum 0 (+ sum (* (m-r mat i 0) (expt n (- num i 1))))])
          ((= i num) sum)))))

(let pro ([i 1] [sum 0])
  (cond
   ((= i 11) sum)
   (else
    (let ([ma (make-A i)]
          [mb (make-b i)])
      (gaussj ma i mb 1)
      (pro (+ i 1) (+ sum ((poly-gen mb i) (+ i 1))))))))

#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.005188800s elapsed real time
    421528 bytes allocated
37076114526
|#