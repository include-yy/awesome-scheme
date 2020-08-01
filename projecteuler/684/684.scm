(define powermod
  (lambda (n k m)
    (let f ([n n] [k k] [r 1])
      (cond
       ((= k 0) r)
       ((= (remainder k 2) 1)
	(f n (- k 1) (remainder (* r n) m)))
       (else
	(f (remainder (* n n) m) (/ k 2) r))))))

(define fib
  (lambda (n)
    (cond
     ((< n 0) #f)
     ((= n 0) 0)
     ((= n 1) 1)
     ((= n 2) 1)
     (else
      (let f ([i 2]
              [a 1]
              [b 1])
        (cond
         ((= i n) b)
         (else
          (f (+ i 1) b (+ a b)))))))))

(define s
  (lambda (n)
    (let ([n/9 (quotient n 9)])
      (+ (* (- n (* 9 n/9)) (expt 10 n/9)) (- (expt 10 n/9) 1)))))

(define S
  (lambda (n)
    (let* ([n/9 (quotient n 9)]
           [re (- n (* n/9 9))])
      (+ (* 6 (expt 10 n/9))
         (- (* 9 n/9))
         -6
         (* (/ (* re (+ re 1)) 2) (expt 10 n/9))
         (* re (- (expt 10 n/9) 1))))))

(define S-imp
  (lambda (n)
    (let* ([m (quotient n 9)]
           [r (- n (* m 9))])
      (- (* (+ (* r r 1/2) (* r 3/2) 6) (expt 10 m)) (+ r (* m 9) 6)))))

(define S-mod
  (lambda (n mo)
    (let* ([m (quotient n 9)]
           [r (- n (* m 9))]
           [fst (remainder (+ (* r r 1/2) (* r 3/2) 6) mo)]
           [sec (powermod 10 m mo)]
           [thr (remainder (+ r (* m 9) 6) mo)])
      (- (* (remainder fst mo) sec) thr))))


(let pro ([i 2] [sum 0])
  (cond
   ((> i 90) (remainder sum 1000000007))
   (else
    (let ([fi (fib i)])
      (pro (+ i 1) (remainder (+ sum (S-mod fi 1000000007)) 1000000007))))))

#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.000772100s elapsed real time
    103904 bytes allocated
922058210
|#