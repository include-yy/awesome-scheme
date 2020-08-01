(define four-million (* 4 1000000))
(define even? (lambda (n) (= 0 (remainder n 2))))
(time (let Fib-even-sum ([i 1] [j 1] [sum 0])
  (cond
   ((> j four-million) sum)
   ((even? j)
    (Fib-even-sum j (+ i j) (+ sum j)))
   (else
    (Fib-even-sum j (+ i j) sum)))))
#|
(time (let Fib-even-sum ...))
    no collections
    0.000000000s elapsed cpu time
    0.000000000s elapsed real time
    0 bytes allocated
4613732
|#
