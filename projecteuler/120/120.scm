(let pro ([a 3] [sum-r 0])
  (cond
   ((> a 1000) sum-r)
   (else
    (if (zero? (remainder a 2))
        (pro (+ a 1) (+ sum-r (* a (- a 2))))
        (pro (+ a 1) (+ sum-r (* a (- a 1))))))))

#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.000007500s elapsed real time
    0 bytes allocated
333082500
|#