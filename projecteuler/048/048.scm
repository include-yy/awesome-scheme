(let pro ([i 1] [sum 0])
  (cond
   ((> i 1000) (remainder sum 10000000000))
   (else
    (pro (+ i 1) (+ sum (expt i i))))))

#|
(time (let pro ...))
    no collections
    0.046875000s elapsed cpu time
    0.047313972s elapsed real time
    2396280 bytes allocated
9110846700
|#
