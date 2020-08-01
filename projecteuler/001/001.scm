(let f ([i 3])
  (cond
   ((>= i 1000) 0)
   ((or (= (remainder i 3) 0)
	(= (remainder i 5) 0))
    (+ i (f (+ i 1))))
   (else
    (f (+ i 1)))))
#|
(time (let f ...))
    no collections
    0.000000000s elapsed cpu time
    0.000015540s elapsed real time
    0 bytes allocated
233168
|#
