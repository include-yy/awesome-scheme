(define finite (expt 2 30))

(let pro ([i 1] [sum 0])
  (let ([fst i]
	[sec (* i 2)]
	[thr (* i 3)])
    (cond
     ((> i finite) sum)
     ((zero? (bitwise-xor fst sec thr))
      (pro (+ i 1) (+ sum 1)))
     (else
      (pro (+ i 1) sum)))))

#|
(time (let pro ...))
    6948 collections
    95.187500000s elapsed cpu time, including 0.250000000s collecting
    95.211948200s elapsed real time, including 0.140494800s collecting
    29351483168 bytes allocated, including 29350899944 bytes reclaimed
2178309
|#
