(define s2 (sqrt 1/2))
(define tot (expt 10 9))

(exact (floor (* s2 tot)))

(define m_in (exact (floor (* s2 tot))))
(define n_in (- tot m_in))


(let pro ([m n_in] [n m_in])
  (let ([left (+ (expt m 2) (- m) (* 2 m n))]
	[right (- (expt n 2) n)])
    (cond
     ((= left right) (values m n (/ (* n (- n 1))
				    (* (+ m n) (+ m n -1)))))
     ((< left right)
      (pro (+ m 1) n))
     (else
      (pro m (+ n 1))))))

;; too slow

(let ([2^0.5 (sqrt 2)])
  (let pro ([m 100000000])
    (let ([s-max (exact (ceiling (* 2^0.5 m)))])
      (let f ([now s-max])
	(let ([too (/ (* m (- m 1)) (* now (- now 1)))])
	  (cond
	   ((= too 1/2) (values m now))
	   ((< too 1/2) (f (- now 1)))
	   (else
	    (pro (+ m 1)))))))))

#|
> (time (let ((...)) ...))
    no collections
    0.000000000s elapsed cpu time
    0.000739700s elapsed real time
    58080 bytes allocated
493
697
> (time (let ((...)) ...))
    no collections
    0.000000000s elapsed cpu time
    0.003751400s elapsed real time
    275744 bytes allocated
2871
4060
> (time (let ((...)) ...))
    no collections
    0.000000000s elapsed cpu time
    0.004830100s elapsed real time
    997840 bytes allocated
16731
23661
> (time (let ((...)) ...))
    38 collections
    0.609375000s elapsed cpu time, including 0.000000000s collecting
    0.610071000s elapsed real time, including 0.000802700s collecting
    159329312 bytes allocated, including 160197280 bytes reclaimed
568345
803761
> (time (let ((...)) ...))
    193 collections
    2.750000000s elapsed cpu time, including 0.000000000s collecting
    2.744197800s elapsed real time, including 0.003110600s collecting
    813541624 bytes allocated, including 813940240 bytes reclaimed
3312555
4684660
> (time (let ((...)) ...))
    733 collections
    11.359375000s elapsed cpu time, including 0.000000000s collecting
    11.366361100s elapsed real time, including 0.014434600s collecting
    3089819688 bytes allocated, including 3091668720 bytes reclaimed
19306983
27304197
> (time (let ((...)) ...))
    972 collections
    16.343750000s elapsed cpu time, including 0.031250000s collecting
    16.372482500s elapsed real time, including 0.019090900s collecting
    4099892608 bytes allocated, including 4099150904 bytes reclaimed
112529341
159140520
|#

(define a (exact (floor (* (sqrt 2) (expt 10 12)))))

(let pro ([n a])
  (let-values ([(zhu fu) (exact-integer-sqrt (- (* 2 (expt n 2)) 1))])
    (cond
     ((zero? fu) (/ (+ n 1) 2))
     (else
      (pro (+ n 1))))))

;;failed, too slow