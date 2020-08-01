(define tri
  (lambda (n)
    (* n (+ n 1) 1/2)))
(define pen
  (lambda (n)
    (* n (- (* n 3) 1) 1/2)))
(define hex
  (lambda (n)
    (* n (- (* n 2) 1))))

(define sqrt3 (sqrt 3))

(let pro ([i 286])
  (let ([now (tri i)]
	[pn (exact (floor (/ i sqrt3)))]
	[hn (quotient i 2)])
    (let f ([p pn] [now2 (pen pn)])
      (cond
       ((> now2 now) (pro (+ i 1)))
       ((= now2 now)
	(let g ([h hn] [now3 (hex hn)])
	  (cond
	   ((> now3 now2) (pro (+ i 1)))
	   ((= now3 now2) (cons i now))
	   ((< now3 now2) (g (+ h 1) (hex (+ h 1)))))))
       ((< now2 now)
	(f (+ p 1) (pen (+ p 1))))))))

#|
(time (let pro ...))
    2 collections
    0.046875000s elapsed cpu time, including 0.000000000s collecting
    0.038153808s elapsed real time, including 0.000133644s collecting
    8765528 bytes allocated, including 8452440 bytes reclaimed
(55385 . 1533776805)
|#

    
