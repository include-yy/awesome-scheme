(define find-3/7
  (lambda (n)
    (let ([nearest (quotient (* n 3) 7)])
      (let f ([p nearest])
	(cond
	 ((or (= p 0) (= p 1)) (cons 1 n))
	 ((= (gcd p n) 1)
	  (cons p n))
	 (else
	  (f (- p 1))))))))

(define from-to
  (lambda (start end)
    (let f ([current start] [better (find-3/7 3)])
      (cond
       ((= current 7) (f (+ current 1) better))
       ((> current end) better)
       (else
	(let* ([now (find-3/7 current)]
	       [bet (/ (car better) (cdr better))]
	       [now-bet (/ (car now) (cdr now))])
	  (if (> (- 3/7 bet) (- 3/7 now-bet))
	      (f (+ current 1) now)
	      (f (+ current 1) better))))))))


(from-to 3 1000000)
#|
(time (from-to 3 ...))
    17 collections
    0.312500000s elapsed cpu time, including 0.000000000s collecting
    0.308445912s elapsed real time, including 0.000519036s collecting
    72005744 bytes allocated, including 71598000 bytes reclaimed
(428570 . 999997)
|#
