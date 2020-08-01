(define N 12000)

(begin
  (define a0 1)
  (define b0 3)
  (define cn 1)
  (define dn 2)

  (define x0 (quotient (- N dn) b0))
  (define c0 (+ (* x0 a0) cn))
  (define d0 (+ (* x0 b0) dn))
)

(time
(let pro ([a a0] [b b0]
	  [c c0] [d d0] [count 0])
  (cond
   ((and (= c cn) (= d dn)) count)
   (else
    (let* ([k (quotient (+ N b) d)]
	   [e (- (* k c) a)]
	   [f (- (* k d) b)])
      (pro c d e f (+ count 1))))))
)
#|
(time (let pro ...))
    no collections
    0.078125000s elapsed cpu time
    0.090140436s elapsed real time
    0 bytes allocated
7295372
|#
