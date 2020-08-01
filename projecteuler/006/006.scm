(define sum-of-square
  (lambda (n)
    (/ (* n (+ n 1) (+ (* n 2) 1)) 6)))

(define square-of-sum
  (lambda (n)
    (let ([sum (/ (* n (+ n 1)) 2)])
      (* sum sum))))

(let ()
  (abs (- (sum-of-square 100)
	  (square-of-sum 100))))
#|
(time (let () ...))
    no collections
    0.000000000s elapsed cpu time
    0.000000000s elapsed real time
    0 bytes allocated
25164150
|#
