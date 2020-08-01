(define countSB
  (lambda (limit leftN leftD rightN rightD)
    (let ([medN (+ leftN rightN)]
	  [medD (+ leftD rightD)])
      (if (> medD limit) 0
	  (let ([count 1])
	    (+ count
	       (countSB limit leftN leftD medN medD)
	       (countSB limit medN medD rightN rightD)))))))

(countSB 10000 1 3 1 2)

#|
(time (countSB 10000 ...))
    no collections
    0.046875000s elapsed cpu time
    0.056046120s elapsed real time
    768 bytes allocated
5066251
|#

(define limit 10000)
(define stack (make-vector (/ limit 2) 0))

(let pro ([left 3] [right 2] [top 0] [count 0])
  (let ([med (+ left right)])
    (if (> med limit)
	(if (> top 0)
	    (pro right (vector-ref stack (- top 1)) (- top 1) count)
	    count)
	(begin
	  (vector-set! stack top right)
	  (pro left med (+ top 1) (+ count 1))))))

#|
(time (let pro ...))
    no collections
    0.046875000s elapsed cpu time
    0.051878736s elapsed real time
    0 bytes allocated
5066251
|#
