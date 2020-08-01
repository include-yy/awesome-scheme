(define penta
  (lambda (n)
    (* n (- (* n 3) 1) 1/2)))

(define pen-list
  (let f ([i 1])
    (cond
     ((= i 10000) '())
     (else
      (cons (penta i) (f (+ i 1)))))))

(let pro ([i 1])
  (let ([now1 (penta i)])
    (let f ([j 1])
      (cond
       ((= i j) (pro (+ i 1)))
       (else
	(let ([now2 (penta j)])
	  (if (and
	       (member (- now1 now2) pen-list)
	       (member (+ now1 now2) pen-list))
	      (- now1 now2)
	      (f (+ j 1)))))))))

#|
(time (let pro ...))
    4 collections
    29.359375000s elapsed cpu time, including 0.000000000s collecting
    30.967049428s elapsed real time, including 0.000149628s collecting
    18793104 bytes allocated, including 16846488 bytes reclaimed
5482660
|#
