(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define palind-10?
  (lambda (a)
    (let ([now (get-num a)])
      (equal? now (reverse now)))))

(define destru-base2
  (lambda (n)
    (cond
     ((= n 0) '())
     ((zero? (remainder n 2))
      (cons 0 (destru-base2 (quotient n 2))))
     (else
      (cons 1 (destru-base2 (quotient n 2)))))))
(define palind-2?
  (lambda (n)
    (let ([now (destru-base2 n)])
      (equal? now (reverse now)))))

(let pro ([i 1] [sum 0])
  (cond
   ((= i 1000000) sum)
   ((and (palind-10? i) (palind-2? i))
    (pro (+ i 1) (+ sum i)))
   (else
    (pro (+ i 1) sum))))

#|
(time (let pro ...))
    142 collections
    0.468750000s elapsed cpu time, including 0.000000000s collecting
    0.466519680s elapsed real time, including 0.002822508s collecting
    597926704 bytes allocated, including 597652488 bytes reclaimed
872187
|#
