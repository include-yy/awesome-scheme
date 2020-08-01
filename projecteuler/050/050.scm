(define range 1000000)
(define sievebound (quotient (- range 1) 2))
(define crosslimit (quotient (- (exact (floor (sqrt range))) 1) 2))

(define prime (make-bytevector sievebound 1))
(define prime-ref
  (lambda (pme i)
    (bytevector-u8-ref pme (- i 1))))
(define prime-set!
  (lambda (pme i arg)
    (bytevector-u8-set! pme (- i 1) arg)))
    
(let filt ([i 1])
  (cond
   ((> i sievebound))
   ((= (prime-ref prime i) 1)
    (let f ([j (* 2 i (+ i 1))])
      (cond
       ((> j sievebound) (filt (+ i 1)))
       (else
	(prime-set! prime j 0)
	(f (+ j (+ (* i 2) 1)))))))
   (else
    (filt (+ i 1)))))

(define prime-r?
  (lambda (n)
    (cond
     ((<= n 0) #f)
     ((= n 1) #f)
     ((= n 2) #t)
     ((= (remainder n 2) 0) #f)
     ((= (prime-ref prime (/ (- n 1) 2)) 1) #t)
     (else #f))))

(define prime-list
  (cons 2
	(let f ([i 3])
	  (cond
	   ((> i 1000000) '())
	   ((prime-r? i)
	    (cons i (f (+ i 2))))
	   (else
	    (f (+ i 2)))))))

(let pro ([ls prime-list] [max (cons 5 2)])
  (cond
   ((> (car ls) 500000) max)
   (else
    (let f ([sum (car ls)] [count 1] [now-ls (cdr ls)] [now-max max])
      (cond
       ((>= sum 1000000)
	(if (> (cdr now-max) (cdr max))
	    (pro (cdr ls) now-max)
	    (pro (cdr ls) max)))
       ((prime-r? sum)
	(if (> count (cdr now-max))
	    (f (+ sum (car now-ls)) (+ count 1) (cdr now-ls) (cons sum count))
	    (f (+ sum (car now-ls)) (+ count 1) (cdr now-ls) now-max)))
       (else
	(f (+ sum (car now-ls)) (+ count 1) (cdr now-ls) now-max)))))))

#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.014482836s elapsed real time
    424 bytes allocated
(997651 . 543)
|#
