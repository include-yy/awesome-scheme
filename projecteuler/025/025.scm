(define num-length
  (lambda (n)
    (string-length (number->string n))))

(define fib
  (lambda (n)
    (let f ([a 1][b 1][n n])
      (cond
       ((< n 0) #f)
       ((= n 0) 0)
       ((= n 1) b)
       ((= n 2) b)
       (else
	(f b (+ a b) (- n 1)))))))

(let pro ([i 12])
  (if (= 1000 (num-length (fib i)))
      i
      (pro (+ i 1))))
#|
(time (let pro ...))
    406 collections
    1.171875000s elapsed cpu time, including 0.000000000s collecting
    1.165280332s elapsed real time, including 0.006264396s collecting
    1731606576 bytes allocated, including 1732758992 bytes reclaimed
4782
|#

(let imp ([a 1] [b 1] [n 2])
  (cond
   ((= (num-length b) 1000) n)
   (else
    (imp b (+ a b) (+ n 1)))))

#|
(time (let imp ...))
    15 collections
    0.109375000s elapsed cpu time, including 0.000000000s collecting
    0.101465988s elapsed real time, including 0.000346764s collecting
    61809272 bytes allocated, including 63278032 bytes reclaimed
4782
|#
