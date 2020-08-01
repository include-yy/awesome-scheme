(define base-n
  (lambda (b)
    (lambda (n)
      (/ (- (expt b n) 1) (- b 1)))))

(define finite (expt 10 12))
(define half (expt 10 6))

(define table (make-vector 10000001 '()))

(define find
  (lambda (range)
    (let f ([base 2])
      (let ([fun (base-n base)])
	(let g ([i 3])
	  (let ([now-val (fun i)])
	    (cond
	     ((> now-val range)
	      (if (= i 3)
		  #t
		  (f (+ base 1))))
	     (else
	      (let-values ([(index rest) (exact-integer-sqrt now-val)])
		(cond
		 ((member rest (vector-ref table index))
		  (g (+ i 1)))
		 (else
		  (vector-set! table index (cons rest (vector-ref table index)))
		  (g (+ i 1)))))))))))))

(define find-val
  (lambda (vec)
    (let ([len (vector-length vec)])
      (let f ([i 0] [tot 1])
	(cond
	 ((= i len) tot)
	 (else
	  (let ([double (expt i 2)])
	    (f (+ i 1) (+ tot (* double (length (vector-ref table i))) (fold-left (lambda (x y) (+ x y)) 0 (vector-ref table i)))))))))))

(begin
  (find finite)
  (find-val table))

#|
(time (begin (find finite) ...))
    158 collections
    1.781250000s elapsed cpu time, including 0.125000000s collecting
    1.780730100s elapsed real time, including 0.139314400s collecting
    669366784 bytes allocated, including 658394928 bytes reclaimed
336108797689259276
|#
