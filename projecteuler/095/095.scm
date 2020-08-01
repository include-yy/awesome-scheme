(define eular-filter
  (lambda (n)
    (let-syntax ([v-ref (syntax-rules ()
			  [(_ vec i)
			   (vector-ref vec (- i 1))])]
		 [v-set! (syntax-rules ()
			   [(_ vec i j)
			    (vector-set! vec (- i 1) j)])])
      (let ([vec (make-vector n 1)]
	    [half (quotient n 2)]
	    [prime-ls (list 2)])
	(letrec ([add-tail! (let ([ptr prime-ls])
			     (lambda (new)
			       (set-cdr! ptr (list new))
			       (set! ptr (cdr ptr))))])
	  (v-set! vec 1 0)
	  (v-set! vec 4 0)
	  (let f ([i 3])
	    (cond
	     [(> i half) (let f ([j i])
			   (cond
			    [(> j n) (list->vector prime-ls)]
			    [(not (zero? (v-ref vec j)))
			     (add-tail! j)
			     (f (+ j 2))]
			    [else (f (+ j 2))]))]
	     [else
	      (let g ([ls prime-ls])
		(cond
		 [(null? ls)
		  (when (not (zero? (v-ref vec i)))
		    (add-tail! i)
		    (if (<= (* i i) n) (v-set! vec (* i i) 0)))
		  (f (+ i 1))]
		 [else
		  (let ([now (car ls)])
		    (cond
		     [(> (* now i) n)
		      (if (not (zero? (v-ref vec i))) (add-tail! i))
		      (f (+ i 1))]
		     [(zero? (remainder i now))
		      (v-set! vec (* i now) 0)
		      (if (not (zero? (v-ref vec i))) (add-tail! i))
		      (f (+ i 1))]
		     [else
		      (v-set! vec (* now i) 0)
		      (g (cdr ls))]))]))])))))))

(define one-bi 1000000)
(define prime-vec (eular-filter one-bi))

(define sod
  (lambda (n)
    (cond
     ((= n 1) 0)
     ((= n 0) 0)
     (else
      (let loop ([i 0] [ne n] [sum 1])
	(let ([now (vector-ref prime-vec i)])
	  (cond
	   ((and (<= (* now now) n) (> ne 1))
	    (cond
	     ((zero? (remainder n now))
	      (let f ([p (* now now)] [num (/ ne now)])
		(cond
		 ((zero? (remainder num now))
		  (f (* p now) (/ num now)))
		 (else
		  (loop (+ i 1) num (/ (* sum (- p 1)) (- now 1)))))))
	     (else
	      (loop (+ i 1) ne sum))))
	   (else
	    (if (= ne 1)
		(- sum n)
		(- (* (+ ne 1) sum) n))))))))))

(define find-chain
  (lambda (n)
    (let f ([ne n] [sum (list n)])
      (let ([now (sod ne)])
	(cond
	 ((> now one-bi) 0)
	 ((= now n) (length sum))
	 ((memq now sum) 0)
	 (else
	  (f now (cons now sum))))))))

(let pro ([i 1] [res (cons 1 0)])
  (cond
   ((> i 1000000) res)
   (else
    (let ([now (find-chain i)])
      (cond
       ((> now (cdr res))
	(pro (+ i 1) (cons i now)))
       (else
	(pro (+ i 1) res)))))))

#|
(time (let pro ...))
    23 collections
    5.968750000s elapsed cpu time, including 0.000000000s collecting
    5.960049800s elapsed real time, including 0.000956500s collecting
    97766920 bytes allocated, including 96822824 bytes reclaimed
(14316 . 28)
|#
