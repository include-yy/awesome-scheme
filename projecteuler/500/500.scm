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

(define prime-vec (eular-filter 10000000))
(define exp-vec (make-vector 500500 1))

(let pro ([i 0] [j 500499])
  (cond
   ((= i j))
   (else
    (let ([i-now (vector-ref prime-vec i)]
	  [i-exp (vector-ref exp-vec i)]
	  [j-now (vector-ref prime-vec j)]
	  [j-exp (vector-ref exp-vec j)])
      (let ([i-inc (expt i-now (expt 2 i-exp))])
	(cond
	 ((< i-inc j-now)
	  (vector-set! exp-vec i (+ i-exp 1))
	  (vector-set! exp-vec j (- j-exp 1))
	  (if (= 1 j-exp)
	      (pro i (- j 1))
	      (pro i j)))
	 (else
	  (pro (+ i 1) j))))))))

(do ([i 0 (+ i 1)]
     [pow 1 (remainder (* pow (expt (vector-ref prime-vec i) (- (expt 2 (vector-ref exp-vec i)) 1))) 500500507)])
    ((zero? (vector-ref exp-vec i)) (remainder pow 500500507)))


#|
(time (begin (let pro ...) ...))
    4 collections
    0.062500000s elapsed cpu time, including 0.015625000s collecting
    0.068659700s elapsed real time, including 0.006388200s collecting
    15940208 bytes allocated, including 16868416 bytes reclaimed
35407281
|#
