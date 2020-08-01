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
(define five-mi 50000000)
(define prime-vec (eular-filter 10000))
(define sieve (make-bytevector 50000000 0))

(let pro ([i 0])
  (let* ([now1 (vector-ref prime-vec i)]
	 [now1t (expt now1 2)])
    (cond
     [(> now1t five-mi)]
     [else
      (let f ([j 0])
	(let* ([now2 (vector-ref prime-vec j)]
	       [now2t (expt now2 3)])
	  (cond
	   [(> (+ now2 now2t) five-mi)
	    (pro (+ i 1))]
	   [else
	    (let g ([k 0])
	      (let* ([now3 (vector-ref prime-vec k)]
		     [now3t (expt now3 4)])
		(cond
		 [(> (+ now1t now2t now3t) five-mi)
		  (f (+ j 1))]
		 [else
		  (bytevector-u8-set! sieve (+ now1t now2t now3t -1) 1)
		  (g (+ k 1))])))])))])))

(do ([i 0 (+ i 1)]
     [sum 0 (if (= (bytevector-u8-ref sieve i) 1) (+ sum 1) sum)])
    ((= i five-mi) sum))

#|
(time (begin (let pro ...) ...))
    5 collections
    0.187500000s elapsed cpu time, including 0.000000000s collecting
    0.190603100s elapsed real time, including 0.000147500s collecting
    18234960 bytes allocated, including 21013704 bytes reclaimed
1097343
|#
