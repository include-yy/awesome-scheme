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

(define nearest
  (lambda (p q N)
    (let ([p0 (do ([pn p (* pn p)])
		  ((> pn N) (/ pn p)))])
      (let f ([px p0] [qx q] [nea 0])
	(let ([now (* px qx)])
	  (cond
	   ((= px 1) nea)
	   ((> now N)
	    (f (/ px p) qx nea))
	   (else
	    (cond
	     ((> (- N nea) (- N now))
	      (f px (* qx q) now))
	     (else
	      (f px (* qx q) nea))))))))))

(define N 10000000)
(define prime-vec (eular-filter N))
(define prime-len (vector-length prime-vec))

(let pro ([i 0] [sum 0])
  (let ([fst (vector-ref prime-vec i)]
	[sec (vector-ref prime-vec (+ i 1))])
    (cond
     ((> (* fst sec) N) sum)
     (else
      (let f ([j (+ i 1)] [sum2 0])
	(let ([ss (vector-ref prime-vec j)])
	  (cond
	   ((= j prime-len)
	    (pro (+ i 1) (+ sum sum2)))
	   ((> (* fst ss) N)
	    (pro (+ i 1) (+ sum sum2)))
	   (else
	    (f (+ j 1) (+ sum2 (nearest fst ss N)))))))))))

#|
(time (let pro ...))
    62 collections
    1.062500000s elapsed cpu time, including 0.000000000s collecting
    1.055281100s elapsed real time, including 0.003745900s collecting
    263573768 bytes allocated, including 261838000 bytes reclaimed
11109800204052
|#
