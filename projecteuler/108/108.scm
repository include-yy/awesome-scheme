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

(define prime-vec (eular-filter 1000000))

(define sum-of-factor
  (lambda (n)
    (let loop ([n n] [i 0] [tt 1])
      (cond
       ((= n 1) tt)
       (else
	(let f ([n n] [cnt 0])
	  (cond
	   ((zero? (remainder n (vector-ref prime-vec i)))
	    (f (/ n (vector-ref prime-vec i)) (+ cnt 1)))
	   (else
	    (loop n (+ i 1) (* tt (+ cnt 1)))))))))))


(let pro ([i 1000])
  (cond
   ((> (/ (sum-of-factor (* i i)) 2) 1000) i)
   (else
    (pro (+ i 1)))))

#|
(time (let pro ...))
    2 collections
    8.531250000s elapsed cpu time, including 0.000000000s collecting
    8.543322000s elapsed real time, including 0.001148900s collecting
    7972896 bytes allocated, including 12144104 bytes reclaimed
180180
|#
