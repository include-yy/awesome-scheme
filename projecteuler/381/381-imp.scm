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

(define n!-mod
  (lambda (n k)
    (cond
     ((< n 0) #f)
     ((= n 0) 1)
     ((= n 1) 1)
     (else
      (let f ([n n] [pow 1])
	(cond
	 ((= n 1) pow)
	 (else
	  (f (- n 1) (remainder (* pow n) k)))))))))

(define n!-k
  (lambda (n)
    (let* ([fst (n!-mod (- n 5) n)])
      (remainder (* 9 fst) n))))

(define prime-vec (eular-filter 100000000))
(define prime-len (vector-length prime-vec))

(time
(let pro ([i 2][sum 0])
  (cond
   ((= i 9592) sum)
   (else
    (pro (+ i 1) (+ sum (n!-k (vector-ref prime-vec i)))))))
)
#|
25      2     .000013  480
168     3       
1229    4     .05      2882332
9592    5   15.9219    226591981
78498   6   
664579  7
5761455 8
|#
