;;B(n) = PI (k = 1, 1, n) (expt k (2k - n - 1))

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

(define prime-vec (eular-filter 20000))
(define prime-sieve (do ([i 0 (+ i 1)] [vec (make-vector 20000 0)])
			([= i (vector-length prime-vec)] vec)
		      (vector-set! vec (- (vector-ref prime-vec i) 1) 1)))
(define prime?
  (lambda (n)
    (not (zero? (vector-ref prime-sieve (- n 1))))))
		      
(define Bn
  (lambda (n)
    (let f ([k 1])
      (cond
       ((= k (+ n 1)) '())
       (else
	(let ([index (- (* 2 k) n 1)])
	  (if (not (zero? index)) 
	      (cons (cons k index) (f (+ k 1)))
	      (f (+ k 1)))))))))

(define prime-convert
  (lambda (par)
    (let ([fst (car par)]
	  [sec (cdr par)])
      (let f ([i 0] [now fst] [fin-ls '()])
	(let ([prime-now (vector-ref prime-vec i)])
	  (cond
	   ((= now 1) (reverse (map (lambda (x) (cons (car x) (* sec (cdr x)))) fin-ls)))
	   (else
	    (let g ([count 0] [now1 now])
	      (cond
	       ((zero? (remainder now1 prime-now))
		(g (+ count 1) (/ now1 prime-now)))
	       (else
		(if (zero? count)
		    (f (+ i 1) now1 fin-ls)
		    (f (+ i 1) now1 (cons (cons prime-now count) fin-ls)))))))))))))

(define deal
  (lambda (pair-ls)
    (let f ([ls pair-ls])
      (cond
       [(null? ls) pair-ls]
       [(prime? (caar ls)) (f (cdr ls))]
       [else
	(set! pair-ls (remove (car ls) pair-ls))
	(let ([now-conv (prime-convert (car ls))])
	  (let g ([now now-conv])
	    (cond
	     [(null? now) (f (cdr ls))]
	     [else
	      (let* ([fst (car now)]
		     [ls-now (memp (lambda (x) (equal? (car x) (car fst))) pair-ls)])
		(set-car! ls-now (cons (car fst)
				       (+ (cdr fst) (cdr (car ls-now)))))
		(g (cdr now)))])))]))))

(define myexpt
  (lambda (bs pw)
    (let f ([exp pw] [bs bs] [sum 1])
      (cond
       ((= exp 0) sum)
       ((zero? (remainder exp 2))
	(f (/ exp 2) (* bs bs) sum))
       (else
	(f (- exp 1) bs (* sum bs)))))))

(define sum-of-div
  (lambda (par)
    (let ([fst (car par)] [sec (cdr par)])
      (/ (- (myexpt fst (+ sec 1)) 1) (- fst 1)))))

(define pi-sum
  (lambda (par-ls)
    (let f ([ls par-ls] [pow 1])
      (cond
       ((null? ls) (remainder pow 1000000007))
       (else
	(f (cdr ls) (remainder (* pow (sum-of-div (car ls))) 100000007)))))))
(time
(let pro ([i 1] [sum 0])
  (cond
   ((= i 1001) (remainder sum 1000000007))
   (else
    (let ([now (deal (Bn i))])
      (pro (+ i 1) (remainder (+ sum (pi-sum now)) 1000000007))))))
)
#|
(time (let pro ...))
    2523 collections
    148.359375000s elapsed cpu time, including 0.125000000s collecting
    148.486827900s elapsed real time, including 0.127204100s collecting
    10776782984 bytes allocated, including 10776743920 bytes reclaimed
806535940
|#
;;still slow

