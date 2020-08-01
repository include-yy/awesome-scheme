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

(define prime-vec (eular-filter 100000000))
(define prime-len (vector-length prime-vec))

(define findit
  (lambda (pri)
    (let ([k+1? (integer? (/ (- pri 1) 6))])
      (let f ([k 0])
	(let ([now (/ (- (* (+ (* k 2) 1) pri) 1) 24)])
	(cond
	 ((integer? now) (remainder (+ pri
			    (/ (- pri 1) 2)
			    (if k+1?
				(- pri (/ (- pri 1) 6))
				(/ (+ pri 1) 6))
			    now) pri))
	 (else
	  (f (+ k 1)))))))))

(let pro ([i 2] [sum 0])
  (cond
   ((= i prime-len) sum)
   (else
    (pro (+ i 1) (+ sum (findit (vector-ref prime-vec i)))))))

#|
(time (let pro ...))
    216 collections
    3.171875000s elapsed cpu time, including 0.062500000s collecting
    3.180738900s elapsed real time, including 0.067490800s collecting
    913500616 bytes allocated, including 1356434312 bytes reclaimed
139602943319822
|#

#|
for prime p:
(p - 1)! == (p - 1) (mod p)
(p - 2)! == 1 (mod p)
(p - 3)! == (p - 1)/ 2 (mod p)
(p - 4)! == (p - (p - 1) / 6) (mod p) {p = 6k + 1}
         == (p + 1) / 6 (mod p) {p = 6k - 1}

