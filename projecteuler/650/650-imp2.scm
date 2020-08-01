(define-syntax v-r
  (syntax-rules ()
    [(_ p i)
     (vector-ref p i)]))
(define-syntax v-s!
  (syntax-rules ()
    [(_ p i n)
     (vector-set! p i n)]))
(define-syntax v-s+
  (syntax-rules ()
    [(_ p i n)
     (vector-set! p i (+ n (vector-ref p i)))]))
(define powermod
  (lambda (n k m)
    (let f ([n n] [k k] [r 1])
      (cond
       ((= k 0) r)
       ((= (remainder k 2) 1)
	(f n (- k 1) (remainder (* r n) m)))
       (else
	(f (remainder (* n n) m) (/ k 2) r))))))


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

(define MD (+ (expt 10 9) 7))
(define prime-vec (eular-filter 20000))
(define prime-len (vector-length prime-vec))
(define facte-vec (make-vector prime-len 0))


(define divide
  (lambda (x k)
    (let f ([n x] [i 0])
      (cond
       ((= n 1))
       (else
	(let ([pri-now (v-r prime-vec i)])
	  (let g ([n n])
	    (cond
	     ((zero? (remainder n pri-now))
	      (v-s+ facte-vec i k)
	      (g (/ n pri-now)))
	     (else (f n (+ i 1)))))))))))


(let pro ([i 2] [ans 1])
  (cond
   ((= i 20001) ans)
   (else
    (vector-fill! facte-vec 0)
    (do ([j 2 (+ j 1)])
	((> j i))
      (divide j (- (* 2 j) i 1)))
    (let f ([tmp 1][j 0])
      (cond
       ((= j prime-len) (pro (+ i 1) (remainder (+ ans tmp) MD)))
       (else
	(f (remainder (* (- (powermod (v-r prime-vec j)
				      (+ (v-r facte-vec j) 1)
				      MD)
			    1)
			 (powermod (- (v-r prime-vec j) 1)
				   (- MD 2)
				   MD)
			 tmp)
		      MD)
	   (+ j 1))))))))

#|
(time (let pro ...))
    11254 collections
    492.718750000s elapsed cpu time, including 0.234375000s collecting
    493.194406800s elapsed real time, including 0.315834500s collecting
    47535755352 bytes allocated, including 47537132848 bytes reclaimed
538319652
|#
