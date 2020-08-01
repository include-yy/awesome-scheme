(begin
(define range 1000000)
(define sievebound (quotient (- range 1) 2))
(define crosslimit (quotient (- (exact (floor (sqrt range))) 1) 2))

(define prime (make-bytevector sievebound 1))
(define prime-ref
  (lambda (pme i)
    (bytevector-u8-ref pme (- i 1))))
(define prime-set!
  (lambda (pme i arg)
    (bytevector-u8-set! pme (- i 1) arg)))
    
(let filt ([i 1])
  (cond
   ((> i sievebound))
   ((= (prime-ref prime i) 1)
    (let f ([j (* 2 i (+ i 1))])
      (cond
       ((> j sievebound) (filt (+ i 1)))
       (else
	(prime-set! prime j 0)
	(f (+ j (+ (* i 2) 1)))))))
   (else
    (filt (+ i 1)))))

(define prime-r?
  (lambda (n)
    (cond
     ((<= n 0) #f)
     ((= n 1) #f)
     ((= n 2) #t)
     ((= (remainder n 2) 0) #f)
     ((= (prime-ref prime (/ (- n 1) 2)) 1) #t)
     (else #f))))
)

(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define get-eq-repeat
  (lambda (n)
    (let* ([num-ls (get-num n)]
	   [num-vec (list->vector num-ls)]
	   [num-len (vector-length num-vec)])
      (let f ([i 0] [ls '()])
	(cond
	 ((= i num-len) (reverse ls))
	 ((< (vector-ref num-vec i) 0)
	  (f (+ i 1) ls))
	 (else
	  (let ([now-num (vector-ref num-vec i)])
	    (let g ([j (+ i 1)] [now-ls (list i)])
	      (cond
	       ((>= j num-len) (f (+ i 1) (cons (reverse now-ls) ls)))
	       ((< (vector-ref num-vec j) 0)
		(g (+ j 1) now-ls))
	       ((= (vector-ref num-vec j) now-num)
		(vector-set! num-vec j -1)
		(g (+ j 1) (cons j now-ls)))
	       (else
		(g (+ j 1) now-ls)))))))))))

(define replace-hint
  (lambda (num ls i)
    (let ([num-str (number->string num)]
	  [char-i (integer->char (+ i (char->integer #\0)))])
      (let f ([now-ls ls])
	(cond
	 ((null? now-ls) (string->number num-str))
	 (else
	  (string-set! num-str (car now-ls) char-i)
	  (f (cdr now-ls))))))))

(let pro ([i 1])
  (cond
   [(= i range) #f]
   [(not (prime-r? i)) (pro (+ i 1))]
   [else
    (let f ([set-list (get-eq-repeat i)])
      (cond
       ((null? set-list) (pro (+ i 1)))
       (else
	(let* ([now-list (car set-list)]
	       [start (if (= 0 (car now-list)) 1 0)])
	  (let g ([j start] [count 0])
	    (cond
	     ((= j 10)
	      (if (= count 8) (list i now-list)
		  (f (cdr set-list))))
	     ((prime-r? (replace-hint i now-list j))
	      (g (+ j 1) (+ count 1)))
	     (else
	      (g (+ j 1) count))))))))]))

#|
(time (let pro ...))
    51 collections
    0.171875000s elapsed cpu time, including 0.000000000s collecting
    0.179749848s elapsed real time, including 0.000953268s collecting
    213438408 bytes allocated, including 214574040 bytes reclaimed
(121313 (0 2 4))
|#
