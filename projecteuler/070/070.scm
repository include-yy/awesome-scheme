(define N 10000000)
(define phi (make-vector N 0))
(vector-set! phi 1 1)

(let pro ([i 2])
  (cond
   ((= i N))
   ((zero? (vector-ref phi i))
    (let f ([j i])
      (cond
       ((>= j N) (pro (+ i 1)))
       ((zero? (vector-ref phi j))
	(vector-set! phi j j)
	(vector-set! phi j (* (- i 1) (vector-ref phi j) (/ i)))
	(f (+ j i)))
       (else
	(vector-set! phi j (* (- i 1) (vector-ref phi j) (/ i)))
	(f (+ j i))))))
   (else
    (pro (+ i 1)))))

(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define permutad?
  (lambda (i)
    (let ([ls (sort < (get-num i))]
	  [ls1 (sort < (get-num (vector-ref phi i)))])
      (equal? ls ls1))))

(let pro ([i 2] [now (cons 2 2)])
  (cond
   ((= i N) now)
   ((permutad? i)
    (if (> (cdr now) (/ i (vector-ref phi i)))
	(pro (+ i 1) (cons i (/ i (vector-ref phi i))))
	(pro (+ i 1) now)))
   (else (pro (+ i 1) now))))

#|
(time (begin (let pro ...) ...))
    3480 collections
    14.640625000s elapsed cpu time, including 0.484375000s collecting
    14.754872928s elapsed real time, including 0.513421620s collecting
    14653458408 bytes allocated, including 14693443624 bytes reclaimed
(8319823 . 8319823/8313928)
|#
