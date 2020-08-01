(define make-array
  (lambda (n)
    (make-vector n 0)))
(define array-ref
  (lambda (arr n)
    (vector-ref arr (- n 1))))
(define array-set!
  (lambda (arr n val)
    (vector-set! arr (- n 1) val)))

(define one-nineteen
  (let ([arr (make-array 19)])
    (array-set! arr 1 3);;one = 3
    (array-set! arr 2 3);;two = 3
    (array-set! arr 3 5);;three = 5
    (array-set! arr 4 4);;four = 4
    (array-set! arr 5 4);;five = 4
    (array-set! arr 6 3);;six = 3
    (array-set! arr 7 5);;seven = 5
    (array-set! arr 8 5);;eight = 5
    (array-set! arr 9 4);;nine = 4
    (array-set! arr 10 3);;ten = 3
    (array-set! arr 11 6);;eleven = 6
    (array-set! arr 12 6);;twelve = 6
    (array-set! arr 13 8);;thirteen = 8
    (array-set! arr 14 8);;fourteen = 8
    (array-set! arr 15 7);;fifteen = 7
    (array-set! arr 16 7);;sixteen = 7
    (array-set! arr 17 9);;seventeen = 9
    (array-set! arr 18 8);;eighteen = 8
    (array-set! arr 19 8);;nineteen = 8
    arr))

(define twenty-ninety
  (let ([arr (make-array 9)])
    (array-set! arr 1 0)
    (array-set! arr 2 6);;twenty = 6
    (array-set! arr 3 6);;thirty = 6
    (array-set! arr 4 5);;forty = 5
    (array-set! arr 5 5);;fifty = 5
    (array-set! arr 6 5);;sixty = 5
    (array-set! arr 7 7);;seventy = 7
    (array-set! arr 8 6);;eighty = 6
    (array-set! arr 9 6);;ninety = 6
    arr))

(define hundred 7);;hundred = 7

(define and* 3);;and = 3

(let f ([i 1] [sum 0])
  (cond
   ((= i 1000) (+ sum (+ 3 8)));;onethousand 11
   (else
    (let ([baiwei (quotient i 100)]
	  [shiwei (remainder i 100)]
	  [now-sum 0])
      (when (not (zero? baiwei))
	(set! now-sum (+ now-sum hundred))
	(set! now-sum (+ now-sum (array-ref one-nineteen baiwei)))
	(when (not (zero? shiwei))
	  (set! now-sum (+ now-sum and*))))
      (cond
       ((> shiwei 19)
	(let ([shi (quotient shiwei 10)]
	      [ge (remainder shiwei 10)])
	  (set! now-sum (+ now-sum (array-ref twenty-ninety shi)))
	  (when (not (zero? ge))
	    (set! now-sum (+ now-sum (array-ref one-nineteen ge))))
	  (f (+ i 1) (+ sum now-sum))))
       (else
	(cond
	 ((zero? shiwei)
	  (f (+ i 1) (+ sum now-sum)))
	 (else
	  (set! now-sum (+ now-sum (array-ref one-nineteen shiwei)))
	  (f (+ i 1) (+ sum now-sum))))))))))
