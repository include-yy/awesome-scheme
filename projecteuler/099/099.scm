(define base_exp-vec (make-vector 1000 #f))


(call-with-input-file "p099_base_exp.txt"
  (lambda (po)
    (let f ([i 0] [now (get-datum po)])
      (cond
       ((= i 1000))
       (else
	(get-char po)
	(let ([noww (get-datum po)])
	  (vector-set! base_exp-vec i (cons now noww))
	  (f (+ i 1) (get-datum po))))))))

(let pro ([i 0] [max (cons 0 0)])
  (cond
   ((= i 1000) max)
   (else
    (let* ([ve (vector-ref base_exp-vec i)]
	   [no (* (cdr ve) (log (car ve)))])
      (if (> no (cdr max))
	  (pro (+ i 1) (cons i no))
	  (pro (+ i 1) max))))))

#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.000124500s elapsed real time
    32104 bytes allocated
(708 . 6919995.552420337)
|#
