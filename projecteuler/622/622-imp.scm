(define fin-vec (make-vector 61 0))

(let f ([i 1])
  (cond
   ((> i 60) #t)
   (else
    (vector-set! fin-vec i (expt 2 i))
    (f (+ i 1)))))

(define find-60?
  (lambda (n)
    (let f ([i 1])
      (cond
       ((> i 60) #f)
       ((= 1 (remainder (vector-ref fin-vec i) (- n 1)))
	(if (= i 60) #t #f))
       (else
	(f (+ i 1)))))))
       
(do ([i 2 (+ i 2)]
     [tot '() (if (find-60? i) (cons i tot) tot)])
    ((> i 1000) (reverse tot)))

;;still slow
