;;1_2_3_4_5_6_7_8_9_0

(define max 1929394959697989990)
(define min 1020304050607080900)

(define start (exact (floor (sqrt min))))
(define end (exact (ceiling (sqrt max))))

(define get-str
  (lambda (n)
    (let ([now (number->string n)])
      (let f ([i 0] [nls '()])
	(cond
	 ((> i 18) (list->string (reverse nls)))
	 (else
	  (f (+ i 2) (cons (string-ref now i) nls))))))))

(let pro ([i start])
  (cond
   ((= i end) #f)
   ((string=? (get-str (* i i)) "1234567890") i)
   (else
    (pro (+ i 10)))))

#|
(time (let pro ...))
    8782 collections
    36.625000000s elapsed cpu time, including 0.109375000s collecting
    36.720711400s elapsed real time, including 0.163024200s collecting
    36985620432 bytes allocated, including 36988073160 bytes reclaimed
1389019170
|#
