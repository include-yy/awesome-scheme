(define rom-vec (make-vector 1000 #f))

(call-with-input-file "p089_roman.txt"
  (lambda (po)
    (let f ([i 0])
      (cond
       ((= i 1000))
       (else
	(let ([now (get-line po)])
	  (vector-set! rom-vec i now)
	  (f (+ i 1))))))))

(define rom-to-digit
  (lambda (chr)
    (cond
     ((char=? chr #\I) 1)
     ((char=? chr #\V) 5)
     ((char=? chr #\X) 10)
     ((char=? chr #\L) 50)
     ((char=? chr #\C) 100)
     ((char=? chr #\D) 500)
     ((char=? chr #\M) 1000)
     (else #f))))

(define digit-to-rom
  (lambda (dig)
    (cond
     ((= dig 1) #\I)
     ((= dig 5) #\V)
     ((= dig 10) #\X)
     ((= dig 50) #\L)
     ((= dig 100) #\C)
     ((= dig 500) #\D)
     ((= dig 1000) #\M))))

(define rom-parse
  (lambda (str)
    (let-syntax ([s-r (syntax-rules ()
			[(_ i)
			 (string-ref str i)])]
		 [rtd (syntax-rules ()
			[(_ c)
			 (rom-to-digit c)])]
		 [dtr (syntax-rules ()
			[(_ d)
			 (digit-to-rom d)])])
      (let ([len (string-length str)])
	(let f ([i 0] [sum 0])
	  (cond
	   ((= i len) sum)
	   ((= i (- len 1))
	    (+ sum (rtd (s-r i))))
	   ((< (rtd (s-r i)) (rtd (s-r (+ i 1))))
	    (let* ([j i]
		   [sum-in (- (rtd (s-r j)))])
	      (f (+ i 2) (+ sum (+ sum-in (rtd (s-r (+ j 1))))))))
	   (else
	    (f (+ i 1) (+ sum (rtd (s-r i)))))))))))

(define dig-parse
  (lambda (dig)
    (letrec-syntax ([rtd (syntax-rules ()
			[(_ c)
			 (rom-to-digit c)])]
		 [dtr (syntax-rules ()
			[(_ d)
			 (digit-to-rom d)])]
		 [s-add (syntax-rules ()
			  [(_ str dig)
			   (string-append str (string (dtr dig)))])])
      (let f ([num dig] [now-str ""])
	(cond
	 ((zero? num) now-str)
	 ((>= num 1000)
	  (f (- num 1000) (s-add now-str 1000)))
	 ((>= num 900)
	  (f (- num 900) (string-append now-str "CM")))
	 ((>= num 500)
	  (f (- num 500) (s-add now-str 500)))
	 ((>= num 400)
	  (f (- num 400) (string-append now-str "CD")))
	 ((>= num 100)
	  (f (- num 100) (s-add now-str 100)))
	 ((>= num 90)
	  (f (- num 90) (string-append now-str "XC")))
	 ((>= num 50)
	  (f (- num 50) (s-add now-str 50)))
	 ((>= num 40)
	  (f (- num 40) (string-append now-str "XL")))
	 ((>= num 10)
	  (f (- num 10) (s-add now-str 10)))
	 ((>= num 9)
	  (f (- num 9) (string-append now-str "IX")))
	 ((>= num 5)
	  (f (- num 5) (s-add now-str 5)))
	 ((>= num 4)
	  (f (- num 4) (string-append now-str "IV")))
	 (else
	  (f (- num 1) (s-add now-str 1))))))))

(let f ([i 0][sum 0])
  (cond
   ((= i 1000) sum)
   (else
    (let* ([now (vector-ref rom-vec i)]
	   [dig (rom-parse now)]
	   [rom (dig-parse dig)])
      (f (+ i 1) (+ sum (- (string-length now)
			   (string-length rom))))))))
;;743
