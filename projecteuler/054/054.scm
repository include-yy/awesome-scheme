(define playerall (make-vector 1000 0))
(do ([i 0 (+ i 1)]) ((= i 1000) #t)
  (vector-set! playerall i (make-vector 10 0)))

(call-with-input-file "p054_poker.txt"
  (lambda (po)
    (let f ([j 0] [i 0])
      (let* ([fst (get-char po)]
	     [sec (get-char po)])
	(get-char po)
	(cond
	 ((eof-object? fst))
	 ((= i 10)
	  (vector-set! (vector-ref playerall (+ j 1)) 0 (cons fst sec))
	  (f (+ j 1) 1))
	 (else
	  (vector-set! (vector-ref playerall j) i (cons fst sec))
	  (f j (+ i 1))))))))

(define player1 (make-vector 1000 0))
(define player2 (make-vector 1000 0))
(do ([i 0 (+ i 1)]) ((= i 1000) #t)
  (vector-set! player1 i (make-vector 5 0))
  (vector-set! player2 i (make-vector 5 0)))

(do ([i 0 (+ i 1)]) ((= i 1000) #t)
  (do ([j 0 (+ j 1)]) ((= j 5) #t)
    (vector-set! (vector-ref player1 i) j
		 (vector-ref (vector-ref playerall i) j)))
  (do ([j 5 (+ j 1)]) ((= j 10) #t)
    (vector-set! (vector-ref player2 i) (- j 5)
		 (vector-ref (vector-ref playerall i) j))))
(do ([i 0 (+ i 1)]) ((= i 1000) #t)
  (vector-sort! (lambda (a b) (< (pvalue->digit a) (pvalue->digit b))) (vector-ref player1 i))
  (vector-sort! (lambda (a b) (< (pvalue->digit a) (pvalue->digit b))) (vector-ref player2 i)))

(define pvalue
  (lambda (con)
    (car con)))
(define psuit
  (lambda (con)
    (cdr con)))

(define pvalue->digit
  (let ([mapvec (vector 1 2 3 4 5 6 7 8 9 10)])
    (lambda (con)
      (let ([val (pvalue con)])
	(cond
	 ((and (char<=? #\1 val) (char<=? val #\9))
	  (let ([now (- (char->integer val) (char->integer #\0))])
	    (vector-ref mapvec (- now 1))))
	 ((eqv? val #\T) 10)
	 ((eqv? val #\J) 11)
	 ((eqv? val #\Q) 12)
	 ((eqv? val #\K) 13)
	 ((eqv? val #\A) 14)
	 (else 0))))))
(define psuit->digit
  (lambda (con)
    (let ([new (psuit con)])
      (cond
       ((char=? new #\H) 1)
       ((char=? new #\C) 2)
       ((char=? new #\S) 3)
       ((char=? new #\D) 4)))))

(define ColorSame?
  (lambda (sgvec)
    (char=? (psuit (vector-ref sgvec 0))
	    (psuit (vector-ref sgvec 1))
	    (psuit (vector-ref sgvec 2))
	    (psuit (vector-ref sgvec 3))
	    (psuit (vector-ref sgvec 4)))))

(define HighCard
  (lambda (sgvec)
    (max (pvalue->digit (vector-ref sgvec 0))
	 (pvalue->digit (vector-ref sgvec 1))
	 (pvalue->digit (vector-ref sgvec 2))
	 (pvalue->digit (vector-ref sgvec 3))
	 (pvalue->digit (vector-ref sgvec 4)))))

(define OnePair
  (lambda (spvec)
    (let f ([i 4])
      (cond
       ((= i 0) 0)
       ((= (pvalue->digit (vector-ref spvec i))
	   (pvalue->digit (vector-ref spvec (- i 1))))
	(pvalue->digit (vector-ref spvec i)))
       (else
	(f (- i 1)))))))

(define TwoPair
  (lambda (spvec)
    (let f ([i 4] [dig 0] [count 0])
      (cond
       ((<= i 0)
	(if (= count 2) dig 0))
       ((= (pvalue->digit (vector-ref spvec i))
	   (pvalue->digit (vector-ref spvec (- i 1))))
	(f (- i 2) (+ (* dig 10) (pvalue->digit (vector-ref spvec i))) (+ count 1)))
       (else (f (- i 1) dig count))))))

(define ThreeKind
  (lambda (spvec)
    (let f ([i 4])
      (cond
       ((< i 2) 0)
       ((= (pvalue->digit (vector-ref spvec i))
	   (pvalue->digit (vector-ref spvec (- i 1)))
	   (pvalue->digit (vector-ref spvec (- i 2))))
	(pvalue->digit (vector-ref spvec i)))
       (else (f (- i 1)))))))

(define Straight
  (lambda (spvec)
    (cond
     ((and (= (+ 1 (pvalue->digit (vector-ref spvec 0)))
	      (pvalue->digit (vector-ref spvec 1)))
	   (= (+ 1 (pvalue->digit (vector-ref spvec 1)))
	      (pvalue->digit (vector-ref spvec 2)))
	   (= (+ 1 (pvalue->digit (vector-ref spvec 2)))
	      (pvalue->digit (vector-ref spvec 3)))
	   (= (+ 1 (pvalue->digit (vector-ref spvec 3)))
	      (pvalue->digit (vector-ref spvec 4))))
      (pvalue->digit (vector-ref spvec 0)))
     (else 0))))

(define Flush
  (lambda (spvec)
    (cond
     ((= (psuit->digit (vector-ref spvec 0))
	 (psuit->digit (vector-ref spvec 1))
	 (psuit->digit (vector-ref spvec 2))
	 (psuit->digit (vector-ref spvec 3))
	 (psuit->digit (vector-ref spvec 4)))
      1)
     (else 0))))

(define FullHouse
  (lambda (spvec)
    (cond
     ((and (= (pvalue->digit (vector-ref spvec 0))
	      (pvalue->digit (vector-ref spvec 1)))
	   (= (pvalue->digit (vector-ref spvec 2))
	      (pvalue->digit (vector-ref spvec 3))
	      (pvalue->digit (vector-ref spvec 4))))
      (+ (* 10 (pvalue->digit (vector-ref spvec 2)))
	 (pvalue->digit (vector-ref spvec 0))))
     ((and (= (pvalue->digit (vector-ref spvec 3))
	      (pvalue->digit (vector-ref spvec 4)))
	   (= (pvalue->digit (vector-ref spvec 0))
	      (pvalue->digit (vector-ref spvec 1))
	      (pvalue->digit (vector-ref spvec 2))))
      (+ (* 10 (pvalue->digit (vector-ref spvec 0)))
	 (pvalue->digit (vector-ref spvec 3))))
     (else 0))))

(define FourKind
  (lambda (spvec)
    (cond
     ((= (pvalue->digit (vector-ref spvec 0))
	 (pvalue->digit (vector-ref spvec 1))
	 (pvalue->digit (vector-ref spvec 2))
	 (pvalue->digit (vector-ref spvec 3)))
      (pvalue->digit (vector-ref spvec 0)))
     ((= (pvalue->digit (vector-ref spvec 1))
	 (pvalue->digit (vector-ref spvec 2))
	 (pvalue->digit (vector-ref spvec 3))
	 (pvalue->digit (vector-ref spvec 4)))
      (pvalue->digit (vector-ref spvec 1)))
     (else 0))))

(define StraightFlush
  (lambda (spvec)
    (let ([a (Straight spvec)]
	  [b (Flush spvec)])
      (cond
       ((or (zero? a) (zero? b)) 0)
       (else a)))))

(define transfer
  (lambda (spvec)
    (list (StraightFlush spvec)
	  (FourKind spvec)
	  (FullHouse spvec)
	  (Flush spvec)
	  (Straight spvec)
	  (ThreeKind spvec)
	  (TwoPair spvec)
	  (OnePair spvec)
	  (HighCard spvec))))

(define compare
  (lambda (sp1 sp2)
    (cond
     ((> (car sp1) (car sp2)) 1)
     ((< (car sp1) (car sp2)) -1)
     ((= (car sp1) (car sp2))
      (compare (cdr sp1) (cdr sp2))))))
      
(do ([i 0 (+ i 1)] [count 0 (if (> (compare (transfer (vector-ref player1 i))
					    (transfer (vector-ref player2 i))) 0) (+ count 1) count)])
    ((= i 1000) count))
;;376
