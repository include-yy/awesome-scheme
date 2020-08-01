(log (* 8 (expt 10 6)) 4)
;; -> 14.468208906450679 -> 15

;; (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)
(log 2000 3)
;; -> 6.9 -> 7

;; (2 3 5 7 11 13 17)

(define prime-vec (vector 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47))
(define exp-vec (make-vector 15 1))
(define first-range (* 4 (expt 10 6)))
(define secnd-range (- (* first-range 2) 1))

(define prime-vec (vector 2 3 5 7 11 13 17))
(define exp-vec (make-vector 7 1))
(define first-range 1000)
(define secnd-range (- (* 2 first-range) 1))

(define num-of-vec-factor
  (lambda (vec)
    (let ([len (vector-length vec)])
      (do ([i 0 (+ i 1)]
	   [accu 1 (* accu (+ (* 2 (vector-ref vec i)) 1))])
	  ((= len i) accu)))))

(define factor-enough?
  (lambda (vec range)
    (>= (num-of-vec-factor vec) range)))

(define find-solus
  (lambda (start-index
	   end-index
	   current-value
	   current-list
	   upper-bound)
    (cond
     ((> start-index end-index) (list current-list))
     ((> (* (vector-ref prime-vec start-index) current-value) upper-bound) (list current-list))
     (else
      (append (find-solus (+ start-index 1)
			  end-index
			  current-value
			  current-list
			  upper-bound)
	      (find-solus start-index
			  end-index
			  (* current-value (vector-ref prime-vec start-index))
			  (cons (vector-ref prime-vec start-index) current-list)
			  upper-bound))))))

(define find-index
  (lambda (vec n)
    (let f ([i 0])
      (cond
       ((> i (vector-length vec)) #f)
       ((= n (vector-ref vec i)) i)
       (else
	(f (+ i 1)))))))

(define solus->vectors
  (lambda (solus)
    (let f ([so solus])
      (cond
       ((null? so) '())
       (else
	(let ([vec (make-vector (vector-length prime-vec) 0)])
	  (let g ([now-solu (car so)])
	    (cond
	     ((null? now-solu)
	      (cons vec (f (cdr so))))
	     (else
	      (vector-set! vec (find-index prime-vec (car now-solu))
			   (+ (vector-ref vec (find-index prime-vec (car now-solu))) 1))
	      (g (cdr now-solu)))))))))))

(define vectors->solvable
  (lambda (curr-vec vectors rep)
    (let ([now-vec
	   (map (lambda (vec)
		  (let ([v (make-vector (vector-length exp-vec) 0)])
		    (do ([i 0 (+ i 1)])
			((= i rep) v)
		      (vector-set! v i (+ (vector-ref vec i) (vector-ref curr-vec i))))))
		vectors)])
      (let ([new-vec (filter (lambda (v) (factor-enough? v secnd-range)) now-vec)])
	(if (null? new-vec) '()
	    new-vec)))))


(define max-index
  (lambda (ls)
    (let f ([ls (cdr ls)] [cur (cons 0 (car ls))] [id 1])
      (cond
       ((null? ls) (car cur))
       ((< (car ls) (cdr cur))
	(f (cdr ls) (cons id (car ls)) (+ id 1)))
       (else
	(f (cdr ls) cur (+ id 1)))))))

(let pro ([j (- (vector-length prime-vec) 1)] [ep-vc exp-vec])
  (let* ([sols (find-solus 0 (- j 1) 1 '() (vector-ref prime-vec j))]
	 [vecs (solus->vectors sols)]
	 [soas (vectors->solvable ep-vc vecs j)])
    (cond
     ((null? soas) ep-vc)
     (else
      (let ([lss (map (lambda (ss)
			(let f ([i 0] [res 1])
			  (cond
			   ((= i (vector-length ss)) res)
			   (else
			    (f (+ i 1) (* res (expt (vector-ref prime-vec i) (vector-ref ss i))))))))
		      soas)])
	(pro (- j 1) (list-ref soas (max-index lss))))))))

(define last-eval
  (lambda (vec)
    (let ([len (vector-length vec)])
      (let f ([i 0] [res 1])
	(cond
	 ((= i len) res)
	 (else
	  (f (+ i 1)
	     (* res (expt (vector-ref prime-vec i)
			  (vector-ref vec i))))))))))

#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.000122200s elapsed real time
    38448 bytes allocated
#(3 3 2 2 1 1 1 1 1 1 1 1 0 0 0)
|#

;;(last-eval '#(3 3 2 2 1 1 1 1 1 1 1 1 0 0 0)) -> 9350130049860600
