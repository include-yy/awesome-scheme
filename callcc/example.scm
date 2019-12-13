(let* ((yin ((lambda(foo) (newline) foo)
	     (call/cc (lambda(bar) bar))))
       (yang ((lambda(foo) (display #\c) foo)
	      (call/cc (lambda(bar) bar)))))
  (yin yang))

(define product
  (lambda(ls)
    (call/cc
     (lambda(break)
       (let f ([ls ls])
	 (cond
	  [(null? ls) 1]
	  [(= (car ls) 0) (braek 0)]
	  [else (* (car ls) (f (cdr ls)))]))))))

(define x (call/cc (lambda(foo) foo)))
(x 'anything)

(let ([x (call/cc (lambda (k) k))])
  (x (lambda (ignore) "hi")))

(((call/cc (lambda (k) k)) (lambda (x)x)) "hey")

(define retry #f)
(define factorial
  (lambda (x)
    (if (= x 0)
	(call/cc (lambda (k) (set! retry k)1))
	(* x (factorial (- x 1))))))

(define lwp-list '())
(define lwp
  (lambda (thunk)
    (set! lwp-list (append lwp-list (list thunk)))))
(define start
  (lambda ()
    (let ([p (car lwp-list)])
      (set! lwp-list (cdr lwp-list))
      (p))))
(define pause
  (lambda ()
    (call/cc
     (lambda (k)
       (lwp (lambda () (k #f)))
       (start)))))

((lambda (pair)
   (begin
     (display (car pair))
     ((cdr pair)
      (cons (+ 1 (car pair)) 
            (cdr pair)))))
 (call/cc (lambda (k) 
            (cons 0 k))))

((cdr
  (or (call/cc (lambda (cc) (cons 2 (lambda () (cc #f)))))
      (cons 3 (lambda() (+ 3 2))))))

;;((call/cc call/cc) f) equals to (f f)
(define (Y f)
  ((lambda (u)
     (u (lambda (x)
	  (lambda (n) ((f (u x)) n)))))
   (call/cc (call/cc (lambda (x) x)))))

(define (Y f)
  ((lambda (x)
     (lambda (n) ((f (x x)) n)))
   (lambda (x)
     (lambda (n) ((f (x x)) n)))))

(define (Y f)
  ((lambda (f)
     (f f))
   (lambda (k)
     (f (lambda (x) ((k k) x))))))

(define saved #f)
(cons 'foo (call/cc (lambda (k) (set! saved k) '())))
(saved 'bar!)
;;in contrast to
;;In the second case, you call procedure test, so the continuation is both
;;1.cons 'wo to something
;;2.call the procedure bound to saved (i.e. the continuation) with 'two!
;;so it is infinitive
(define (test)
    (define saved #f)
    (cons 'one (call/cc (lambda (k) (set! saved k) '())))
    (saved 'two!))

(define (fact n)
   (let ((r 1) (k 'void))
      (call/cc (lambda (c) (set! k c) 'void))
      (set! r (* r n))
      (set! n (- n 1))
      (if (= n 1) r (k 'recurse))))
