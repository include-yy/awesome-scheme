;;2-29
;;a)
(define make-mobile
  (lambda (left right)
    (list left right)))
(define make-branch
  (lambda (length structure)
    (list length structure)))

(define left-branch
  (lambda (mobile)
    (car mobile)))
(define right-branch
  (lambda (mobile)
    (cadr mobile)))

(define branch-length
  (lambda (branch)
    (car branch)))
(define branch-structure
  (lambda (branch)
    (cadr branch)))
;;b)
(define total-weight
  (lambda (mobile)
    (cond
     ((number? (branch-length mobile))
      (branch-structure mobile))
     (else
      (+ (total-weight (left-branch mobile))
	 (total-weight (right-branch mobile)))))))
;;c)
(define mobile-balance?
  (lambda (mobile)
    (let ([result
	   (let f ([mobi mobile])
	     (cond
	      ((number? (branch-length mobi))
	       (* (branch-length mobi) (branch-structure mobi)))
	      (else
	       (- (f (left-branch mobi)) (f (right-branch mobi))))))])
      (if (= result 0) #t #f))))

;;2-32
(define subsets
  (lambda (s)
    (cond
     ((null? s) '(()))
     (else
      (let ([rest (subsets (cdr s))])
	(append rest (map (lambda (x) (cons (car s) x)) rest)))))))

;;2-34
(define horner-eval
  (lambda (x ls)
    (cond
     ((null? ls) 0)
     (else
      (+ (car ls) (* x (horner-eval x (cdr ls))))))))

(define horner-eval
  (lambda (x ls)
    (fold-right (lambda (a b) (+ a (* x b))) 0 ls)))

;;2-39
(define rev1
  (lambda (ls)
    (fold-right (lambda (x y) (append y (list x))) '() ls)))

(define rev2
  (lambda (ls)
    (fold-left (lambda (x y) (cons y x)) (list (car ls)) (cdr ls))))

(define prime?
  (lambda (n)
    (cond
     ((<= n 0) #f)
     ((= n 1) #f)
     ((= n 2) #t)
     (else
      (let f ([i 3])
	(cond
	 ((> (* i i) n) #t)
	 ((zero? (remainder n i)) #f)
	 (else
	  (f (+ i 2)))))))))

(define enumerate-interval
  (lambda (start end)
    (cond
     ((> start end) '())
     (else
      (cons start (enumerate-interval (+ start 1) end))))))

(define n 5)
(fold-right append '()
	    (map (lambda (i)
		   (map (lambda (j) (list i j))
			(enumerate-interval 1 (- i 1))))
		 (enumerate-interval 1 n)))

(define flatmap
  (lambda (proc seq)
    (fold-right append '() (map proc seq))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))


;;2-40
(define unique-pairs
  (lambda (n)
    (fold-right append '()
		(map (lambda (i)
		       (map (lambda (j)
			      (list i j))
			    (enumerate-interval 1 i)))
		     (enumerate-interval 1 n)))))

;;2-41
(define n 3)
(define 2-41
  (lambda (n)
    (filter (lambda (m) (and (not (or (= (car m) (cadr m)) (= (car m) (caddr m)) (= (cadr m) (caddr m))))
			     (= n (+ (car m) (cadr m) (caddr m)))))
	    (fold-right append '()
			(fold-right append '()
				    (map (lambda (i)
					   (map (lambda (j)
						  (map (lambda (k)
							 (list i j k))
						       (enumerate-interval 1 n)))
						(enumerate-interval 1 n)))
					 (enumerate-interval 1 n)))))))

;;2-42
(define (queen board-size)
  (letrec
      ([queen-cols (lambda (k)
		     (cond
		      ((= k 0)
		       (list empty-board))
		      (else
		       (filter
			(lambda (positions) (safe? k positions))
			(flatmap
			 (lambda (rest-of-queens)
			   (map (lambda (new-row)
				  (adjoin-position new-row k rest-of-queens))
				(enumerate-interval 1 board-size)))
			 (queen-cols (- k 1)))))))])
    (queen-cols board-size)))

(define empty-board
  (let ([new-mat (make-list 8 #f)])
    (do ([i 0 (+ i 1)] [ls new-mat (cdr ls)])
	((= i 8) new-mat)
      (set-car! ls (make-list 8 0)))))

(define board-copy
  (lambda (mat)
    (let ([new (make-list 8 #f)])
      (do ([i 0 (+ i 1)] [ls new (cdr ls)] [old mat (cdr old)])
	  ((= i 8) new)
	(set-car! ls (list-copy (car old)))))))

(define mat-ref
  (lambda (mat i j)
    (let ([now (list-ref mat (- i 1))])
      (list-ref now (- j 1)))))
(define mat-set!
  (lambda (mat i j new)
    (let ([now (list-ref mat (- i 1))])
      (let f ([n now] [cnt 1])
	(cond
	 ((= cnt j) (set-car! n new))
	 (else
	  (f (cdr n) (+ cnt 1))))))))

(define (adjoin-position new-row k rest-of-queens)
  (let ([new-board (board-copy rest-of-queens)])
    (mat-set! new-board new-row k 1)
    new-board))

(define safe? 
  (lambda (k positions)
    (let f ([i 1] [ls '()])
      (cond
       ((> i 8)
	(cond
	 ((= (length ls) 0) #t)
	 ((>= (length ls) 2) #f)
	 (else
	  (let ([now-row (car ls)])
	    (let ([now-ls (list-ref positions (- now-row 1))])
	      (let ([res (fold-left (lambda (x y) (if (= y 1) (+ x 1) x)) 0 now-ls)])
		(cond
		 ((> res 1) #f)
		 (else
		  (letrec ([judge (lambda (i j)
				    (cond
				     ((or (< i 1) (> i 8) (< j 1) (> j 8)) #t)
				     (else
				      (if (= 1 (mat-ref positions i j))
					  #f
					  #t))))])
		    (if (and (judge (- now-row 1) (- k 1))
			     (judge (- now-row 1) (+ k 1))
			     (judge (+ now-row 1) (- k 1))
			     (judge (+ now-row 1) (+ k 1)))
			#t
			#f))))))))))
       ((= (mat-ref positions i k) 1)
	(f (+ i 1) (cons i ls)))
       ((= (mat-ref positions i k) 0)
	(f (+ i 1) ls))))))
(length
(let pro ([ls (queen 8)] [new-ls '()])
  (cond
   ((null? ls) new-ls)
   ((member (car ls) (cdr ls))
    (pro (remove (car ls) ls) (cons (car ls) new-ls)))
   (else
    (pro (cdr ls) (cons (car ls) new-ls)))))
)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))
      
;;2-46
(define make-vect
  (lambda (x y)
    (cons x y)))
(define xcor-vect
  (lambda (vec)
    (car vec)))
(define ycor-vect
  (lambda (vec)
    (cdr vec)))

(define add-vect
  (lambda (vect1 vect2)
    (make-vect
     (+ (xcor-vect vect1)
	(xcor-vect vect2))
     (+ (ycor-vect vect1)
	(ycor-vect vect2)))))
(define sub-vect
  (lambda (vect1 vect2)
    (make-vect
     (- (xcor-vect vect1)
	(xcor-vect vect2))
     (- (ycor-vect vect1)
	(ycor-vect vect2)))))
(define scale-vect
  (lambda (scale vect)
    (make-vect
     (* scale (xcor-vect vect))
     (* scale (ycor-vect vect)))))

;;2-47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
#| not used 
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
|#
(define origin-frame
  (lambda (frame)
    (car frame)))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

(define make-painter
  (lambda (points)
    (lambda (frame)
      (let ([n (frame-coord-map frame)])
      (map
       (lambda (x)
	 (n x)) points)))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ([m (frame-coord-map frame)])
      (let ([new-origin (m origin)])
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))
(define (shrink-to-upper-right painter)
  (transform-painter painter
		     (make-vect 0.5 0.5)
		     (make-vect 1.0 0.5)
		     (make-vect 0.5 1.0)))
(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))
(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

;;2-50
(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 0.0 1.0)))
(define (rot180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)))
(define (rot270 painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

;;2-51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-xia
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point))
	  (paint-shang
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.0 0.5))))
      (lambda (frame)
	(paint-xia frame)
	(paint-shang frame)))))

(define below
  (lambda (painter1 painter2)
    (let ([n-p1 (rot270 painter1)]
	  [n-p2 (rot270 painter2)])
      (rot270 (rot180 (beside n-p1 n-p2))))))

;;2-54
(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))
(define my-equal?
  (lambda (a b)
    (cond
     ((and (atom? a) (atom? b))
      (eq? a b))
     ((or (atom? a) (atom? b)) #f)
     ((and (null? a) (null? b)) #t)
     ((or (null? a) (null? b)) #f)
     (else
      (and (my-equal? (car a) (car b))
	   (my-equal? (cdr a) (cdr b)))))))

(define (deriv exp var)
  (cond
   ((number? exp) 0)
   ((variable? exp)
    (if (same-variable? exp var) 1 0))
   ((sum? exp)
    (make-sum (deriv (addend exp) var)
	      (deriv (augend exp) var)))
   ((product? exp)
    (make-sum
     (make-product (multiplier exp)
		   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
 		   (multiplicand exp))))
   ((exp? exp)
    (make-product
     (make-product (exp-exp exp)
		   (make-exp (exp-base exp) (- (exp-exp exp) 1)))
     (deriv (exp-base exp) var)))
   (else
    (display "error"))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond
   ((=number? a1 0) a2)
   ((=number? a2 0) a1)
   ((and (number? a1) (number? a2)) (+ a1 a2))
   (else
    (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond
   ((or (=number? m1 0) (=number? m2 0)) 0)
   ((=number? m1 1) m2)
   ((=number? m2 1) m1)
   ((and (number? m1) (number? m2) (* m1 m2)))
   (else
    (list '* m1 m2))))

;;2-56
(define make-exp
  (lambda (base exp)
    (list '^ base exp)))
(define exp?
  (lambda (ls)
    (eq? (car ls) '^)))
(define exp-base
  (lambda (ls)
    (cadr ls)))
(define exp-exp
  (lambda (ls)
    (caddr ls)))

;;2-57
;;(define (augend s) (caddr s))
(define (augend s)
  (let ([len (length s)])
    (cond
     ((<= len 2) (display "augend: too less"))
     ((= len 3) (caddr s))
     (else
      (let ([now (cddr s)])
	(cons '+ now))))))
;;(define (multiplicand p) (caddr p))
(define (multiplicand s)
  (let ([len (length s)])
    (cond
     ((<= len 2) (display "multiplicand : too less"))
     ((= len 3) (caddr s))
     (else
      (let ([now (cddr s)])
	(cons '* now))))))

(define template+8
  (lambda (symbol str)
    (lambda (s)
      (let ([len (length s)])
	(cond
	 ((<= len 2) (display s))
	 ((= len 3) (caddr s))
	 (else
	  (cons symbol (cddr s))))))))
(define augend (template+8 '+ "augend : tooles"))
(define multiplicand (template+8 '* "augend : tooles"))

;;2-58
;;;a 
(define (make-sum a1 a2) (list a1 '+ a2))
(define (make-product m1 m2) (list m1 '* m2))

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

;;;b
(define (addend s) (car s))
(define (augend s)
  (let ([rest (cddr s)])
    (cond
     ((null? (cdr rest)) (car rest))
     (else
      rest))))

(define (multiplier p) (car p))
(define (multiplicand s)
  (let ([rest (cddr s)])
    (cond
     ((null? (cdr rest)) (car rest))
     (else rest))))


(define (element-of-set? x set)
  (cond
   ((null? set) #f)
   ((equal? x (car set)) #t)
   (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond
   ((or (null? set1) (null? set2)) '())
   ((element-of-set? (car set1) set2)
    (cons (car set1)
	  (intersection-set (cdr set1) set2)))
   (else (intersection-set (cdr set1) set2))))

;;2-59
(define union-set
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((element-of-set? (car set1) set2)
      (union-set (cdr set1) set2))
     (else
      (cons (car set1) (union-set (cdr set1) set2))))))

;;2-60
(define (element-of-set? x set)
  (cond
   ((null? set) #f)
   ((equal? x (car set)) #t)
   (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (cons x set))
(define (union-set set1 set2)
  (append set1 set2))
(define (intersection-set set1 set2)
  (cond
   ((or (null? set1) (null? set2)) '())
   ((element-of-set? (car set1) set2)
    (cons (car set1)
	  (intersection-set (cdr set1) set2)))
   (else (intersection-set (cdr set1) set2))))


(define (element-of-set? x set)
  (cond
   ((null? set) #f)
   ((> x (car set)) #f)
   ((= x (car set)) #t)
   (else
    (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ([x1 (car set1)]
	    [x2 (car set2)])
	(cond
	 ((= x1 x2)
	  (cons x1 (intersection-set (cdr set1) (cdr set2))))
	 ((< x1 x2)
	  (intersection-set (cdr set1) set2))
	 ((> x1 x2)
	  (intersection-set set1 (cdr set2)))))))

;;2-61
(define adjoin-set
  (lambda (x set)
    (cond
     ((null? set) (cons x '()))
     ((< x (car set)) (cons x set))
     ((= x (car set)) set)
     ((> x (car set)) (cons (car set)(adjoin-set x (cdr set)))))))

;;2-62
(define union-set
  (lambda (set1 set2)
    (cond
     ((and (null? set1) (null? set2)) '())
     ((null? set1) set2)
     ((null? set2) set1)
     ((< (car set1) (car set2))
      (cons (car set1) (union-set (cdr set1) set2)))
     ((= (car set1) (car set2))
      (cons (car set1) (union-set (cdr set1) (cdr set2))))
     ((> (car set1) (car set2))
      (cons (car set2) (union-set set1 (cdr set2)))))))


(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond
   ((null? set) #f)
   ((= x (entry set)) #t)
   ((< x (entry set))
    (element-of-set? x (left-branch set)))
   ((> x (entry set))
    (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond
   ((null? set) (make-tree x '() '()))
   ((= x (entry set)) set)
   ((< x (entry set))
    (make-tree (entry set)
	       (adjoin-set x (left-branch set))
	       (right-branch set)))
   ((> x (entry set))
    (make-tree (entry set)
	       (left-branch set)
	       (adjoin-set x (right-branch set))))))

;;2-63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(define ls '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define ls2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define ls3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
(tree->list-1 ls3)
(tree->list-2 ls3)

;;2-64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ([left-size (quotient (- n 1) 2)])
	(let ([left-result (partial-tree elts left-size)])
	  (let ([left-tree (car left-result)]
		[non-left-elts (cdr left-result)]
		[right-size (- n (+ left-size 1))])
	    (let ([this-entry (car non-left-elts)]
		  [right-result (partial-tree (cdr non-left-elts) right-size)])
	      (let ([right-tree (car right-result)]
		    [remaining-elts (cdr right-result)])
		(cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))


;;2-65
(define union-set
  (lambda (set1 set2)
    (let ([s1 (tree->list set1)]
	  [s2 (tree->list set2)])
      (letrec ((A (lambda (ls1 ls2)
		    ...)))
	(list->tree (A s1 s2))))))
...

;;2-66 too easy ...


(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ([next-branch
	       (choose-branch (car bits) current-branch)])
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond
   ((= bit 0) (left-branch branch))
   ((= bit 1) (right-branch branch))
   (else (display "error"))))

(define (adjoin-set x set)
  (cond
   ((null? set) (list x))
   ((< (weight x) (weight (car set))) (cons x set))
   (else
    (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ([pair (car pairs)])
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

;;2-67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree) => (A D A B B C A)

;;2-68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))
(define (encode-symbol latter tree)
  (cond
   ((leaf? tree) '())
   (else
    (let ([left-tree (left-branch tree)]
	  [right-tree (right-branch tree)])
      (cond
       ((member latter (symbols left-tree))
	(cons 0 (encode-symbol latter left-tree)))
       ((member latter (symbols right-tree))
	(cons 1 (encode-symbol latter right-tree)))
       (else
	(display "error")))))))

;;2-69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define my-pairs '((A 4) (B 2) (C 1) (D 1)))

#| ;;error answer
(define (successive-merge l-pairs)
  (cond
   ((= (length l-pairs) 1) (car l-pairs))
   ((>= (weight (car l-pairs)) (weight (cadr l-pairs)))
    (cond
     ((= (length l-pairs) 2)
      (make-code-tree (cadr l-pairs) (car l-pairs)))
     ((= (length l-pairs) 3)
      (successive-merge (cons (make-code-tree (car l-pairs) (cadr l-pairs)) (cddr l-pairs))))
     (else
      (successive-merge (cons (car l-pairs)
			      (cons (make-code-tree (caddr l-pairs) (cadr l-pairs))
				    (cdddr l-pairs)))))))
   (else
    (successive-merge (cons (make-code-tree (cadr l-pairs) (car l-pairs))
			    (cddr l-pairs))))))
|#

(define (successive-merge l-pairs)
  (cond
   ((= (length l-pairs) 1) (car l-pairs))
   ((> (weight (car l-pairs)) (weight (cadr l-pairs)))
    (successive-merge (adjoin-set (car l-pairs) (cdr l-pairs))))
   (else
    (successive-merge (cons (make-code-tree (car l-pairs) (cadr l-pairs))
			    (cddr l-pairs))))))
;;2-70
(define yaogun '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))
(define huf-yaogun (generate-huffman-tree yaogun))
(define song '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA
		   WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
108, 

;;2-71 nil 
;;2-72 nil

(define (square x) (* x x))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-mag (* (magnitude z1) (magnitude z2))
		     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		     (- (angle z1) (angle z2))))

;;real-imag
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

;;mag-ang
(define (real-part z)
  (* magnitude z) (cos (angle z)))
(define (imag-part z)
  (* magnitude z) (sin (angle z)))
(define (magnitude z) (car z))
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
	(atan y x)))
(define (make-from-mag-ang r a) (cons r a))

;;tagged
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (display "bad tagged datum")))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (display "bad tagged datum")))
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))
;;rectangular 
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
	   (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
	(real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular (cons x y)))

;;polar
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
	      (cons (sqrt (+ (square x) (square y)))
		    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;;de
(define (real-part z)
  (cond
   ((rectangular? z)
    (real-part-rectangular (contents z)))
   ((polar? z)
    (real-part-polar (contents z)))
   (else
    (error "Unknown type -- REAL-PART" z))))
(define (imag-part z)
  (cond
   ((rectangular? z)
    (imag-part-rectangular (contents z)))
   ((polar? z)
    (imag-part-polar (contents z)))
   (else
    (error "Unknown type -- IMAG-PART" z))))
(define (magnitude z)
  (cond
   ((rectangular? z)
    (magnitude-rectangular (contents z)))
   ((polar? z)
    (magnitude-polar (contents z)))
   (else
    (error "Unknown type -- IMAG-PART" z))))
(define (angle z)
  (cond
   ((rectangular? z)
    (angle-rectangular (contents z)))
   ((polar? z)
    (angle-polar (contents z)))
   (else
    (error "Unknown type -- ANGLE" z))))
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;;put and get implemented by myself 
(define func-list '())
(define (put op type proc)
  (set! func-list (cons (list op type proc) func-list)))
(define (get op type)
  (let f ([ls func-list])
    (cond
     ((null? ls) (error "no-proc found -- GET" ls))
     ((and (eq? op (car (car ls))) (equal? type (cadr (car ls))))
      (caddr (car ls)))
     (else
      (f (cdr ls))))))
;;2.4.3
(define (install-rectangular-package)
  ;;internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-iamg x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangluar) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-tag r a))))
  'done)

(define (install-polar-package)
  ;;internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
  ;;interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (display "bad tagged datum")))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (display "bad tagged datum")))
(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
	  (apply proc (map contents args))
	  (error "no method for these types -- APPLY-GENERIC"
		 (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-magm-ang 'polar) r a))

;;2-73
(define (deriv exp var)
  (cond
   ((number? exp) 0)
   ((variable? exp) (if (same-variable? exp var) 1 0))
   ((sum? exp)
    (make-sum (deriv (addend exp) var)
	      (deriv (augend exp) var)))
   ((product? exp)
    (make-sum
     (make-product (multiplier exp)
		   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
		   (multiplicand exp))))
   ;;more rules
   (else (error "unknown expression type -- DERIV" exp))))

;;imp
(define (variable? x)
  (symbol? x))
(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))
(define (deriv exp var)
  (cond
   ((number? exp) 0)
   ((variable? exp) (if (same-variable? exp var) 1 0))
   (else
    ((get 'deriv (operator exp)) (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;;b)c)
(define (install-deriv-package)
  (define (make-sum x y)
    (list '+ x y))
  (define (addend x) (car x))
  (define (augend x) (cadr x))
  (define deriv+
    (lambda (no-exp var)
      (make-sum (deriv (addend no-exp) var)
		(deriv (augend no-exp) var))))
  (define (make-product x y)
    (list '* x y))
  (define (multiplier exp) (car exp))
  (define (multiplicand exp) (cadr exp))
  (define deriv*
    (lambda (no-exp var)
      (make-sum
       (make-product (deriv (multiplier no-exp) var)
		     (multiplicand no-exp))
       (make-product (multiplier no-exp)
		     (deriv (multiplicand no-exp) var)))))
  (define (make-power x y)
    (list '^ x y))
  (define (base x) (car x))
  (define (base-exp x) (cadr x))
  (define deriv^
    (lambda (no-exp var)
      (make-product
       (make-product (base-exp no-exp)
		     (make-power (base no-exp) (- (base-exp no-exp) 1)))
       (deriv (base no-exp) var))))
  (put 'deriv '+ deriv+)
  (put 'deriv '* deriv*)
  (put 'deriv '^ deriv^)
  'done)

;;2-74 nil

;;2-75
(define (make-from-mag-ang r a)
  (define dispatch
    (lambda (op)
      (cond
       ((eq? op 'real-part) (* (cos a) r))
       ((eq? op 'imag-part) (* (sin a) r))
       ((eq? op 'magnitude) r)
       ((eq? op 'angle) a)
       (else
	(error "Unknown -- MAKE-FROM-REAL-IMAG" op)))))
  dispatch)

;;2-76 nil


;;2.5.1
(define 2.5.1-list '())
(define (put op type proc)
  (set! 2.5.1-list (cons (list op type proc) 2.5.1-list)))
(define (get op type)
  (let f ([ls 2.5.1-list])
    (cond
     ((null? ls) (error "no-proc found -- GET" ls))
     ((and (eq? op (car (car ls))) (equal? type (cadr (car ls))))
      (caddr (car ls)))
     (else
      (f (cdr ls))))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (display "bad tagged datum")))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (display "bad tagged datum")))

(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
	  (apply proc (map contents args))
	  (error "no method for these types -- APPLY-GENERIC"
		 (list op type-tags))))))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))

  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;;internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))
  ;;interface to rest of the system
  (define (tag x) (attach-tag 'rational x))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (install-complex-package)
  ;;imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-real-imag 'polar) r a))

  ;;internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))

  ;;interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))

  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-ral-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
