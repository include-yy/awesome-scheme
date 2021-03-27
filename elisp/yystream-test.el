(setq lexical-binding t)

(require 'yystream)

;; test on nullp
(yystream-nullp nil)
(yystream-nullp yystream-null)
(yystream-nullp t)

;; test on car cdr cons
(yystream-car nil)
(yystream-cdr nil)

(setq t1 (yystream-cons 1 (progn (print "Hello") 2)))
(yystream-cdr t1)

;; test on ref
(setq t-ones (yystream-cons 1 t-ones))
(yystream-ref t-ones 10000)

(setq t-natural (yystream-partial-sums t-ones))
(yystream-ref t-natural 10)

;; test on map
(setq pos-integer (yystream-cons 1 (yystream-add t-ones pos-integer)))
(setq t-n+1 (yystream-add pos-integer t-ones))
(yystream-ref t-n+1 10)

(yystream-map (lambda (x) 1) nil)

;; test on filter
(setq x7 (yystream-filter (lambda (x) (zerop (mod x 7))) t-n+1))
(yystream-ref x7 10)
(setq x14 (yystream-filter (lambda (x) (zerop (mod x 2))) x7))
(yystream-ref x14 2)

;; test on foldl
(setq t-tri (yystream-cons 1 (yystream-cons 2 (yystream-cons 3 yystream-null))))
(yystream-foldl (lambda (acc v) (cons v acc)) nil t-tri)
(yystream-foldl (lambda (acc v) (cons v acc)) nil x14 10)

;; test on make-const
(setq t-twos (yystream-make-const 2))
(yystream-ref t-twos 10000)

;; test on scale
(setq t-twent (yystream-scale 10 t-twos))
(yystream-ref t-twent 2000)

;; test on add, sub, mul, div
(yystream-ref (yystream-add t-twos t-twent) 2000)
(yystream-ref (yystream-sub t-twos t-twent) 2001)
(yystream-ref (yystream-mul t-twos t-twent) 2019)
(yystream-ref (yystream-div t-twos t-twent) 2020)
(yystream-ref (yystream-div t-twent t-twos) 2021)


;; test on partial sums :failed
(defun yystream-partial-sums (s)
  "get partial sums of s, get a0, a0 + a1, a0 + a1 + a2 ......"
  (yystream-cons (yystream-car s)
		 (yystream-add (yystream-partial-sums s)
			       (yystream-cdr s))))

(defun yystream-partial-sums (s)
  (yystream-cons (yystream-car s)
		 (yystream-add (yystream-make-const (yystream-car s))
			       (yystream-partial-sums (yystream-cdr s)))))

(setq t-ones (yystream-cons 1 t-ones))
(setq t-natural (yystream-partial-sums t-ones))
(yystream-ref t-natural 740)

;; test on finite-difference
(setq t-delta (yystream-finite-difference t-ones))
(yystream-ref t-delta 0)

;; test on integrate
(reverse (yystream-foldl (lambda (acc v) (cons v acc)) ()
			 (yystream-integrate t-ones) 100))
(defconst yystream-std-one-seq (yystream-cons 1 yystream-std-one-seq))
(defconst yystream-std-natural-seq (yystream-cons
				    1
				    (yystream-add
				     yystream-std-one-seq
				     yystream-std-natural-seq)))
(yystream-ref yystream-std-natural-seq 10)

;; test on derivative
(reverse (yystream-foldl (lambda (acc v) (cons v acc)) ()
			 (yystream-derivative t-ones) 100))


;; test on mul-series
(defun yystream-mul-series (s1 s2)
  "this function gives the taylor series of conv(s1, s2)"
  (yystream-cons (* (yystream-car s1) (yystream-car s2))
		 (yystream-add (yystream-mul-series s1 (yystream-cdr s2))
			       (yystream-scale (yystream-car s2)
					       (yystream-cdr s1)))))
(defconst yystream-std-zero-seq (yystream-cons 0 yystream-std-zero-seq)
  "0, 0, 0, 0, 0 ...")
(defconst yystream-std-delta-seq (yystream-cons 1 yystream-std-zero-seq))
(reverse (yystream-foldl (lambda (acc v) (cons v acc)) ()
			 (yystream-mul-series yystream-std-natural-seq  yystream-std-delta-seq) 10))


;; test on inv-series

