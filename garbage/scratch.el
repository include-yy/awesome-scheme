;;; Name.el --- summary -*- lexical-binding: t -*-
;;; -*- mode: c-mode -*-

;; author:
;; maintainer:
;; version: version
;; package-requires: (dependencies)
;; homepage: homepage
;; keywords: keywords


;; this file is not part of gnu emacs

;; this file is free software; you can redistribute it and/or modify
;; it under the terms of the gnu general public license as published by
;; the free software foundation; either version 3, or (at your option)
;; any later version.

;; this program is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  see the
;; gnu general public license for more details.

;; for a full copy of the gnu general public license
;; see <http://www.gnu.org/licenses/>.


;;; commentary:

;; commentary

;;; code:

(message "hello world!")

(provide 'name)

;;; name.el ends here
f(sequencep '(1 2 3))
t

(length [12 3])
2

(copy-sequence [12 3] )
"123"

[12 3]
[]
[]

[[]]
[[]]

(setq a(copy-sequence "abc"))
"abc"

a
"abc"
(eq a "abc")
nil

(aset a 0 ?b)
98

(standard-syntax-table)

a
"bbc"

"abc"
"abc"
(reverse '(1 2 3))
(3 2 1)


3
(seq-map (lambda (x)(+ x 1)) '(1 2 3))
(2 3 4)
[a b]
[a b]


(symbolp (elt [a b] 0))
t
(seq-mapn (lambda (x y z) (+ x y z))
	  '(1 2 3)
	  [4 5 6]
	  [7 8 9])
(12 15 18)


(1 2 3)



1

1

1

1

1
#^[1 2 3]
[1 2 3]

'#&3"\3"
""

""

""

"\3"
""
(bool-vector 1 2 3)
#&3""

(set 'a (bool-vector 1 1 1 1 1 1 1 1 1 nil))
#&10"\377"

a
#&10"\377"
(length a)
10

(setq a (make-bool-vector 16 1))
#&16"\377\377"

(setq b (make-bool-vector 16 nil))
#&16"  "

(setq c (make-bool-vector 16 nil))
#&16"  "

(bool-vector-exclusive-or a b)
#&16"\377\377"

(aset b 1 1)
1
b
#&16" "
(bool-vector-exclusive-or a b)
#&16"\375\377"
(aset b 0 1)
1

(bool-vector-exclusive-or a b)
#&16"\374\377"
(bool-vector-union a b)
#&16"\377\377"
(bool-vector-set-difference a b)
#&16"\374\377"
(bool-vector-subsetp b a)
t
(bool-vector-count-consecutive a nil 0)
0

(bool-vector-count-consecutive b nil 2)
14
(bool-vector-count-population b)
2

16


0

16


(make-ring 9)
(0 0 . [nil nil nil nil nil nil nil nil nil])

(setq d (make-ring 16))
(0 0 . [nil nil nil nil nil nil nil nil nil nil nil nil ...])

(ring-size d)
16

(ring-length d)
0

(ring-empty-p d)
t

(ring-insert d 1)
1

d
(0 1 . [1 nil nil nil nil nil nil nil nil nil nil nil ...])
(ring-ref d 0)
1

(ring-ref d 16)
1

(ring-insert d 2)
2

d
(0 2 . [1 2 nil nil nil nil nil nil nil nil nil nil ...])
(ring-extend d 10)
0

d
(0 2 . [1 2 nil nil nil nil nil nil nil nil nil nil ...])

(ring-size d)
26

(ring-elements d)
(2 1)
(ring-minus1 d)
d
(0 2 . [1 2 nil nil nil nil nil nil nil nil nil nil ...])
(ring-ref d 3)
1
(ring-ref d 0)
2

(ring-ref d 1)
1

(ring-ref d 2)
2


(cdr d)
(2 . [1 2 nil nil nil nil nil nil nil nil nil nil ...])

(0 2 . [1 2 nil nil nil nil nil nil nil nil nil nil ...])

(ring-index 0 d 16 16)

(ring-previous d 2)
1

(ring-previous d 1)
2

(type-of 1)
integer

(type-of 1.1)
float

(type-of [1])
vector

(type-of '(1 2 3))
cons

(type-of #&3"\377")
bool-vector

#&3"\377"
"\377"

(aref (record 'wocao 1 2 3) 0)
wocao


#s(wocao 1 2 3)
#s(wocao 1 2 3)

(setq a #s(hash-table size 30 data (1 1 2 2)))
#s(hash-table size 30 test eql rehash-size 1.5 rehash-threshold 0.8125 data (1 1 2 2 ...))

#s(hash-table size 30 test eql rehash-size 1.5 rehash-threshold 0.8125 data (1 1 2 2 ...))


a
#s(hash-table size 30 test eql rehash-size 1.5 rehash-threshold 0.8125 data (1 1 2 2 ...))

(gethash 1 a)
1
focus-in-hook
nil

(puthash 2 3 a)
3

a
#s(hash-table size 30 test eql rehash-size 1.5 rehash-threshold 0.8125 data (1 1 2 3 ...))

(remhash 1 a)
nil

a
#s(hash-table size 30 test eql rehash-size 1.5 rehash-threshold 0.8125 data ( 2 3 ...))
(maphash (lambda (key value) key) a)
nil
a
#s(hash-table size 30 test eql rehash-size 1.5 rehash-threshold 0.8125 data ( 2 3 ...))

(hash-table-keys a)
(2)

(hash-table-size a)
30

(hash-table-count a)
1
(buffer-file-name)
"c:/users/yiyue/desktop/scratch.el"

buffer-file-name
"c:/users/yiyue/desktop/scratch.el"

(message "%s" obarray)

(length obarray)
15121

(type-of obarray)
vector

(setq e (gensym))
g219

(eq e 'g219)
nil

(get)

(defun fibonacci(n)
  (if (<= n 1)
      n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(let ((time (current-time)))
  (fibonacci 40)
  (message "%.06f" (float-time (time-since time))))

(read "1")
1

(subrp (symbol-function 'car))
t

(setq a 'car)
car

a
car

(funcall a '(1 2))
1

(fset 'a 'car)
car

a
car

#'a
a

(a '(1 2))
1

(indirect-function a)
#<subr car>




#<subr car>

max-lisp-eval-depth
800

values
nil

nil

(+ 1 2)
3

(list auto-save-default)
(nil)
values
nil

nil
9
(setq g (thunk-delay))


lexical-binding
(setq lexical-binding t)
t

nil

(thunk-delay (+ 1 2))
(lambda (x) (* x x))
(lambda (x) (* x x))
(lambda (x) (* x x))

(require 'thunk)
thunk

(closure (t) (x) (* x x))

thunk-

(thunk-force (thunk-delay (+ 1 1)))
2

(closure ((val closure (t) nil (+ 1 1)) (forced) t) (&optional check) (if check forced (if forced nil (setq val ...) (setq forced t)) val))

(pcase 1

  ((guard 1) 27)
  ((app (lambda (x) (+ x 1)) (pred (= 2))) 26)
  ((pred (= 1)) 25)
  ((pred numberp) 24)
  (1 23)
  (- 123))
27

26




25

24

24


24
(= 42)
t

23

123
(setq x t)
t

nil

(defun foo-outer ()
  (catch 'foo
    (foo-inner)))

(defun foo-inner ()
  (if x
      (throw 'foo 123)))
foo-inner


(foo-outer)
123

t

nil

(setq f 0.5
      l 0.25
      w 0.06
      t 0.003
      e 70e5
      k 2.1
      )
2.1

(setq m (* f l 0.5))
0.0625

(setq iz (/ (* w t t t) 12.0))
1.3499999999999997e-10

(setq epl (/ (* 0.5 t m) e iz))
(* 0.09920634920634923 k 120)
25.000000000000007



(pcase 1
  ((let 1  1) 1))
1
(<= 1 2 3)
t

1


1


nil

1

2

2
(pcase 1
  ((app (lambda (x) (+ x 1)) v) v))
2
(require 'generator)

(iter-defun a (x)
  (let ((i x))
    (while (< i 10)
      (setq i (+ i 1))
      (iter-yield i))))
a



a
(setq b (a 1))

(iter-next b)


10

9

8

7

6

5

4

3
p
(setq c 0)
(macroexpand '(iter-do (b (a 1)) (setq c (+ c b))))
(let (b iter-do-result236 (iter-do-iterator-done233 nil) (iter-do-iterator235 (a 1))) (while (not iter-do-iterator-done233) (condition-case iter-do-condition234 (setf b (iter-next iter-do-iterator235)) (iter-end-of-sequence (setf iter-do-result236 (cdr iter-do-condition234)) (setf iter-do-iterator-done233 t))) (unless iter-do-iterator-done233 (setq c (+ c b)))) iter-do-result236)

nil
c
54

(print 1)

1
1

(error "that is an error -- try something else")
(define-error 'abc "wocao")
"wocao"


(signal 'abc '(x y))

(cl-case 1
  ((1 2 3 4 5) 2))
2


nil

(pcase '(1 2 3)
  (`(,(pred consp) . ,y ) (list y)))

(pcase-let* ((`(,x . ,y) '(1 2 3))
	     (`(,x ,y) y))
  (list x y))
(2 3)

nil

((2 3))

(1 (2 3))

(1 2)

nil

nil

nil

debug-on-signa

max-specpdl-size
1983

(setq x 1)
1

(defun qu (x)
  (setq x 2))

(defun qq (y)
  (let ((x 1))
    (setq x y)))
qq

x
1

(qq 2)
2

x
1
x
1

(makunbound 'x)
x

x
(boundp 'x)
nil

(let ()
  (defun xc ()
    1))
xc

(xc)
1

(let ()
  (defvar x)
  (setq x 1)
  )
1

x

x
1

(setq lexical-binding nil)
nil

t


(defvar y 2)

2

y
2

1
x
1

y
2
(setq lexical-binding t)
t

(let ((y 2))
  (defvar y 1)
  y)
2

y
2
(setq p 1)
1

(setq q 2)
2

(make-variable-buffer-local 'q)
q

q
2

(setq q 1)
1
(setq-default q 3)
3
q
1
(local-variable-p 'change-major-mode-hook)
t

(default-value 'a)
1

(make-local-variable 'c)
c

(default-boundp 'c)
nil

(default-value 'a)
1

file-local-variables-alist
nil
dir-locals-file
".dir-locals.el"

(func-arity 'and)
(0 . unevalled)
(subr-arity 'car)
(subrp 'car)
nil

(1 . 1)

(0 . many)



(make-variable-buffer-local )
(setq lexical-binding t)
t

(funcall (lambda (x) (x))
	 (lambda () 1))

((lambda (x) ((function x)))
 (lambda () 1))


(defun wwww (x y z)
  "hello every body
i'm include-yy

\(wwww me you he)"
  (+ x  y z))
wwww

wwww
(wwww )
(boundp 'car)
nil

(symbol-function 'f)
nil


(macroexpand '(defun aaa (x0) (+ x0 1)))
(defalias 'aaa #'(lambda (x0) (+ x0 1)))
aaa


(identity 'a)
a

(setq lexical-binding nil)
nil

(aaa 1)
2
a
1
b

(funcall (symbol-value 'b) (lambda () 1))
1






(defun a (x) (funcall x))
a

a
1

(setq b (function a))
a
b
a
(b )
1

b
1
a
1

a
1

((lambda (x) (funcall x x)) (lambda (x) (funcall x x)))


(defmacro inc (var)
  (list 'setq var (list '1+ var)))
inc
(setq a 0)
0


(macroexpand '(inc a))
(setq a (1+ a))
(gensym)
g2039
(make-symbol "123")


1
a
1
(gensym)
g12

g11

'g11
g11

(seq-take)

(setq x (lambda(x) 1))

x
(lambda (x) 1)

((lambda (x) (funcall x x)) x)
1



(closure (y t) (x) (x x) x)
funcall
((lambda (x) (identity x)) (lambda (x) x))
(closure (y t) (x) x)

(closure (y t) (x) (identity x))

(cl-defgeneric theone (ll)
  "get the first value"
  (car ll))
theone

(theone '(1))
1

(cl-defmethod theone ((ll array))
  (aref ll 1))
theone

(theone [1 2 3])
2

(cl-defmethod theone ((ll array))
  (eq (aref array ll 2) 3)
  (aref ll 0))
theone

theone

(theone [1 2 3])


(require 'seq)
seq

(defun si (x) (sin x))
si

(defun si-1 (x) (asin x))
si-1

(advice-add 'si :filter-return 'si-1)
nil

(si 1)
1.0

(si 2)
1.1415926535897933

(si 1.5)
1.5000000000000002

(si (+ float-pi 1))
-0.9999999999999998



(symbolp :1)
t


-1
-1

+1
1
#'1+
1+

'((1 . 2) . (3 . 4))
((1 . 2) 3 . 4)

(defun my-double (x) (* x 2))
my-double

my-double


my-double
(defun my-increase (x) (+ x 1))
my-increase

my-increase

my-increase

(advice-add 'my-double :filter-return 'my-increase)
nil

(my-double 1)
2

3

(advice-remove 'my-double 'my-increase )
nil

(process-filter )

(defun his-tracing-function (orig-fun &rest args)
  (message "display-buffer called with args %s" args)
  (let ((res (apply orig-fun args)))
    (message "display-buffer returned %s" res)
    res))
his-tracing-function

his-tracing-function

(advice-add 'my-double :around 'his-tracing-function)
nil

nil

nil
(my-double 2)
4


(my-double 1)
2

2

3

3

2

2


(add-function :after (symbol-function 'my-double) 'my-increase)
#[128 "\300\302\"\300\301\"\210\207" [apply my-increase #[128 "\300\301\302#\207" [apply his-tracing-function ... nil] 5 nil] nil] 5 nil]


(advice-add 'my-double :after 'my-increase)
nil
(advice-function-member-p 'his-tracing-function 'my-double)
nil

nil

nil

nil

nil



(my-increase 1)
2

2
(my-double 2)
4
(advice-eval-interactive-spec "r\np")
(9517 9555 nil)

(advice-member-p 'his-tracing-function 'my-double)
#[128 "\300\301\302#\207" [apply his-tracing-function #[128 "\300\302\"\300\301\"\210\207" [apply my-increase (closure (y t) (x) (* x 2)) nil] 5 nil] nil] 5 nil]

(defmacro define (a plist body)
  `(defun ,a ,plist ,body))
define

define

(define a (x) (+ x 1))
a

(a 1)
2

(define add2 (x) (+ x 2))
add2

(add-function :filter-return (symbol-function 'add2) (lambda(x) (+ x 1)))
#[128 "\301\300\302\"!\207" [apply (closure (y t) (x) (+ x 1)) (closure (y t) (x) (+ x 2)) nil] 5 nil]

#[128 "\300\302\"\300\301\"\210\207" [apply (closure (y t) (x) (+ x 1)) (closure (y t) (x) (+ x 2)) nil] 5 nil]

(add2 1)
3

(advice-add 'add2 :after (lambda (x) (+ x 1)))
nil

(add2 2)
5

4

3

(define-advice add2 (:filter-return (lambda (x) (+ x 2))))
nil

(define add3 (x) (+ x 3))
add3

(advice-add 'add3 :around (lambda (x y) (funcall x y)))
nil

nil
(add3 2)

(define add4 (x) (+ x 4))
add4
(apply '+ 1 23  '())
24
(1+ 2)
3

(advice-add 'add5 :around (lambda (org &rest args)
			    (print (apply '1+ args))
			    (apply org args)
			    (print (apply org args))
			    555))
nil

nil
(define add5 (x) (+ x 5))
add5

nil

(add4 10)

11

11

11

555
555


11
555
(add5 1)

2

6
555

(defun add6 (x) (+ x 6))
add6


debug-on-error
t

debug-ignored-errors
("^exit the snippet first!$" "^invalid `winum-scope': .$" "^got a dead window .$" "^no window numbered .$" beginning-of-line beginning-of-buffer end-of-line end-of-buffer end-of-file buffer-read-only file-supersession mark-inactive user-error)

eval-expression-debug-on-error
t


debug-on-signal
nil

(debug)

(defun factor (x)
  (if (= x 0) 1
    (* x (factor (- x 1)))))
factor

factor

(factor 100)
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000

(debug-on-entry 'factor)
factor

(+ 1 (factor 10))

3628801

(factor 10)

(cancel-debug-on-entry 'factor)
factor

(factor 10)
3628800

(setq a 1)
1

1

(debug-on-variable-change 'a)
nil



nil
a

(setq a 2)
2
(cancel-debug-on-variable-change 'a)
nil

(progn
  (print 1)
  (debug)
  (print 2))

1

2
2


1

2
2

(debug-on-entry 'factor)
factor


(factor 10)
3628800

3628800

(defun factor-2 (x)
  (let ((res 1))
    (while (> x 0)
      (setq res (* res x))
      (setq x (- x 1)))
    res))
factor-2

factor-2

(factor-2 3)
6

nil
(cancel-debug-on-entry 'factor)
factor

(debug-on-entry 'factor-2)
factor-2

(factor-2 10)
3ommand
3628800

(debug-on-entry 'factor)
factor

(factor 5)





10



3628800

3628800

(debug 'hello)


(debug)


(/ 1 0)

(with-output-to-temp-buffer "backtrace-output"
  (let ((var 1))
    (save-excursion
      (setq var (eval '(progn
			 (1+ var)
			 (list 'testing (backtrace))))))))


(defun fac (n)
  (if (< 0 n)
      (* n (fac (1- n)))
    1))

fac

fac

fac

(fac 10)
3628800


(fac 5)
120


120

120

120


120

120

120

120

120

120

120




21312312
120

120


120

edebug-all-defs


(fac 1)
1


edebug-sit-for-seconds
1

(progn
  (setq a (list 'x 'y))
  (setcar a a))

'#0=(#0# y)
(#0 y)

print-circle
nil

edebug-print-circle
t

edebug-trace
nil


(setq edebug-trace nil)
nil

t

(fac 5)
120

(edebug-tracing "abc" (fac 5))
120

edebug-test-coverage
nil

(defun k1 (x) (k2 x))
(defun k2 (x) (k3 x))
(defun k3 (x) (+ x 1))

(k1 1)

(fac 5)
(setq edebug-test-coverage t)
t

(defun fac1 (x)
  (cond
   ((= x 1) 1)
   (t
    (* x (fac1 (- x 1))))))
fac1

fac1

(fac1 5)


(fac1 10)
3628800

(fac1 100)
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000

93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000

(defun fibonacci(n)
  (if (<= n 1)
      n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
fibonacci

(fibonacci 30)
832040

832040

6765


(elp-instrument-function 'fibonacci)
nil
elp-function-list
nil

(fibonacci 30)
832040

(setq elp-function-list '(fibonacci))
(fibonacci)
elp-function-list
(fibonacci)

(fibonacci 30)

(elp-results)


(benchmark-run 10000 (car '(1 2 3)))
(0.0015109999999999993 0 0.0)

(1.4000000000000001e-05 0 0.0)

(4e-06 0 0.0)

features
(autoload benchmark elp profiler timeclock time-stamp misearch multi-isearch edebug pp cl-print eieio-opt ...)


(symbol-name 'a)
"a"

(symbol-file 'find-file)
"c:/users/yiyue/myself/software/emacs/share/emacs/27.1/lisp/files.elc"

(symbol-plist 'find-file)
(group-documentation "finding files." custom-group ((directory-abbrev-alist custom-variable) (find-file-existing-other-name custom-variable) (find-file-visit-truename custom-variable) (revert-without-query custom-variable) (find-file-run-dired custom-variable) (find-directory-functions custom-variable) (file-name-at-point-functions custom-variable) (find-file-hook custom-variable) (enable-local-variables custom-variable) (enable-local-eval custom-variable) (confirm-nonexistent-file-or-buffer custom-variable) (large-file-warning-threshold custom-variable) ...) event-symbol-element-mask (find-file 0) event-symbol-elements (find-file) modifier-cache ((0 . find-file)))

(read "1")
1

(read "(1 2 3)")
(1 2 3)

(read t)
123

(message "%s" (read t))
"1"

(read nil)
123

(read a)

a
(#0 y)

(setq m (set-marker (make-marker) 1 (get-buffer "scratch.el")))
#<marker at 1 in scratch.el>

(read m
      )
(copy-sequence [12 3])

2

(length [12 3])

t

(sequencep '(1 2 3))

m
#<marker at 67 in scratch.el>

standard-input


(setq standard-input "123")
"123"
(setq standard-input t)
t

(read)
123



123
(read)
123


(print "123" m)
"123"

(print 1 (get-buffer "scratch.el"))
1
(setq foo (list 1))
(1)

(setf (car foo) foo)
(#0)

foo
(#0)
(print foo)

(#0)
(#0)

1
1

(write-char ?a)
a97
(prin1 123)
123123
(print 1)
(prin1 "1\n2")
"1
2""1
2"

"1\n2""1\n2"
(setq print-escape-newlines nil)
nil

t


"1\n2"
"1\n2"

1
1

(setq print-escape-newlines t)
t

nil

nil

print-length
nil
(print '(1 2 3))

(1 2 3)
(1 2 3)
print-level
nil
print-circle
nil

'#0=(#0#)
(#0)

float-output-format
nil

print-number-table
nil

enable-recursive-minibuffers
nil


minibuffer-inactive-mode-map
(keymap #^[nil nil keymap
	       #^^[3 0 nil nil nil nil nil nil nil nil nil nil nil ...] #^^[1 0 #^^[2 0
										      #^^[3 0 nil nil nil nil nil nil nil nil nil nil nil ...] nil nil nil nil nil nil nil nil nil nil ...] nil nil nil nil nil nil nil nil nil nil ...] nil nil nil nil nil nil nil ...] (down-mouse-1 . ignore) (mouse-1 . view-echo-area-messages) (remap keymap (self-insert-command . undefined)))

(read-from-minibuffer "hello:-> " "asdf" nil nil '(a b c))
(read-from-minibuffer
 (concat
  (propertize "bold" 'face '(bold default))
  (propertize " and normal: " 'face '(default))))
"1231231213123"

(propertize "abc" 'face '(bold default))
:#("abc" 0 3 (face (bold default)))



""

"asdf"

(+ 1 2)

asdf

(read-regexp "hello")
"123"

(read-regexp "(a|b)")
"(a)|(b)"


(eval-minibuffer "w")
3

(read-passwd "qq")
"12321312312312"

(execute-kbd-macro )
c-x onil

onil

(defun yy-1 (x) (interactive "p") (princ x))
yy-1

yy-1

(defun yy-2 (x y) (interactive "p\nbfib: ") (+ x y))
yy-2

(interactive-form 'forward-char)
(interactive "^p")


(defun yy-3 (x) (interactive "c") (print x))
yy-3


a

emacs-version
"27.1"

(set-frame-font " 14")


font-width-table
[[50 ultra-condensed ultracondensed] [63 extra-condensed extracondensed] [75 condensed compressed narrow] [87 semi-condensed semicondensed demicondensed] [100 normal medium regular unspecified] [113 semi-expanded semiexpanded demiexpanded] [125 expanded] [150 extra-expanded extraexpanded] [200 ultra-expanded ultraexpanded wide]]

(face-remap-add-relative 'default '((:height 165)))
(default :height 165)

(default :height 167)

(default :height 150)

(default :height 200)

(default :height 100)

(default :height 20)


customize-package-emacs-version-alist
nil

(defun yy-1 nil "yy's first custom")

require-final-newline
t


(symbol-name 'a)
"a"

?\s
32

?\a
7

?\8
56

?\7
7

?\9
57

?\a
7

(eval-defun)
(type-of t)
symbol


(defgroup yy-1 nil "yy's first custom")
yy-1

yy-1

(defcustom yq t "t or nil"
  :type 'boolean)
yq
nil
(type-of '(1))
cons

integer

symbol

(defcustom yp 1 "nuimber"
  :type 'integer
  :options '(1 2 3 4 5))
yp

yp
6

(defcustom ys 1 "func"
  :set (lambda (s y) (set s (+ y 1))))
ys
4

(defcustom yg 0 "gfunc")
yg
(defvar a 1)
a

(default-value 'a)
1

(defgroup qwer nil "www")
qwer

(defcustom ty '((1 . 2)) "hello"
  :type '(alist :key-type integer :value-type integer)
  :options '((1 . 2) (3. 4)))
ty

aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

((lambda (x) (progn (print (symbol-value x))
		    (print (symbol-function x))))
 (lambda (x) 1))

(type-of (lambda (x) 1))

(defun x (x) x)

(type-of 'x)

(type-of t)

((symbol-function 'y) 1 2)

(type-of (symbol-function 'y))

(defun y (x y) (+ x y))

((lambda (x) x) 1)

values

(setq x 1)


((lambda (x) (x x))
 (lambda (x) (x x)))
mapc


mapcar

(map-pairs )

(map-apply)


(funca)

(functionp #'car)

(consp 'car)


((lambda (x) (funcall x x)) (lambda (x) (funcall x)))

(defun a (x) (+ x 1))

(funcall a 1)

(defun a (x) (+ x 1))
(setq a (lambda (x) (+ x 2)))

(funcall a 1)

(funcall 'a 1)

(defun a (x) (+ x 1))
(setq a (lambda (x) (+ x 2)))

(apply a 1 nil)
(apply 'a 1 nil)

(functionp 'defun)

(functionp 'ff)

(special-form-p 'let)

((car (list (symbol-function '+))) 1 2)

(functionp (lambda (x) x))
(functionp (car (list '+)))


https://www.pinterest.com/pin/442337994649421174/

(eq (make-symbol "abc") (make-symbol "abc"))

lexical-binding

(require 'thunk)

lexical-binding

(setq lexical-binding t)

(print (macroexpand-1 '(thunk-delay 1)))

(let (forced
      (val (lambda nil 1)))
  (lambda (&optional check)
    (if check
	forced
      (unless forced
	(setf val (funcall val))
	(setf forced t))
      val)))
(thunk-force)

(macroexpand-1
 '(thunk-let
   ((a (+ 1 2)))
   a
   )
 )
(macroexpand
 '(let ((a-thunk (thunk-delay (+ 1 2)))) (cl-symbol-macrolet ((a (thunk-force a-thunk))) a))
 )
(let ((a-thunk (thunk-delay (+ 1 2)))) (cl-symbol-macrolet ((a (thunk-force a-thunk))) a))

(let ((a-thunk (thunk-delay (+ 1 2)))) (cl-symbol-macrolet ((a (thunk-force a-thunk))) a))

#x16
22
(setq a
      '(#b01001111 #b01001101 #b01000111 #b01001000 #b01001111 #b01001100 #b01011001 #b01010011 #b01001000 #b01001001 #b01010100))
(79 77 71 72 79 76 89 83 72 73 84)

a
(79 77 71 72 79 76 89 83 72 73 84)


(79 77 71 72 79 76 89 83 72 73 84)


(princ 65)
6565






79

77

71

72

79

76

89

83

72

73

84
(79 77 71 72 79 76 89 83 72 73 84)


79

77

71

72

79

76

89

83

72

73

84
(79 77 71 72 79 76 89 83 72 73 84)


(prin)
79
?\79

?\a
7
?a
97



77

71

72

79

76

89

83

72

73

84
(79 77 71 72 79 76 89 83 72 73 84)

(expt 2 4)

(if nil 1
  (print 1)
  (print 2)
  (print 3))

(cond)

(macroexpand '(and)
	     )
(while (progn
         (forward-line 1)
         (not (looking-at "^$"))))
(xor nil nil)
(macroexpand
 '(dolist (a '(1 2 3) d)
    (print a)
    (setq d a))
 )
(let
    (let ((--dolist-tail-- '(1 2 3))) (while --dolist-tail-- (let ((a (car --dolist-tail--))) (print a) (setq d a) (setq --dolist-tail-- (cdr --dolist-tail--))))(let ((--dolist-tail-- '...)) (while --dolist-tail-- (let (...) (print a) (setq d a) (setq --dolist-tail-- ...))) d)))


(-map (lambda (x) (* x x)) '(1 2 3))
(1 4 9)

()

(iter-defun a (x)
  (while (< x 10)
    (iter-yield x)
    (cl-incf x))
  (iter-yield 10))
a


a
(iter-next b)

(setq b (a 0))
(iter-next b 6)

(iter-defun c ()
  (let ((x 0))
    (while (< x 5)
      (print (iter-yield x))
      (cl-incf x))
    (iter-yield 5)))
c

c

c

(setq d (c))
(iter-next d 4)


4
5


4
4


4
3


5
2


5
1

0
(iter-defun e ()
  (iter-yield-from (c)))
e

(iter-yield-from)

(setq f (e))
(closure ((cps-state-atom-352 closure #1 nil (setq cps-current-value-313 ...)) (cps-state-let*-351 closure #1 nil (progn ... ...)) (cps-state-atom-350 closure #1 nil (setq cps-current-value-313 ...)) (cps-state-let*-349 closure #1 nil (progn ... ...)) (cps-state-atom-348 closure #1 nil (setq cps-current-value-313 ...)) (cps-state-let*-346 closure #1 nil (progn ... ...)) (cps-state-atom-345 closure #1 nil (setq cps-current-value-313 ...)) (cps-state-let*-343 closure #1 nil (progn ... ...)) (cps-state-while-331 closure #1 nil (progn ...)) (cps-state-atom-342 closure #1 nil (setq cps-current-value-313 ...)) (cps-state-iter-yield-340 closure #1 nil (progn ... ...)) (cps-state-after-yield-339 closure #1 nil (setq cps-current-state-314 cps-state-let*-338)) ...) (op value) (cond ((eq op :stash-finalizer) (setq cps-iterator-finalizer-353 value)) ((eq op :get-finalizer) cps-iterator-finalizer-353) ((eq op :close) (let ... ... ...)) ((eq op :next) (setq cps-current-value-313 value) (let ... ...)) (t (error "unknown iterator operation %s" op))))

(iter-next f)


nil
5


nil
4


nil
3


nil
2


nil
1

0

(iter-defun g ()
  (iter-yield-from (e))
  (iter-yield "hello"))
g

(setq h (g))
(closure ((cps-state-atom-426 closure #1 nil (setq cps-current-value-385 ...)) (cps-state-let*-425 closure #1 nil (progn ... ...)) (cps-state-atom-424 closure #1 nil (setq cps-current-value-385 ...)) (cps-state-let*-423 closure #1 nil (progn ... ...)) (cps-state-atom-422 closure #1 nil (setq cps-current-value-385 ...)) (cps-state-let*-420 closure #1 nil (progn ... ...)) (cps-state-atom-419 closure #1 nil (setq cps-current-value-385 ...)) (cps-state-let*-417 closure #1 nil (progn ... ...)) (cps-state-while-405 closure #1 nil (progn ...)) (cps-state-atom-416 closure #1 nil (setq cps-current-value-385 ...)) (cps-state-iter-yield-414 closure #1 nil (progn ... ...)) (cps-state-after-yield-413 closure #1 nil (setq cps-current-state-386 cps-state-let*-412)) ...) (op value) (cond ((eq op :stash-finalizer) (setq cps-iterator-finalizer-427 value)) ((eq op :get-finalizer) cps-iterator-finalizer-427) ((eq op :close) (let ... ... ...)) ((eq op :next) (setq cps-current-value-385 value) (let ... ...)) (t (error "unknown iterator operation %s" op))))

(iter-next h)

"hello"


nil
5


nil
4


nil
3


nil
2


nil
1

0

(iter-defun)

(pcase 1
  (a a))

(require 'thunk)
(setq lexical-binding t)
(fset 'a
      (let ((fevaled nil)
	    (thevalue nil))
	(lambda ()
	  (if fevaled thevalue
	    (progn
	      (print "hhh")
	      (setq thevalue (+ 1 2))
	      (setq fevaled t)
	      thevalue)))))

a
a
(closure ((thevalue) (fevaled) y yq yp ys yg a ty t) nil (if fevaled thevalue (progn (print "hhh") (setq thevalue ...) (setq fevaled t) thevalue)))

(a)
3


"hhh"
3

(null nil)
t
(require 'thunk)
(setq lexical-binding t)

;; stream-car
(defun stream-car (s) (car s))
;; stream-cdr
(defun stream-cdr (s) (thunk-force (cdr s)))
;; stream-cons
(defmacro stream-cons (a b) `(cons ,a (thunk-delay ,b)))
;; stream-nullp
(defun stream-nullp (s) (null s))
;; stream-null
(defvar stream-null nil)

(setq ones (stream-cons 1 ones))
(1 closure ((val closure (y yq yp ys yg a ty t) nil ones) (forced) y yq yp ys yg a ty t) (&optional check) (if check forced (if forced nil (setq val ...) (setq forced t)) val))

(stream-car ones)
1

(stream-car (stream-cdr ones))
1

(defun stream-ref (s n)
  (if (< n 0) (error "negative index" n)
    (cl-loop for i from 0 to (- n 1)
	     if (stream-nullp s)
	     return stream-null
	     else
	     do (setq s (stream-cdr s))
	     finally return (stream-car s))))
stream-ref

stream-ref

(stream-ref ones 10)
1
nil

3

(defun stream-map (func &rest ss)
  (if (stream-nullp (car ss)) stream-null
    (stream-cons (apply func (mapcar 'stream-car ss))
		 (apply 'stream-map func (mapcar 'stream-cdr ss)))))



(defun stream-add (sa sb)
  (stream-map '+ sa sb))
stream-add

(setq twos (stream-add ones ones))



(stream-ref twos 10)
2
(setq pos-integer (stream-cons 1 (stream-add ones pos-integer)))


(stream-ref pos-integer 10000)

;;------------------------------------------------------------------------------
(require 'thunk)
(setq lexical-binding t)

;; stream-car
(defun stream-car (s) (car s))
;; stream-cdr
(defun stream-cdr (s) (thunk-force (cdr s)))
;; stream-cons
(defmacro stream-cons (a b) `(cons ,a (thunk-delay ,b)))
;; stream-nullp
(defun stream-nullp (s) (null s))
;; stream-null
(defvar stream-null nil)

(setq ones (stream-cons 1 ones))

(defun stream-ref (s n)
  (if (< n 0) (error "negative index" n)
    (cl-loop for i from 0 to (- n 1)
	     if (stream-nullp s)
	     return stream-null
	     else
	     do (setq s (stream-cdr s))
	     finally return (stream-car s))))

(defun stream-map (func &rest ss)
  (if (stream-nullp (car ss)) stream-null
    (stream-cons (apply func (mapcar 'stream-car ss))
		 (apply 'stream-map func (mapcar 'stream-cdr ss)))))


(defun stream-add (sa sb)
  (stream-map '+ sa sb))

(setq pos-integer (stream-cons 1 (stream-add ones pos-integer)))

(stream-ref pos-integer 25)

(defun fibgen (a b)
  (stream-cons a (fibgen b (+ a b))))

(setq fib (fibgen 1 1))

(stream-ref fib 5)


(setq fib2 (stream-cons 0
			(stream-cons 1
				     (stream-add (stream-cdr fib2)
						 fib2))))

(stream-ref fib2 5)

(defun stream-scale (s n)
  (stream-map (lambda (x) (* x n)) s))

(setq fives (stream-scale ones 5))

(stream-ref fives 10)

(defun stream-filter (func s)
  (if (stream-nullp s) stream-null
    (if (funcall func (stream-car s))
	(stream-cons (stream-car s) (stream-filter func (stream-cdr s)))
      (stream-filter func (stream-cdr s)))))

(setq x5 (stream-filter (lambda (x) (zerop (mod x 5))) pos-integer))

(stream-ref x5 10)

(setq start100 (stream-filter (lambda (x) (> x 100)) pos-integer))

(stream-ref start100 0)

(setq double (stream-cons 1 (stream-scale double 2)))

(stream-ref double 10)

(setq three (stream-cons 1 (stream-scale three 3)))

(stream-ref three 3)

(defun stream-mul (s1 s2)
  (stream-cons (* (stream-car s1) (stream-car s2))
	       (stream-mul (stream-cdr s1) (stream-cdr s2))))

(defun stream-mul (s1 s2)
  (stream-map '* s1 s2))

(setq factor (stream-cons 1 (stream-mul factor pos-integer)))

(stream-ref factor 3)

(defun stream-const (n)
  (stream-cons n (stream-const n)))

(defun stream-partial-sums (s)
  (stream-cons (stream-car s) (stream-add (stream-const (stream-car s))
					  (stream-partial-sums (stream-cdr s)))))

(setq pos-int (stream-partial-sums pos-integer))

(stream-ref pos-int 30)

(defun stream-integrate (s)
  (stream-mul (stream-map (lambda (x) (/ 1.0 x)) pos-integer)
	      s))

(setq one2 (stream-integrate pos-integer))

(stream-ref one2 200)

(setq exp-series
      (stream-cons 1 (stream-integrate exp-series)))

(stream-ref exp-series 500)

(setq cosine-series
      (stream-cons 1
		   (stream-scale
		    (stream-integrate sine-series)
		    -1)))
(setq sine-series
      (stream-cons 0
		   (stream-integrate cosine-series)))

(stream-ref cosine-series 4)

(stream-ref sine-series 3030)

(defun stream-mul-series (s1 s2)
  (stream-cons (* (stream-car s1) (stream-car s2))
	       (stream-add (stream-mul-series s1 (stream-cdr s2))
			   (stream-scale (stream-cdr s1)
					 (stream-car s2)))))

(setq iii (stream-add (stream-mul-series sine-series sine-series)
		      (stream-mul-series cosine-series cosine-series)))

(setq zeros (stream-cons 0 zeros))

(defun stream-div0 (s)
  (stream-add (stream-cons (/ 1.0 (stream-car s)) zeros)
	      (stream-scale
	       (stream-mul-series (stream-cons 0 (stream-cdr s))
				  (stream-div0 s))
	       (/ -1.0 (stream-car s)))))

(defun stream-div0 (s)
  (stream-cons (/ 1.0 (stream-car s))
	       (stream-mul-series
		(stream-scale (stream-cdr s) (/ -1.0 (stream-car s)))
		(stream-div0 s))))

(defun stream-div-series (s1 s2)
  (stream-mul-series s1 (stream-div0 s2)))

(setq tan-series (stream-div-series sine-series cosine-series))

(stream-ref tan-series 500)

(defun fib (n)
  (cond
   ((= n 0) 0)
   ((= n 1) 1)
   (t (+ (fib (- n 1)) (fib (- n 2))))))

(fib 5)

(defun fib2 (n)
  (let ((table (make-vector (+ n 2) 0)))
    (cl-loop for i from 2 to n
	     do (let ((i i))
		  (aset table i
			(thunk-delay (+ (thunk-force (aref table (- i 1)))
					(thunk-force (aref table (- i 2))))))))
    (aset table 0 (thunk-delay 0))
    (aset table 1 (thunk-delay 1))
    (thunk-force (aref table n))))
					;(thunk-force (aref table n))))

(fib2 2)

(setq mat0
      [[131 673 234 103 18]
       [201 96 342 965 150]
       [630 803 746 422 111]
       [537 699 497 121 956]
       [805 732 524 37 331]]
      )

(setq mat1 (make-vector 5 0))

(cl-loop for i from 0 to 4
	 do (aset mat1 i (make-vector 5 0))
	 (cl-loop for j from 0 to 4
		  do (aset (aref mat1 i) j (aref (aref mat0 i) j))))

(defun mat-ref (mat i j)
  (aref (aref mat i) j))
(defun mat-set (mat i j newval)
  (aset (aref mat i) j newval))
(defun make-matrix (i j)
  (let ((ret (make-vector i nil)))
    (cl-loop for k from 0 to (- i 1)
	     do (aset ret k (make-vector j nil)))
    ret))

(setq thunk-mat (make-matrix 5 5))
(cl-loop
 for i from 0 to 4
 do (cl-loop
     for j from 0 to 4
     do (mat-set thunk-mat i j
		 (let ((i i)
		       (j j))
		   (thunk-delay
		    (+ (mat-ref mat0 i j)
		       (min (thunk-force (mat-ref thunk-mat i (- j 1)))
			    (thunk-force (mat-ref thunk-mat (- i 1) j)))))))))


(cl-loop
 for i from 0 to 4
 do (mat-set thunk-mat i 0
	     (let ((i i))
	       (thunk-delay (+ (thunk-force (mat-ref thunk-mat (- i 1) 0))
			       (mat-ref mat0 i 0)))))
 do (mat-set thunk-mat 0 i
	     (let ((i i))
	       (thunk-delay (+ (thunk-force (mat-ref thunk-mat 0 (- i 1)))
			       (mat-ref mat0 0 i))))))
(mat-set thunk-mat 0 0
	 (thunk-delay (mat-ref mat0 0 0)))


(thunk-force (mat-ref thunk-mat 4 4))

max-specpdl-size

(defun fib (n)
  (cond
   ((= n 0) 0)
   ((= n 1) 1)
   (t (+ (fib (- n 1))
	 (fib (- n 2))))))

(fib 5)


(let ((time (current-time)))
  (fib 40)
  (message "%.06f" (float-time (time-since time))))
(setq lexical-binding t)
(iter-defun cak ()
  (iter-yield 1)
  (iter-yield 2))

(iter-defun cak ()
  (unwind-protect
      (progn
	(iter-yield 1)
	(iter-yield 2)
	(iter-yield 3))
    (iter-yield 4)))

(setq a (cak))

(iter-next a)


(iter-do)

(signal 'wrong-number-of-arguments '(x y z))
(error "%s" '(1 2 3))

'wrong-number-of-arguments

command-error-function

;; (setq lexical-binding nil)
;; -> nil

;; (lambda (x) x)
;; -> (lambda (x) x)

;; (setq lexical-binding t)
;; -> t

;; (lambda (x) x)
;; -> (closure (y yq yp ys yg a ty stream-null stream-null t) (x) x)

;; (condition-name 'wrong-number-o)
debug-on-signal
(setq debug-on-error nil)
(setq a
      (condition-case x
	  (signal 'wrong-number-of-arguments '(x  y))
	((error) (progn (print x)
			(signal (car x) (cdr x))
			1))
	(t (message "hello")))
      )

(error-message-string '(arith-error))
(setq debug-on-error t)
t

(define-error 'yy "yy's" 'error)

(symbol-plist 'yy)

(member 'error-conditions (symbol-plist 'wrong-number-of-arguments))
(error-conditions (wrong-number-of-arguments error) error-message "wrong number of arguments")

(signal 'yy '(1 2 3))

(signal 'wrong-number-of-arguments '(x  y))
(member 'error-conditions (symbol-plist 'yy))

(define-error 'new-error "a new error" 'my-own-errors)

(plist-member (symbol-plist 'my-own-errors))
(plist-member (symbol-plist 'new-error))
(setq lexical-binding nil)
(letrec ((a (lambda (x)( + x b)))
	 (b 1))
  (funcall a 2))

(setq lexical-binding 'nil)

(defvar yyy)

(setq-local cc 1)

(defvar cc 20)
cc

(defvar cc 13)

cc
1

(default-value 'cc)
x
((lambda (x)
   (defvar y 10)
   (list x y))20)
x
(makunbound 'x)
x
y
10

((lambda (y)
   (setq y 20)
   (set 'y 30)
   y) 31)

(setq yao 1)
(defvar yao)

(setq lexical-binding t)
(fset 'em
      (lambda (x)
	(+ x yao)))

(let ((yao 20))
  (em 20))

(let ((yao 30))
  ((lambda() )))
(setq lexical-binding nil)
(setq woo 1)
(defvar yyy 1)
yyy
(special-variable-p 'yyy)


(defun my-map (f &rest lss))


('+ 1 2)


(defvar a)

(setf a 1)

a
1


(consp (lambda (x) 1))
t

(functionp (lambda (x) 1))
t

(func-arity (lambda (x &optional b c &rest d) 1))
(1 . many)

(1 . 3)

(1 . 1)

(defun meme (x)
  "ehllo every

\(wocao nima)"
  (+ x 1))

(meme 1)
2

(defalias 'woca 'meme)

(woca 1)
2

(macroexpand '(defun x (x) (+ x 1)))
(defalias 'x #'(lambda (x) (+ x)))


x
(fset 'bcd (apply-partially '+ 1))

(bcd 2)
3

(symbol-name woo)
(defmacro gen-funyy (x y name docstr)
  (declare
   (doc-string 4))
  (let* ((k (intern (concat "gen-funyy" (symbol-name name)))))
    `(defun ,k (a b)
       (+ a b ,x ,y))))
gen-funyy

gen-funyy

gen-funyy


(gen-funyy 1 2 woo "123")
gen-funyywoo

gen-funyywoo

gen-funyywoo

(gen-funyywoo 1 2)
6

gen-funyyname



gen-funyyname

gen-funyyname

(macroexpand '(gen-funyy 1 2 woo "123"))
(defalias 'gen-funyyname #'(lambda (a b) (+ a b 1 2)))

(gen-funyyname 1 2)

(defmacro gen-funny (x y name docstr)
  (declare (doc-string 3))

  gen-funny

  (gen-funny 1 2 wocao "1")

  (defclass)

  ()
  (setq-default)


  (setq a 'c)
  c

  (eval a)
  1
  (eval 'a)
  c

  (setq c 1)
  1

  (defmacro foo (a)
    (list 'setq (eval a) t))
  foo

  foo

  (setq x 'b)
  b
  (setq lexical-binding nil)
  nil
  )
(macroexpand '(foo x))
(setq b t)

(setq a t)

(setq c t)

(setq b t)


t

(find-file-read-only  "koishi.txt")
write-file-functions
nil

#<window 14 on koishi.txt>

(with-temp-file "texx.txt"
  (cl-loop
   for i from 0 to 100
   do (prin1 1)))

(file-modes "texx.txt")
(format "%o" 438)
"666"


(-map-first 'numberp (lambda (x) (+ x 1)) '(1 2 b c d))

(-map-indexed (lambda (id x) (list id x)) '(a b c))

(-annotate (lambda (x) 1) '(1 23))
((1 . 1) (1 . 23))

(-splice 'numberp (lambda (x) (+ x 1)) '(1 2 3 ))

(-mapcat (lambda (x) (list x)) '(1 2 3))
(1 2 3)

(-copy '(1 2 3))
(1 2 3)
1
1

1

1

(setq lexical-binding t)
(defun x (x) x)

(symbol-function 'x)

(functionp 'x)
y2y=
(consp (symbol-function 'x))
aw5jzi0=
bmn5lq==
(unwiy)
y3kt
zi0=
bmn5
aw5jzg==
lq==
lte=
lte5
mq==

inczg(-19|lq19|lte9|lte5)
iny2y -19
ibmn5 -19
aw5jzg -19

inczi0 (19|mq9)
iny3kt 19
ibmn5lq 19
aw5jzi0 19

incf19
(- ?y  9)
incp9
(- ?y 5)
inct5

aq==
aw4=
aw5j
aw5jzg==
aw5jzje=
aw5jzje5
oq==
mtk=
zje5
y2yxoq==
bmnmmtk=
bmnmmtk=

(defun a (x)
  (lambda (b)
    (+ x b)))

(fset 'b (a 1))

(funcall 'b  2)

(setq a 'car)

(funcall a '(1 2))

(defun c (xxx)
  (lambda ()
    (funcall xxx)))

(setq b (c (lambda () 1)))


(let ((a 1))
  ;;;;;;;;;;;;;;;;;;;;;
  ;; (+ a	     ;;
  ;;    (let ((b 2)) ;;
  ;;      (+ a b)))) ;;
  ;;;;;;;;;;;;;;;;;;;;;
  )

b

(let ()
  (setq b 1)
  b)
b

(setq lexical-binding t)

b

(defun add-gen (x)
  (lambda (y) (+ x y)))

(fset 'add1 (add-gen 1))
(let ((x 1))
  (add1 1))

(setq x 1)
(defun add (y) (+ x y))
(let ((x 2))
  (add 1))

(let ((a 1))
  (+ a (let ((a 2)) a)))
(setq lexical-binding nil)
(defvar a 1)
(defun b (x)
  (+ a x))
(let ((a 2))
  (b 1))
(setq y 1)
(defun yy (x)
  (+ x y))

(symbol-function 'yy)
(closure (y yq yp ys yg ty stream-null stream-null yyy cc cc yao ...) (x) (+ x y))

(setq lexical-binding t)

(let ((y 2))
  (yy 1))

(setq lexical-binding t)
(setq y 1)
(defun yy (x)
  (+ x y))
(setq lexical-binding nil)
(let ((y 1))
  (yy 2))

(special-variable-p 'y)

(setq lexical-binding t)
(defmacro yy2 (x)
  `(+ 1 ,x))
(symbol-function 'yy2)
(macro closure (y yq yp ys yg ty stream-null stream-null yyy cc cc yao yyy t) (x) (list '+ 1 x))


(macroexpand '(yy2 1))

(setq lexical-binding t)
(defmacro foo (a)
  (list 'setq (eval a) t))
(setq a 'c)
(foo a)
c
t
(setq lexical-binding t)
(defmacro yy-unless (condit &rest exp)
  `(if (funcall 'not ,condit)
       (progn ,@exp)
     nil))


(cl-flet ((not (x) x))
  (yy-unless t (+ 1 2)))

(symbol-function 'yy-unless)


(read (get-buffer "scratch.el"))
1 2
(read t)

standard-input
t
(print 1 t)

(terpri)

pp

(set-binary-mode 'stdin t)

enable-recursive-minibuffers
nil

(read-regexp ">")

(setq a (read-from-minibuffer ">:" "abc" nil nil '() "def"))


(read-from-minibuffer (propertize "bold" 'face '(bold default)))
bold

minibuffer-allow-text-properties

(read-from-minibuffer ">> " nil nil t)

(read-regexp)

(read-no-blanks-input ">:")

history-length

query-replace-history

file-name-history

(try-completion "wo" '("wocaonima" "nimawocao" "wocao"))

#a(1 2)

(gethash 2 #s(hash-table data (1 2 2 3)))
3

2


#s(has-table (1 2 2 3))

#s(hash-table (1 2 2 3))
#s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data ())


#s(has-table (1 2 2 3))

(try-completion "fo" '("foo" "bar") 'stringp)

(try-completion "ffip" obarray)
"ffip-find-executable"

"catch"
(require 'yystream)
(test-completion "yystream" obarray)
t
completion-ignore-case
t

completion-regexp-list
nil
1
;;(lazy-completion-table foo '("123" "234"))

(completing-read ">: " '("abc" "def" "cdr") nil t )
"abc"
(read-buffer ">: " )

(read-command ">")
woman

(read-color)

(color-values "white")
(65535 65535 65535)

(0 0 0)

(read-shell-command ">")
"scheme.exe "

"wocao"

"cd"

completion-at-point-functions

(yes-or-no-p ">")
nil

t

(y-or-n-p-with-timeout ">" 1 nil)
nil
(yes-or-no-p ">")
t

(read-passwd ">")
"wwoowowowoo"
(let ((read-answer-short t))
  (read-answer "foo "
	       '(("yes"  ?y "perform the action")
		 ("no"   ?n "skip to the next")
		 ("all"  ?! "perform for the rest without more questions")
		 ("help" ?h "show help")
		 ("quit" ?q "exit"))))

"yes"
"yes"

(minibuffer-window)
#<window 4 on  *minibuf-0*>

(interactive-form  'find-file)
(interactive (byte-code "\300\301\302 \"\207" [find-file-read-args "find file: " confirm-nonexistent-file-or-buffer] 3))

nil

(commandp 'try-completion)
nil

t

t
this-command
eval-print-last-sexp

eval-print-last-sexp

eval-print-last-sexp

last-command
move-end-of-line

move-end-of-line

move-end-of-line

move-end-of-line

move-end-of-line

move-end-of-line

move-end-of-line

eval-last-sexp

(this-command-keys)
"
"

12
12
(read-key-sequence ">")
[(down-mouse-1 (#<window 3 on scratch.el> 40082 (300 . 386) 665586171 nil 40082 (18 . 12) nil (204 . 26) (16 . 30)))]

"a"

""

(read-event)
57

(down-mouse-1 (#<window 3 on scratch.el> 40200 (469 . 242) 666007046 nil 40200 (29 . 8) nil (373 . 2) (16 . 30)))

16
(read-event ">" nil 1)
nil
cursor-in-echo-area

(let ((cursor-in-echo-area t))
  (read-event ">"))
3

3

49
(read-event)
97

f1

right

(read-char)
97

(read-key)

num-nonmacro-input-events
1157
123                        --
1152                      /  \__
123                       \  /  \
1                          --
1146

1137

1133

1129

1125

1
1
(read-event)
(keyboard-translate ?\c-x 'control-x)
(global-set-key [control-x] 'kill-region)

(sit-for 1)
nil

t

command-history
((kill-buffer "b") (switch-to-buffer "b") (avy-goto-char 115 nil) (avy-goto-char-2 115 32 nil nil nil) (describe-key '(("" . [33554433]))) (describe-key '(("	" . [9]))) (describe-key '(([up] . [up]))) (describe-key '(("l" . [108]))) (lisp-interaction-mode) (smex) (execute-extended-command nil "smex" "sm"))

(start-kbd-macro)

(kbd "<f1>")
[f1]

company-mode-hook
(company-mode-set-explicitly)
(setq a '((lambda () (print 1))))
((lambda nil (print 1)))

a
((lambda nil (print 1)))
(run-hooks a)
a
((lambda nil (print 1)))

company-mode-hook

company-global-modes

(add-hook 'a (lambda () (print 2)))
((closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) nil (print 2)) (lambda nil (print 1)))
a
((closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) nil (print 2)) (lambda nil (print 1)))

(setq x (list 0.5	1	1.5	2	2.5	3	3.5	4	4.5	5	5.5	6	6.5	7	7.5	8	-0.5	-1	-1.5	-2	-2.5	-3	-3.5	-4	-4.5	-5	-5.5	-6	-6.5	-7	-7.5	-8 0
	      ))
(0.5 1 1.5 2 2.5 3 3.5 4 4.5 5 5.5 6 ...)

[0.5 1 1.5 2 2.5 3 3.5 4 4.5 5 5.5 6 ...]

(setq x (sort x '<))
(-8 -7.5 -7 -6.5 -6 -5.5 -5 -4.5 -4 -3.5 -3 -2.5 ...)


(setq y (list 120	240	390	520	640	770	890	1010	1120	1230	1350	1460	1580	1690	1800	1920	-70	-190	-360	-480	-610	-710	-830	-950	-1100	-1230	-1350	-1470	-1610	-1730	-1850	-1980 0
	      ))
(120 240 390 520 640 770 890 1010 1120 1230 1350 1460 ...)

(setq y (sort y '<))
(-1980 -1850 -1730 -1610 -1470 -1350 -1230 -1100 -950 -830 -710 -610 ...)


(defun yycal (x)
  (+ (* x 244.71) 6.5625))
yycal

(defun yysub (x1 x2)
  (cond
   ((null x1) '())
   (t
    (cons (- (car x1) (car x2))
	  (yysub (cdr x1) (cdr x2))))))
yysub

yysub

yysub

(apply 'max (-map 'abs (yysub y (-map 'yycal x))))
48.14750000000001
(/ 48.14 1980 0.01)
2.4313131313131313


(28.882499999999936 21.237499999999955 23.592499999999973 25.94749999999999 8.302500000000009 10.657500000000027 13.012500000000045 5.367500000000064 22.277500000000032 19.922500000000014 17.567499999999995 4.787500000000023 ...)
(max 1 2)
2
(max 1 2 3)
3

(setq x (list 20	40	60	80	100	120	140	160	180	200
	      ))
(20 40 60 80 100 120 140 160 180 200)

(setq y (list -90	-190	-290	-390	-480	-580	-680	-790	-890	-1000
	      ))

(defun yycal (x)
  (+ 14 (* -5.0182 x)))
yycal

yysub
x
(20 40 60 80 100 120 140 160 180 200)
y
(-90 -190 -290 -390 -480 -580 -680 -790 -890 -1000)


(apply 'max (-map 'abs (yysub y (-map 'yycal x))))
(/ 10.3599999999999 0.01 1000)
1.03599999999999


(this-command-keys-vector)
(progn
  (call-interactively 'next-line)
  (called-interactively-p 'any)
  )
(untrace-function 'call-interactively)
(trace-function 'call-interactively)
nil

(-function 'call-interactively)

(read-key "> ")

(expt 2 32)

(/  4294967296  2)
(type-of 2147483642147483648214748364214748364812312312312121221.1)
float

integer

integer

(type-of 1)
integer

(read-key-sequence "> ")
(event-click-count 'down-mouse)
1
double-click-time
500

1

1


1

1

1
(track-mouse
  (let ((a 1))
    (+ a 1)))
(read-key ">")
97

(event-basic-type 'mouse-1)
mouse-1

(click)

(shift control)

(control)

(shift)

nil

(event-convert-list '(control meta ?a))
134217729

(read-key-sequence "> ")
"\346"


num-input-keys
392

297
297


(read-event)
(down-mouse-1 (#<window 3 on scratch.el> 43918 (741 . 1309) 137959281 nil 43918 (46 . 43) nil (533 . 19) (16 . 30)))

97

(read-char)
(read-key-sequence-vector nil)
(read-key)
(read-event)

keyboard-translate-table
nil

input-method-function
list

extra-keyboard-modifiers
0

(let ((extra-keyboard-modifiers ?\c-x))
  (read-key))

(keyboard-translate ?\c-q 'yy-x)
keyboard-translate-table

(setq a(read-key))
(setq keyboard-translate-table nil)
nil

(read-key)
(read-quoted-char)

input-method-function

(read-event)
97

97

97

unread-command-events
nil
(progn (sleep-for 2)
       (discard-input))
nil

asda
nil

nil


nil


nil

nil
while-no-input-ignore-events
nil
last-input-event
10

10
(read-char)
106

(redisplay)
t

(read-key-sequence nil)
?
19

(setq quit-flag t)


(read-from-minibuffer "> ")
(keyboard-quit)

(defun display-prefix (arg)
  "display the value of the raw prefix arg."
  (interactive "p")
  (message "%s" arg))

(prefix-numeric-value '(1))
1
last-prefix-arg
nil
(read-from-minibuffer ">")
""
(recursion-depth)
0

nil

1

1

-1
command-history
((top-level) (execute-extended-command nil "top-level" "top-") (negative-argument nil) (execute-extended-command nil "negative-argument" "nega") (digit-argument nil) (execute-extended-command nil "digit-argument" "digit") (universal-argument) (execute-extended-command nil "universal-argument" "u") (universal-argument) (execute-extended-command nil "universal-argument" "univ") (display-prefix -77) (execute-extended-command -77 "display-prefix" nil) ...)

executing-kbd-macro
global-abbrev-table

(current-global-map)

lisp-mode-map
(keymap
 (menu-bar
  keymap
  (lisp "lisp"
	keymap
	(ind-sexp menu-item "indent sexp" indent-sexp :help "indent each line of the list starting just after point")
	(ev-def menu-item "eval defun" lisp-eval-defun :help "send the current defun to the lisp process made by m-x run-lisp")
	(run-lisp menu-item "run inferior lisp" run-lisp :help "run an inferior lisp process, input and output via buffer `*inferior-lisp*'")
	"lisp"))
 (3 keymap
    (26 . run-lisp))
 (27 keymap
     (24 . lisp-eval-defun))
 keymap
 (127 . backward-delete-char-untabify)
 (27 keymap
     (17 . indent-sexp))
 keymap (27 keymap (17 . prog-indent-sexp)))
(functionp 'keymapp)

t

(make-sparse-keymap ">")
(keymap ">")
(keymap-parent lisp-mode-map)
(keymap (127 . backward-delete-char-untabify) (27 keymap (17 . indent-sexp)) keymap (27 keymap (17 . prog-indent-sexp)))
(setq a (make-sparse-keymap))
(keymap)

a
(keymap)

(set-keymap-parent a text-mode-map)



(kbd "\c-x \m-x")
[24 248]

""
ctl-x-4-map
esc-map
goto-map
(keymap (101 . avy-goto-word-0) (119 . avy-goto-word-1) (9 . move-to-column) (112 . previous-error) (110 . next-error) (27 keymap (112 . previous-error) (110 . next-error) (103 . goto-line)) (103 . avy-goto-line) (99 . goto-char))


(global-set-key [?\c-x 4  ] 'next-line)

(kbd "\c-x 4 g")
"4g"
(global-set-key "4g" 'next-line)
next-line

ctl-x-4-map
(keymap (103 . next-line) (10 . dired-jump-other-window) (46 . xref-find-definitions-other-window) (100 . dired-other-window) (15 . display-buffer) (98 . switch-to-buffer-other-window) (6 . find-file-other-window) (114 . find-file-read-only-other-window) (102 . find-file-other-window) (48 . kill-buffer-and-window) (99 . clone-indirect-buffer-other-window) (97 . add-change-log-entry-other-window) (109 . compose-mail-other-window))

vc-prefix-map
(keymap (120 . vc-delete-file) (126 . vc-revision-other-window) (68 . vc-root-diff) (61 . vc-diff) (80 . vc-push) (43 . vc-update) (118 . vc-next-action) (117 . vc-revert) (115 . vc-create-tag) (114 . vc-retrieve-tag) (109 . vc-merge) (77 keymap (68 . vc-diff-mergebase) (76 . vc-log-mergebase)) (79 . vc-log-outgoing) (73 . vc-log-incoming) (76 . vc-print-root-log) (108 . vc-print-log) (105 . vc-register) (104 . vc-region-history) (71 . vc-ignore) (103 . vc-annotate) (100 . vc-dir) (98 . vc-switch-backend) (97 . vc-update-change-log))

(symbol-plist 'keymap)
(char-table-extra-slots 0)

(current-active-maps)

(make-sparse-keymap)
(keymap)

emulation-mode-map-alists
(company-emulation-alist yas--direct-keymaps)
minor-mode-overriding-map-alist
nil
(global-key-binding "\c-f")
forward-char

nil
(key-binding [6])
forward-char

(current-local-map)
(keymap (menu-bar keymap (lisp-interaction "lisp-interaction" keymap (complete-symbol menu-item "complete lisp symbol" completion-at-point :help "perform completion on lisp symbol preceding point") (indent-pp-sexp menu-item "indent or pretty-print" indent-pp-sexp :help "indent each line of the list starting just after point, or prettyprint it") (edebug-defun-lisp-interaction menu-item "instrument function for debugging" edebug-defun :help "evaluate the top level form point is in, stepping through with edebug" :keys "c-u c-m-x") (eval-print-last-sexp menu-item "evaluate and print" eval-print-last-sexp :help "evaluate sexp before point; print value into current buffer") (eval-defun menu-item "evaluate defun" eval-defun :help "evaluate the top-level form containing point, or after point") "lisp-interaction")) (10 . eval-print-last-sexp) (27 keymap (9 . completion-at-point) (17 . indent-pp-sexp) (24 . eval-defun)) keymap (127 . backward-delete-char-untabify) (27 keymap (17 . indent-sexp)) keymap (27 keymap (17 . prog-indent-sexp)))

minor-mode-alist
((diff-minor-mode " diff") (yas-minor-mode " yas") (xref-etags-mode "") (company-search-mode company-search-lighter) (company-mode company-lighter) (highlight-parentheses-mode " hl-p") (aggressive-indent-mode " =>") (indent-guide-mode " ing") (buffer-face-mode " bufface") (text-scale-mode (" " text-scale-mode-lighter)) (dired-utils-format-information-line-mode "") (dash-fontify-mode dash-fontify-mode-lighter) (superword-mode " ²") (subword-mode " ,") (eldoc-mode eldoc-minor-mode-string) (visible-mode " vis") (visual-line-mode " wrap") (next-error-follow-minor-mode " fol") (abbrev-mode " abbrev") (overwrite-mode overwrite-mode) (auto-fill-function " fill") (defining-kbd-macro mode-line-defining-kbd-macro) (isearch-mode isearch-mode) (ace-window-mode ace-window-mode))

(key-binding "\c-x")
control-x-prefix

(event-modifiers ?\c-x)
(control)

(lookup-key (current-local-map) "\c-c")
nil
nil
forward-char

(define-key (current-global-map) (kbd "c-p") ctl-x-map)

(setq mymap (make-sparse-keymap))
(keymap)
(kbd "c-x f")
"f"

(define-key mymap (kbd "c-x f") 'find-file)
find-file

find-file

find-file

find-file
mymap
(keymap (24 keymap (102 . find-file) (45 keymap (102 . find-file))) (67 keymap (45 keymap (120 keymap ...))) (94 keymap (88 keymap (102 . find-file))))

(keymap (24 keymap (45 keymap (102 . find-file))) (67 keymap (45 keymap (120 keymap (32 keymap (102 . find-file))))) (94 keymap (88 keymap (102 . find-file))))

(keymap (67 keymap (45 keymap (120 keymap (32 keymap (102 . find-file))))) (94 keymap (88 keymap (102 . find-file))))

(keymap (94 keymap (88 keymap (102 . find-file))))

(substitute-key-definition 'find-file 'find-change-log mymap (current-global-map))
nil

mymap
(keymap (24 keymap (102 . find-file) (45 keymap (102 . find-file))) (67 keymap (45 keymap (120 keymap (32 keymap (102 . find-file))))) (94 keymap (88 keymap (102 . find-file))))

(substitute-key-definition 'find-file 'find-change-log mymap)
nil

mymap
(keymap (24 keymap (102 . find-change-log) (45 keymap (102 . find-change-log))) (67 keymap (45 keymap (120 keymap (32 keymap (102 . find-change-log))))) (94 keymap (88 keymap (102 . find-change-log))))
(suppress-keymap (current-global-map))
nil


(setq myp (make-sparse-keymap))
(keymap)

(define-key myp (kbd "c-c b") 'find-file)
find-file

myp
(keymap (3 keymap (98 . find-file)))

(define-key myp [remap find-file] 'kill-line)
kill-line

kill-line

myp
(keymap
 (remap keymap
	(find-file . kill-line))
 (3 keymap (98 . find-file)))

(define-key myp [remap find-file] '())
nil
myp
(keymap
 (remap keymap (find-file))
 (3 keymap (98 . find-file)))

myp
(keymap
 (remap keymap (find-file . kill-line))
 (3 keymap (98 . find-file)))

(command-remapping 'find-file nil myp)
kill-line

input-decode-map
(keymap
 (27 keymap (c-backspace) (c-delete)) (c-m-backspace) (c-m-delete) (m-backspace) (m-delete))

local-function-key-map
(keymap (backspace . [127]) (kp-delete . [deletechar]) (delete . [deletechar]) keymap (s-tab . [backtab]) (s-iso-lefttab . [backtab]) (iso-lefttab . [backtab]) (m-escape . [134217755]) (m-return . [134217741]) (m-clear . [134217740]) (m-linefeed . [134217738]) ...)

?a
65

key-translation-map
(keymap
 (double-mouse-1 . mouse--click-1-maybe-follows-link)
 (mouse-1 . mouse--click-1-maybe-follows-link)
 (double-down-mouse-1 . mouse--down-1-maybe-follows-link)
 (down-mouse-1 . mouse--down-1-maybe-follows-link)
 (24 keymap (56 . iso-transl-ctl-x-8-map)))

(defun hyperify (prompt)
  (let ((e (read-event)))
    (vector (if (numberp e)
		(logior (ash 1 24) e)
	      (if (memq 'hyper (event-modifiers e))
		  e
		(add-event-modifier "h-" e))))))
hyperify


(defun add-event-modifier (string e)
  (let ((symbol (if (symbolp e) e (car e))))
    (setq symbol (intern (concat string (symbol-name symbol))))
    (if (symbolp e) symbol
      (cons symbol (cdr e)))))
add-event-modifier

(define-key local-function-key-map "\c-cc" 'hyperify)
hyperify

(accessible-keymaps (current-local-map))

(map-keymap (lambda(type value)(print type) (print value)) (current-local-map))

menu-bar

(keymap (lisp-interaction "lisp-interaction" keymap (complete-symbol menu-item "complete lisp symbol" completion-at-point :help "perform completion on lisp symbol preceding point") (indent-pp-sexp menu-item "indent or pretty-print" indent-pp-sexp :help "indent each line of the list starting just after point, or prettyprint it") (edebug-defun-lisp-interaction menu-item "instrument function for debugging" edebug-defun :help "evaluate the top level form point is in, stepping through with edebug" :keys "c-u c-m-x") (eval-print-last-sexp menu-item "evaluate and print" eval-print-last-sexp :help "evaluate sexp before point; print value into current buffer") (eval-defun menu-item "evaluate defun" eval-defun :help "evaluate the top-level form containing point, or after point") "lisp-interaction"))

10

eval-print-last-sexp

27

(keymap (9 . completion-at-point) (17 . indent-pp-sexp) (24 . eval-defun))

127

backward-delete-char-untabify

27

(keymap (17 . indent-sexp))

27

(keymap (17 . prog-indent-sexp))
nil


(where-is 'find-file)
find-file is remapped to ido-find-file
which is on <open>, c-x c-f, <menu-bar> <file> <new-file>nil

(where-is 'kill-line)
kill-line is on c-k, <deleteline>nil

(describe-bindings (kbd "c-c"))

(keymap-prompt (current-local-map))
nil

(color-values "#ffffff")
(65535 65535 65535)

(24672 24672 24672)
(key-binding (kbd "c-x c-k"))
kmacro-keymap
(keymap (120 . kmacro-to-register) (110 . kmacro-name-last-macro) (98 . kmacro-bind-to-key) (32 . kmacro-step-edit-macro) (108 . kmacro-edit-lossage) (101 . edit-kbd-macro) (13 . kmacro-edit-macro) (5 . kmacro-edit-macro-repeat) (1 . kmacro-add-counter) (9 . kmacro-insert-counter) (3 . kmacro-set-counter) (6 . kmacro-set-format) (12 . kmacro-call-ring-2nd-repeat) (20 . kmacro-swap-ring) (4 . kmacro-delete-ring-head) (22 . kmacro-view-macro-repeat) (16 . kmacro-cycle-ring-previous) (14 . kmacro-cycle-ring-next) (113 . kbd-macro-query) (114 . apply-macro-to-region-lines) (11 . kmacro-endab-or-call-macro-repeat) (19 . kmacro-start-macro) (115 . kmacro-start-macro))
abc
abcabcabc?1?2?3
abc
kmacro-ring-max
1
113113"wocao114nima"
wocaonima0\nwocaonima1\n
wo ni ma

(woca)
(fset 'w?1?2?3oca
      ?1?2?3      (kmacro-lambda-form
		   [?\c-p ?\c-p ?\c-p ?\c-p ?\c-x ?q ?\c-p ?\c-p] 0 "%d"))


(fset 'cak
      (kmacro-lambda-form
       [?1 ?2 ?3 ?4] 0 "%d"))

(cak)1234
12341234

1
1
1
1
11


(menu-bar-mode 1)
(menu-foo-mode 1)
(menu-foo-mode 1)




(setq tty-menu-open-use-tmm nil)

(key-binding (kbd "c-x.8"))

(add-hook 'yy-hook (lambda (x) 1))

lexical-binding

(key-binding (kbd "c-x ret"))
举头望明月

(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar widget-example-repeat)

(defun widget-example ()
  "create the widgets from the widget manual."
  (interactive)
  (switch-to-buffer "*widget example*")
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "here is some documentation.\n\n")
  (widget-create 'editable-field
                 :size 13
                 :format "name: %v " ; text after the field!
                 "my name")
  (widget-create 'menu-choice
                 :tag "choose"
                 :value "this"
                 :help-echo "choose me, please!"
                 :notify (lambda (widget &rest ignore)
                           (message "%s is a good choice!"
                                    (widget-value widget)))
                 '(item :tag "this option" :value "this")
                 '(choice-item "that option")
                 '(editable-field :menu-tag "no option" "thus option"))
  (widget-create 'editable-field
                 :format "address: %v"
                 "some place\nin some city\nsome country.")
  (widget-insert "\nsee also ")
  (widget-create 'link
                 :notify (lambda (&rest ignore)
                           (widget-value-set widget-example-repeat
                                             '("en" "to" "tre"))
                           (widget-setup))
                 "other work")
  (widget-insert
   " for more information.\n\nnumbers: count to three below\n")
  (setq widget-example-repeat
        (widget-create 'editable-list
                       :entry-format "%i %d %v"
                       :notify
                       (lambda (widget &rest ignore)
                         (let ((old (widget-get widget
                                                ':example-length))
                               (new (length (widget-value widget))))
                           (unless (eq old new)
                             (widget-put widget ':example-length new)
                             (message "you can count to %d." new))))
                       :value '("one" "eh, two?" "five!")
                       '(editable-field :value "three")))
  (widget-insert "\n\nselect multiple:\n\n")
  (widget-create 'checkbox t)
  (widget-insert " this\n")
  (widget-create 'checkbox nil)
  (widget-insert " that\n")
  (widget-create 'checkbox
                 :notify (lambda (&rest ignore) (message "tickle"))
                 t)
  (widget-insert " thus\n\nselect one:\n\n")
  (widget-create 'radio-button-choice
                 :value "one"
                 :notify (lambda (widget &rest ignore)
                           (message "you selected %s"
                                    (widget-value widget)))
                 '(item "one") '(item "another one.")
                 '(item "a final one."))
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (if (= (length
                                   (widget-value widget-example-repeat))
                                  3)
                               (message "congratulation!")
                             (error "three was the count!")))
                 "apply form")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (widget-example))
                 "reset form")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup))

(widget-example )


(major-mode-suspend)
(major-mode-restore)
(setq a 1)
1

(unintern 'a)
t

t
(setq-default a 1)

(setq-local a 2 )

major-mode
lisp-interaction-mode

(default-value 'major-mode)
text-mode

(symbol-plist 'lisp-interaction-mode)
(derived-mode-parent emacs-lisp-mode
		     event-symbol-element-mask (lisp-interaction-mode 0)
		     event-symbol-elements (lisp-interaction-mode)
		     modifier-cache ((0 . lisp-interaction-mode)))
(setq-local a 1)
(kill-local-variable 'a)

(lisp-interaction-mode)
(key-binding "\c-\\")
toggle-input-method

lisp-interaction-mode-syntax-table

font-lock-defaults
((lisp-el-font-lock-keywords lisp-el-font-lock-keywords-1 lisp-el-font-lock-keywords-2) nil nil nil nil (font-lock-mark-block-function . mark-defun) (font-lock-extra-managed-props help-echo) (font-lock-syntactic-face-function . lisp-font-lock-syntactic-face-function))

(set-auto-mode t)
(set-buffer-major-mode (current-buffer))

initial-major-mode
lisp-interaction-mode

interpreter-mode-alist
(("ruby1.8" . ruby-mode)
 ("ruby1.9" . ruby-mode)
 ("jruby" . ruby-mode)
 (("ruby1.8" . ruby-mode)
  ("ruby1.9" . ruby-mode)
  ("jruby" . ruby-mode)
  ("rbx" . ruby-mode)
  ("ruby" . ruby-mode)
  ("python[0-9.]*" . python-mode)
  ("rhino" . js-mode)
  ("gjs" . js-mode)
  ("nodejs" . js-mode)
  ("node" . js-mode)
  ("gawk" . awk-mode)
  ("nawk" . awk-mode)
  ("mawk" . awk-mode)
  ("awk" . awk-mode)
  ("pike" . pike-mode)
  ("\\(mini\\)?perl5?" . perl-mode)
  ("wishx?" . tcl-mode)
  ("tcl\\(sh\\)?" . tcl-mode)
  ("expect" . tcl-mode)
  ("octave" . octave-mode)
  ("scm" . scheme-mode)
  ("[acjkwz]sh" . sh-mode)
  ("r?bash2?" . sh-mode)
  ("dash" . sh-mode)
  ("mksh" . sh-mode)
  ("\\(dt\\|pd\\|w\\)ksh" . sh-mode)
  ("es" . sh-mode)
  ("i?tcsh" . sh-mode)
  ("oash" . sh-mode)
  ("rc" . sh-mode)
  ("rpm" . sh-mode)
  ("sh5?" . sh-mode)
  ("tail" . text-mode)
  ("more" . text-mode)
  ("less" . text-mode)
  ("pg" . text-mode)
  ("make" . makefile-gmake-mode)
  ("guile" . scheme-mode)
  ("clisp" . lisp-mode)
  ("emacs" . emacs-lisp-mode)))

magic-mode-alist
nil

magic-fallback-mode-alist
((yas-snippet-mode-buffer-p . snippet-mode) (image-type-auto-detected-p . image-mode) ("\\(pk00\\)?[p]k" . archive-mode) ("\\(?:<\\?xml[
]+[^>]*>\\)?[
]*<\\(?:!--\\(?:[^-]\\|-[^-]\\)*-->[
]*<\\)*\\(?:!doctype[
]+[^>]*>[
]*<[
]*\\(?:!--\\(?:[^-]\\|-[^-]\\)*-->[
]*<\\)*\\)?[hh][tt][mm][ll]" . mhtml-mode) ("<!doctype[
]+[hh][tt][mm][ll]" . mhtml-mode) ("<\\?xml " . xml-mode) ("[
]*<\\(?:!--\\(?:[^-]\\|-[^-]\\)*-->[
]*<\\)*!doctype " . sgml-mode) ("\320\317\340\241\261\341" . doc-view-mode-maybe) ("%!ps" . ps-mode) ("# xmcd " . conf-unix-mode))

auto-mode-alist
(("\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'" nil epa-file) ("\\.elc\\'" . elisp-byte-code-mode) ("\\.zst\\'" nil jka-compr) ("\\.dz\\'" nil jka-compr) ("\\.xz\\'" nil jka-compr) ("\\.lzma\\'" nil jka-compr) ("\\.lz\\'" nil jka-compr) ("\\.g?z\\'" nil jka-compr) ("\\.bz2\\'" nil jka-compr) ("\\.z\\'" nil jka-compr) ("\\.vr[hi]?\\'" . vera-mode) ("\\(?:\\.\\(?:rbw?\\|ru\\|rake\\|thor\\|jbuilder\\|rabl\\|gemspec\\|podspec\\)\\|/\\(?:gem\\|rake\\|cap\\|thor\\|puppet\\|berks\\|vagrant\\|guard\\|pod\\)file\\)\\'" . ruby-mode) ...)


minor-mode-list
(ace-window-mode isearch-mode defining-kbd-macro ispell-minor-mode edebug-mode ace-window-display-mode avy-linum-mode yas-global-mode yas-minor-mode xref-etags-mode company-tng-mode company-search-mode ...)

indent-guide-mode
t

minor-mode-alist
((ispell-minor-mode " spell") (edebug-mode " *debugging*") (yas-minor-mode " yas") (xref-etags-mode "") (company-search-mode company-search-lighter) (company-mode company-lighter) (highlight-parentheses-mode " hl-p") (aggressive-indent-mode " =>") (indent-guide-mode " ing") (buffer-face-mode " bufface") (text-scale-mode (" " text-scale-mode-lighter)) (dired-utils-format-information-line-mode "") (dash-fontify-mode dash-fontify-mode-lighter) (superword-mode " ²") (subword-mode " ,") (eldoc-mode eldoc-minor-mode-string) (visible-mode " vis") (visual-line-mode " wrap") (next-error-follow-minor-mode " fol") (abbrev-mode " abbrev") (overwrite-mode overwrite-mode) (auto-fill-function " fill") (defining-kbd-macro mode-line-defining-kbd-macro) (isearch-mode isearch-mode) (ace-window-mode ace-window-mode))

header-line-format
nil
(window-header-line-height)
0

0

(force-mode-line-update)
nil

nil

(getenv "host")
nil
mode-line-mule-info
("" (current-input-method (:propertize ("" current-input-method-title) help-echo (concat "current input method: " current-input-method "
mouse-2: disable input method
mouse-3: describe current input method") local-map (keymap ...) mouse-face mode-line-highlight)) #("%z" 0 2 (local-map (keymap ...) mouse-face mode-line-highlight help-echo mode-line-mule-info-help-echo)) (:eval (mode-line-eol-desc)))
mode-line-format
((:eval (format winum-format (winum-get-number-string))) "%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)

(setq mode-line-format
      (list "-"
	    'mode-line-mule-info
	    'mode-line-modified
	    'mode-line-frame-identification
	    "%b--"
	    ;; note that this is evaluated while making the list.
	    ;; it makes a mode line construct which is just a string.
	    (getenv "host")
	    ":"
	    'default-directory
	    "   "
	    'global-mode-string
	    "   %[("
	    '(:eval (format-time-string "%f"))
	    'mode-line-process
	    'minor-mode-alist
	    "%n"
	    ")%]--"
	    '(which-function-mode ("" which-func-format "--"))
	    '(line-number-mode "l%l--")
	    '(column-number-mode "c%c--")
	    '(-3 "%p")))
("-" mode-line-mule-info mode-line-modified mode-line-frame-identification "%b--" nil ":" default-directory "   " global-mode-string "   %[(" (:eval (format-time-string "%f")) ...)

(font-lock-add-keywords major-mode "abc")
nil

(current-buffer)
#<buffer scratch.el>

(set-buffer "1")
(defun my-appendtb (buffer start end)
  (interactive "bappend to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-current-buffer
      (set-buffer (get-buffer-create buffer))
      (insert-buffer-substring oldbuf start end))))

(local-set-key "\c-cq" 'my-appendtb)
(buffer-name (current-buffer))
"scratch.el"
(buffer-name (other-buffer))
(kill-buffer (other-buffer))
(buffer-name (other-buffer))
"*messages*"

"mule-cmds.el"
(rename-buffer "sc.el")
(buffer-file-name )
"c:/users/include-yy/desktop/scratch.el"

(get-buffer "sc.el")
#<buffer sc.el>
(get-buffer (get-buffer "sc.el"))
#<buffer sc.el>

(generate-new-buffer-name "sc.el" "sc.el<2>")
"sc.el<2>"

"sc.el<2>"

"sc.el<2>"

"sc.el<2>"

"foo"

"123"

"123"

(buffer-file-name (other-buffer))
nil

buffer-file-name
(local-variable-p 'buffer-file-name)
t
(default-value 'buffer-file-name)
nil

"c:/users/include-yy/desktop/scratch.el"

buffer-file-truename
"c:/users/include-yy/desktop/scratch.el"

(get-file-buffer "scratch.el")
#<buffer sc.el>

nil

nil

nil

#<buffer sc.el>

(set-visited-file-name "scratch.el")

123

list-buffers-directory
nil

(set-visited-file-name "scratch.el")

(buffer-modified-p (current-buffer))
t

(set-buffer-modified-p (buffer-modified-p))
nil
111

(buffer-modified-tick)
24096

24093

24090

24087
(buffer-chars-modified-tick)
24129

24126

24123
(with-silent-modifications (insert ?c))ccccc

(verify-visited-file-modtime)

(clear-visited-file-modtime)

(visited-file-modtime)
(24731 59087 0 0)
(set-visited-file-modtime )

123

(ask-user-about-supersession-threat "scratch.el")
nil
nil

buffer-read-only
nil
123

(barf-if-buffer-read-only)
nil

(length (buffer-list))
15

(buffer-list)

(get-buffer-create "123")
#<buffer 123>
(bury-buffer)

(generate-new-buffer "3")
#<buffer 3<5>>

#<buffer 3<4>>

#<buffer 3<3>>

#<buffer 3<2>>
(kill-buffer "3<2>")


(make-indirect-buffer (get-buffer "scratch.el") "woc")
#<buffer woc>


(buffer-swap-text (get-buffer "scratch.el"))

(gap-position)
(gap-size)

(current-idle-time)

(point)

woc ni ma
(run-with-idle-timer 1 nil (lambda () (print (point))))
(point-min)
(point-max)
(buffer-end 0)
(buffer-size)(point-max)

(save-excursion)
(save-excursion
  (goto-char (point-max))
  (insert "wocao"))
wocaowocao

(setq inhibit-field-text-motion nil)
inhibit-field-text-motion
(line-beginning-position)
(count-lines)

(vertical-motion 2)

(next-line)

aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
(forward-list)
123
(forward-sexp)

(backward-sexp)

(beginning-of-defun )

(defun a ()
  (+ 1 2)
  (+ 2 3))
(end-of-defun )

(forward-list)





123
123
123



(point)
(setq a (make-marker))
(set-marker a 68418)
a

(insert "Q")Q

(marker-buffer )

(point-marker)

(marker-position a)

(insert-before-markers "Abc")Abc

(marker-insertion-type a)
mark

mark-ring-max

(use-region-p)

(selected-window-group)
(current-window-configuration)
(window-list-1)
(window-list)

(require 'package-x)

image-types

charset-list
(chinese-cns11643-15
 (chinese-cns11643-15 gb18030 gb18030-4-byte-ext-2 gb18030-4-byte-ext-1 gb18030-4-byte-smp gb18030-4-byte-bmp gb18030-2-byte cp154 pt154 ptcp154 mik cp850 ...))


woooo
(abbrev-mode 1)

(require 'yystream)

(yystream-foldl (lambda (acc e) (+ acc e)) 0 yystream-std-natural-seq 100)

register-separator


aaaa
bbbb
ccc  d

10(+ 10 10)
30
20(+ 10 )(+ 10 )(+ 10 )(+ 10 )


1
symbol's function definition is void: tool-bar-modecd

(tool-bar-mode)
(local-set-key )


49152
1024

doc-directory

(cd doc-directory)

(documentation-property 'command-line-processed 'variable-documentation)

(documentation 'car)
'car-safe

(defun myy ()
  "\\[myy]
\\{mapvar}
\\<mapvar>"
  (interactive)
  (print 1))

(global-set-key (kbd "C-c C-a") 'myy)
(key-binding (kbd "C-c C-a"))
values-ab
234123aAAAAAAAAAAc
(newline 10)

(delete-and-extract-region 69593 69611)

kill-ring

(copy-region-as-kill)
(kill-ring-save)

(yank-pop)
(kill-ring-save)

(delete-and-extract-region 69593 69611)

kill-ring

(copy-region-as-kill)
(kill-ring-save)
(setq kill-ring nil)
(kill-ring-save)
(current-kill 1)

(current-kill 50 t)

(setq buffer-undo-list t)
(setq buffer-undo-list nil)



MTIzMTIzMTIzMTJhc2RuY2FzZXdxb25jMTI5KCk=

(md5 "123")

<p> hello </p>
(libxml-parse-html-region 69994 70008)

(json-parse-string "{\"a\" : 1}" :object-type 'alist)

?龙
(memory-use-counts )

enable-multibyte-characters

(byte-to-position (point))

(string-bytes "我")
?我
?草

(unibyte-string 1 2 3 255)

(aref "我" 0)

(string-bytes "卧槽1")

(string-to-unibyte "我")

(multibyte-char-to-unibyte ?1)

(string-as-multibyte "我")

(characterp ?我)

(max-char)
(string-bytes "\x3fff7f")
?\x3fffff
ふ
?\x3fff7f

"\x3fff7f"


(charsetp 'iso-)

(print (format "%c" ?我))
(with-temp-buffer
  (set-visited-file-name "yy123.txt")
  (setq x 1)
  ;;(while (< x ?\x300)
  ;;(print (format "%c" x))
  ;;(setq x (+ x 1)))
  (print "wocao")
  (save-buffer))

(make-process :name "notepad")

(call-process "notepad")

(call-process "gnuplot" nil t )
(progn
  (next-line)
  (call-process "cmd" nil (current-buffer) nil
		"/C"
		"dir /B"))
aaa
aaa.txt
emacs.lnk

(call-process-shell-command "dir /B" nil (current-buffer))
aaa
aaa.txt
emacs.lnk

(make-process :name "notepad" :command '("notepad"))

(list-processes)

(process-command (get-process "cmd"))

(call-process "cmd" nil (current-buffer) nil "/K" "dir")

(progn
  (next-line)
  (call-process "cmd" nil (current-buffer) nil
		"/C"
		"dir /B"))


(setq now-p (start-process "woo" (current-buffer) "1.exe"))

(list-system-processes)

system-type

system-name

(current-time)

(run-with-idle-timer)

baud-rate

(play-sound '(sound :file "C:/Users/include-yy/Desktop/1.mp3"))(play-sound '(sound :file "C:/Users/include-yy/Desktop/1.mp3"))(play-sound '(sound :file "C:/Users/include-yy/Desktop/1.mp3"))(play-sound '(sound :file "C:/Users/include-yy/Desktop/1.mp3"))

(beep)

window-system

(print 1)

(setq echo-keystrokes .1)
(defun my-disp ()
  (print 1))
(add-function :after (symbol-function 'my-disp) '(lambda () (print 2))
	      )


((lambda (f) (funcall f '(b c)))
 '(lambda (x) (cons 'a x)))


(ad-unadvise)
(defun swt (x)
  (cl-case x
    (1 (+ x 1))
    (2 (+ x 2))
    ((3 4 5) (+ x 3))
    (t (+ x 100))))



(setq a (list 1 2 3))

(incf (car a) 1)

a
(2 2 3)

(setq x 1)
(cl-incf x)
x
2
(cl-decf x)
x
1
(setq x 1)
(cl-incf x 114513)
(cl-decf x 114495)
x
19

(let ((i 0)
      (x 0)
      (y 1))
  (while (< i 10)
    (cl-psetq x y y (+ x y))
    (cl-incf i))
  x)

(cl-flet ((a (a) (+ a 1)))
  (funcall #'a 1))


(cl-flet ((a (x) (+ x 1)))
  (b (x y) (+ x y)))
(+ (a 1) (b 2 3)))

(cl-labels ((a (ls)
	       (if (null ls)
		   0
		 (+ 1 (a (cdr ls))))))
  (mapcar #'a '((1 2 3) (2 3 4) (3 4 5))))

(defun swt (x)
  (cl-case x
    (1 (+ x 1))
    (2 (+ x 2))
    ((3 4 5) (+ x 3))
    (t (+ x 100))))

(swt 6)

(cl-block abc
  (+ 1 2)
  (cl-return-from abc 123))

(cl-block abc
(+ 1 2)
(cl-return-from abc 123))

'dash

'ido-mode

'highlight-parentheses

(cl-block wocao
(+ 1 2)
(setq x 1)
(while (< x 10)
  (cl-incf x)
  (when (= x 5) (cl-return-from wocao x))))

(cl-do
((a '(1 2 3) (cdr a))
 (b '(4 5 6) (cdr b))
 (c '(7 8 9) (cdr c))
 (d))
((and (null a) (null b) (null c)) d)
(setq d (cons (list (car a) (car b) (car c)) d)))

(cl-loop
for buf in (buffer-list)
collect (buffer-file-name buf))

(cl-loop

(print 1)
(print 2)
(print 3)
return 0)

(cl-loop (cl-incf x)(cl-return))
(setq x 0)
x

(cl-loop
with x = 1
(incf x 1)
return (+ x 1))

(cl-loop with x = 1 do (incf x) (incf x) (incf x) return x)

(cl-loop for x in '(1 2 3 4 5 6)  (* x x))

(cl-loop with x = 1 do
collect x
(incf x)
return x
)

(cl-loop
collect 1 into w
collect 1 into w
collect 1 into w
collect 1 into w
collect 1 into w
do
(print w)
return 0)
(cl-loop
 collect 1 into w
 collect 1 into w
 collect 1 into w
 collect 1 into w
 collect 1 into w
 return w)

(cl-loop
 with x = 1
 sum 1 into x
 sum 2 into x
 sum 3 into x
 sum 4 into x
 return x)

(cl-loop )

(cl-loop
 for x from 0 to 10 by 2
 sum x)

(cl-loop
 for x from 10 downto 1
 collect x)

(cl-loop
 for x from 10 above 1
 collect x)

(cl-loop
 for x from 0 below 10
 collect x)

(cl-loop
 for x in '(1 2 () 3)
 collect x)

(nth 3 '(1 2 () 3))

(cl-loop
 for x in '(1 2 () 3)
 collect (if (numberp x) x 0))

(setq x (list 2 3 4 5 6 7))
(cl-loop
for a in-ref x by 'cddr
do
(setf a (+ a 1)))
x
a

(cl-loop
 repeat ((lambda (x) (+ x 2)) 10)  collect 1)

(cl-loop repeat 10 sum 1)

(cl-loop
 for x in '(1 3 4 5 7 9)
 always (cl-oddp x))

(cl-loop
 for x to 100
 maximize x into a
 minimize x into b
 finally return (list a b))

(cl-loop
for x on '(1 2 3)
append x)

(odd)

(or 1 2)

(floor)

(random)

(defun yys (n n2)
  (+ n (- (/ (* n2 (+ n2 1)) 2) (/ (* n (+ n 1)) 2))))

(yys 2 4)
(sqr)

(cl-floor 20 2)

(cl-isqrt 99)

mod
%

(random)

(cl-floor -1.2)

(cl-ceiling 1.7)

(cl-floor 10 3)

(truncate 1.5)

(cl-truncate -1.6)
(cl-round 1.4)

(cl-round -1.3)

(cl-ceiling 10 3.1)

(/ 10 3.1)

(cl-parse-integer "123" :radix 11)

(int-to-string 123)
(string-to-number "123.45")

(cl-random)

cl--random-state

(setq ss1 (cl-make-random-state t))
(setq ss2 (cl-make-random-state ss1))

(cl-random 100 ss1)
(cl-random 100 ss2)
(untrace-function 'cl-random)

(setq ss3 (cl-make-random-state))
(setq ss4 (cl-make-random-state ss3))

(cl-random 100)
(cl-random 100 ss3)
(cl-random 100 ss4)


(get)
' cl-lib
(default ^(\(defun\|defmacro\) +cl-[^- ]+): ^(\(defun\|defmacro\) +cl-[^- ]+))
'cl-extra

'cl-seq

'cl-macs

(require 'cl-macs)

(cl-the)
(setq x 1)
(cl-incf (cl-incf x))

aHR0cHM6Ly95YW5kZS5yZS9wb3N0L3Nob3cvNzY3NjU4

aHR0cHM6Ly95YW5kZS5yZS9wb3N0L3Nob3cvNzMxOTEy

aHR0cHM6Ly95YW5kZS5yZS9wb3N0L3Nob3cvNDA0MTI3

aHR0cHM6Ly95YW5kZS5yZS9wb3N0L3Nob3cvMzYyMjUy

7

aHR0cHM6Ly95YW5kZS5yZS9wb3N0L3Nob3cvMzM3NTMz
aHR0cHM6Ly95YW5kZS5yZS9wb3N0L3Nob3cvMzMzODI1
aHR0cHM6Ly95YW5kZS5yZS9wb3N0L3Nob3cvMzEyMjkw
aHR0cHM6Ly95YW5kZS5yZS9wb3N0L3Nob3cvMjk3NTI3
aHR0cHM6Ly95YW5kZS5yZS9wb3N0L3Nob3cvMjY2MM2T


(+ 1 2)

(setq point-x 0)
(setq point-y 0)
(setq a 1)

(+ -1 (cl-loop
       for i from (- a) to a
       sum (cl-loop
	    for j from (- a) to a
	    if (<= (+ (expt (- i point-x) 2)
		      (expt (- j point-y) 2))
		   (* a a))
	    sum 1)))







(setq echo-keystrokes 0.01)

(setq x 0)
(cl-loop
 do (cl-incf x))

(cl-loop
 repeat 10
 collect 1)

(cl-loop )

(cl-loop )
;; wrong example
(cl-loop collect 1 (+ 1 2))

;; right
(cl-loop collect 1 do (+ 1 2))

(cl-loop
 with x = 1
 with y = x
 with z = (+ 1 y)
 return (+ x y z))

(cl-loop for i from 0 to 10 append (list i))

(cl-loop
for i on (list 1 2 3) do (print i)); nconc i)

(setq a (list 1 2))
(setq b (list 3 4))

(setq c (list 5))

(nconc a b c)

a
b
c

(cl-loop for i from ?a to ?z concat (string i))
(cl-loop for i from 0 to 10 vconcat (vector i))

(cl-loop for i from 0 to 10 count t)
(cl-loop for i from 0 to 10 maximize i)
(cl-loop for i from 0 to 10 minimize i)

(cl-loop with x = 1
	 sum 1 into x
	 return x)

(print
 (cl-loop
  for i from 0 to 10
  collect i
  sum (+ i 1) into x
  collect x)
 )

(cl-loop
					;with y = 1
if (> 1 0)
do (format "%s" 1)
else
do (format "%s" (- 1 1)))
(setq y 1)
(cl-loop

if (> 1 0)
do '1
else
do '2)

(cl-loop do 1)





(setq echo-keystrokes 0.01)

(cl-loop 1)
(cl-loop do 1)
(cl-loop do (quote 1))

(cl-loop when 1 collect 1 into x collect 2 into x return x)


(cl-loop
 with x = 1
 if (> x 1) do (print 1) and if (> x 1.5) do (print 2) else do (print 1.5)
 return 1)

(cl-loop
 with x = 1
 if (> x 1)
 do (print 1)
 and if (> x 1.5)
 do (print 2) end
 else do (print 1.5)
 return 1)

(cl-loop when t do (print 1))
(cl-loop when t do (print 1) return 0)
(cl-loop unless nil do (print 1) return 0)
(cl-loop when nil do (print 1) else do (print 2) return 0)
(cl-loop else do (print 2) return 0)

(cl-loop unless t do (print 1) else do (print 2) return 0)

(cl-loop
 for i from 0 to 10
 collect (+ i j l)
 initially (setq j 10) (setq l 11))

(cl-loop
with x = 5
for i from 0 to 0
do (identity 1)
finally (cl-incf x)
finally (cl-incf x)
finally return x
finally (cl-incf x))

(cl-loop
 repeat 10 count t)

(cl-loop
 with x = 10
 repeat 10
 do (cl-incf x 2)
 never(cl-oddp x))

(cl-loop
with x = 10
repeat 10
do (cl-incf x 2)
thereis (= x 16))

(iter-defun wocao ()
  (cl-loop for i from 1 to 10
	   do (iter-yield i)))

(cl-loop
 iter-by wocao
 do count t)

(cl-loop
for x downto -10
collect 1)

(cl-loop
 for x upfrom 0 downto 10
 collect x)

(cl-loop
 for x downfrom 0 upto 10
 collect x)

(cl-loop for x upto 10 collect x)

(cl-loop
for i from 0 to 10
collect (* i i))
=> (0 1 4 9 16 25 36 49 64 81 100)

(cl-loop
for i to 10
collect (+ i i))
=> (0 2 4 6 8 10 12 14 16 18 20)

(cl-loop
 for i from 0
 if (= i 11)
 return x
 else
 collect i into x)
=> (0 1 2 3 4 5 6 7 8 9 10)

(cl-loop
 for i by 3
 if (< i 20)
 collect i into x
 else
 return x)
=> (0 3 6 9 12 15 18)

(cl-loop
 for i upfrom 0 to 10
 sum i)
=> 55

(cl-loop
for i downfrom 10 to 0
sum i)
=> 55

(cl-loop
for i upfrom 10
when (> i 100) return x
do (cl-incf i i)
collect i into x)
=> (20 42 86 174)

(cl-loop
for i downfrom 10 by 2
when (< i 0) return x
sum i into x)
=> 30

(cl-loop
for i from 0 upto 10
collect i)
=> (0 1 2 3 4 5 6 7 8 9 10)

(cl-loop
for i upto 10
collect i)
=> (0 1 2 3 4 5 6 7 8 9 10)

(cl-loop
for i from 0 downto -5
collect i)
=> (0 -1 -2 -3 -4 -5)

(cl-loop
 for i below 10
 collect i)
=> (0 1 2 3 4 5 6 7 8 9)

(cl-loop
for i from 10 above 5
collect i)
=> (10 9 8 7 6)


(cl-loop
 for x on '(1 2 3)
 collect x)
=> ((1 2 3) (2 3) (3))



(cl-loop
 with x = (list 1 2 3)
 for i in-ref x
 do (cl-incf i)
 finally return x)
=> (2 3 4)

(cl-loop
with x = [1 2 3]
for i across x
collect i)
=> (1 2 3)

(cl-loop
 with x = (string ?a ?b ?c)
 for i across-ref x
 do (cl-incf i 23)
 finally return x)
=> "xyz"

(cl-loop
 with x = (vector 1 2 3)
 for i being the elements of-ref x
 do (cl-incf i 100)
 finally return x)
=> [101 102 103]

(cl-loop
for x = 1 then (cl-incf x)
when (> x 10) return y
collect x into y)
=> (1 2 3 4 5 6 7 8 9 10)

(setq mat2 [[1 200 300][200 3 400][300 400 5]])

(cl-loop
for i below 3
for j below 3
sum (aref (aref mat2 i) j))
9

(cl-loop
 for i below 3
 sum (cl-loop
      for j below 3
      sum (aref (aref mat2 i) j)))
=> 1809

(cl-loop
for i across mat2
sum (cl-loop
     for j across i
     sum j))
=> 1809

(cl-loop
for i from 0 to 10
for j to 100 by i
collect (list i j))
=> ((0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0) (9 0) (10 0))



(cl-loop
 for i from 1 to 10
 for j to 5 by i
 collect (list i j))
((1 0) (2 1) (3 2) (4 3) (5 4) (6 5))

=> ((1 0) (2 1) (3 2) (4 3) (5 4) (6 5) (7 6) (8 7) (9 8) (10 9))

(cl-loop
 for x below 5
 for y = nil then x
 collect (list x y))
=> ((0 nil) (1 1) (2 2) (3 3) (4 4))
(cl-loop
 for x below 5
 and y = nil then x
 and z = nil then (+ x 1)
 collect (list x y z))
((0 nil nil) (1 0 1) (2 1 2) (3 2 3) (4 3 4))

((0 nil) (1 0) (2 1) (3 2) (4 3))

=> ((0 nil) (1 0) (2 1) (3 2) (4 3))

((0 nil) (1 0) (2 1) (3 2) (4 3))

(cl-loop
 named hello
 for i to 10
 do (cl-loop
     for j from i to 10
     if (and (= i 5) (= j 9))
     do (cl-return-from hello w)
     else
     collect (list i j) into w))
((5 5) (5 6) (5 7) (5 8))


aHR0cHM6Ly93d3cuZGV2aWFudGFydC5jb20vbmluYW1vLWNoYW4vYXJ0L0Npcm5vLWZhbmFydC0xNzg2NzU4OTY=

https://www.deviantart.com/smilocg/art/cirno-168923283

(setq echo-keystrokes 0.1)


(custom-group-members 'editing nil)

((killing custom-group)
(delete-trailing-lines custom-variable)
(indent custom-group)
(mouse custom-group)
(paragraphs custom-group)
(fill custom-group)
(electricity custom-group)
(matching custom-group)
(emulations custom-group)
(i18n custom-group)
(undo custom-group)
(editing-basics custom-group)
(yasnippet custom-group)
(rectangle custom-group))

(custom-group-members nil nil)

(setq echo-keystrokes 0.1)

(defgroup incx nil
  "include-yy's group"
  :group 'editing)

(defgroup incy nil
  "incx's group"
  :group 'incx
  :tag "incy_tag")

(defgroup incz nil
  "incy's subgroup"
  :group 'incx)

(defgroup inca nil
  "incz's subgroup"
  :group 'incz
  :link '(url-link "www.baidu.com"))

(defgroup incb nil
"incz's subgroup"
:group 'incz
:group 'incy)

custom-unlispify-remove-prefixes


(defgroup yyvar nil
  "var test"
  :group 'editing)

(defcustom yycon '(1 . 2)
  "yy's cons"
  :type '(cons integer integer))

(setq yycon '(3 . 4))

(defcustom yylst '(1 wo "123")
"yy's list"
:type '(list integer symbol string))

(defcustom yyl1 '(3 4)
  "yy l1"
  :tag "hao"
  :type '(group integer integer))

(defcustom yyl2 '(1 2)
"yy l2"
:tag "le"
:type '(list integer integer))

(defcustom yyal '((1 . 2) (2 . 3))
"yy's alist"
:type '(alist :key-type integer :value-type integer)
:options '(4 5 6))

(defcustom yyco 123
  "yy's choice"
  :type '(choice
	  (string :tag "str")
	  (integer :tag "int")
	  (symbol :tag "sym")))

(defcustom yyrad 123
"yy's radio"
:type '(radio string integer symbol))

yyrad
(symbolp yyrad)
(setq lexical-binding nil)
(defun add (x y)
  "add two number"
  (+ x y))
(defcustom yyfun (symbol-function 'add)
  "yy's function-item"
  :type '(function-item add))
(setq yyfun (lambda (x) "hhh" (+ x 1)))

(defcustom yyfun-val 1
"yy's fun and val item"
:type '(radio (function-item add)
	      (variable-item lexical-binding)
	      integer))

(fset 'yyse (lambda (sb va) (set sb (+ va 1))))

(defcustom yyset1 1
"yy's set1"
:type 'integer
:set 'yyse)
yyset1
(setq yyset1 1)
yyset1
yyset1
(custom-set-variables
 '(yyset1 2))

(defcustom yypt '(a  b)
"yy's plist"
:type '(plist))


(setq lexical-binding nil)
(defcustom a '(a . 1)
"yy's a"
:type '(alist :key-type symbol :value-type integer)
:set '(lambda (sym x) (set sym (list (cons (car x) (cdr x)))))
:get '(lambda (sym) (cons  1 (symbol-value sym))))



	(setq )
(setq a 20)
(custom-set-variables '(a 2))

(symbol-plist )

(symbol-plist 'b)

(cons 1 '(a . 1))

(defcustom b '(1 2 3)
"yy's b"
:type '(repeat integer)
:get (lambda (s) (-map  (lambda (x) (+ x 1)) (symbol-value s))))

(defcustom s1 1 ""
  :type 'number
  :initialize 'custom-initialize-set
  :set (lambda (s x) (set s (+ x 1))))

(setq s1 1)
s1

(defcustom s2 1 ""
  :type 'number
  :initialize 'custom-initialize-default)

(setq s2 2)
s2

(defcustom s3 1 ""
:type 'number
:initialize 'custom-initialize-reset
:set (lambda (s x) (set s (+ x 1))))
s3

(setq s3 2)

(defcustom s4 1 ""
  :type 'number
  :initialize 'custom-initialize-changed
  :(setq )et (lambda (s x) (set s (+ x 1))))
s4

(defgroup wcd nil ""
:group 'wp)

(defcustom w1 1 ""
  :type 'number
  :initialize 'custom-initialize-default)

(defcustom w2 1 ""
:type 'number
:set (lambda (s v) (progn (print 1) (set s v)))
:initialize 'custom-initialize-default
:set-after 'w1)

(defcustom w3 1 ""
:type 'number
:set (lambda (s v) (progn (print 2) (set s v)))
:initialize 'custom-initialize-default
:set-after 'w2)
w3
(load-file  "./my.el")

(setq cnt 0)
(defgroup wcd nil ""
  :group 'wp)

(defcustom w1 2 ""
  :type 'number
  :set (lambda (s v) (progn (cl-incf cnt) (set s cnt)))
  :initialize 'custom-initialize-default)

(defcustom w2 3 ""
  :type 'number
  :set (lambda (s v) (progn (cl-incf cnt) (set s cnt)))
  :initialize 'custom-initialize-default
  :set-after '(w1)
  )

(defcustom w3 4 ""
  :type 'number
  :set (lambda (s v) (progn (cl-incf cnt) (set s cnt)))
  :initialize 'custom-initialize-default
  :set-after '(w2)
  )

(load-file  "./my.el")
w1
w2
w3

(custom-set-variables
'(w3 1))
(custom-set-variables
'(w2 1))
(custom-set-variables
'(w1 1))

aHR0cHM6Ly93d3cucGl4aXYubmV0L2FydHdvcmtzLzYzMzE2OTUw
(defcustom yy-test 1 ""
  :type 'number)

yy-test

(load "1.el" nil nil t)

(add-to-list 'load-path "./")

load-read-function

load-suffixes

load-file-rep-suffixes

(get-load-suffixes )

load-path

(locate-library "yystream")

(list-load-path-shadows)

(fmakunbound 'abc)
;;;###autoload
(defun abc () 1)

(symbol-function 'run-scheme)

(locate-library "loaddefs")

generate-autoload-cookie
generated-autoload-file

features

;;(provide 'my-feature)  ; Ignored by byte compiler,

(provide 'yy)

(member 'yy features)

(provide 'yy '(a b c))

(symbol-plist 'yy)

(symbol-file 'normal-about-screen)

load-history

(eval-after-load)
(with-eval-after-load)

module-file-suffix

(defun sillya-loop (n)
  (let ((t1 (float-time)))
    (fib n)
    (- (float-time) t1)))

(defun fib (n)
  (cond
   ((= n 0) 0)
   ((= n 1) 1)
   ((= n 2) 1)
   (t (+ (fib (- n 1))
	 (fib (- n 2))))))
(silly-loop 50000000)

(sillya-loop 30)
(byte-compile 'silly-loop)

(defun add (x)
"add 1"
(+ x 1))

(symbol-function 'add)
#[257 "\211T\207" [] 2 "

(fn X)"]

(add 1)
2

(format "%x" 257)
"101"
#b000100000001
(symbol-function 'backward-sexp)
#[256
  "\211\204 \300\262\301[!\207"
  [1 forward-sexp]
  3
  1964246
  "^p"
  ]

000100000000

(expt 2 7)

(disassemble 'add)
nil
(disassemble (lambda (x) (funcall x x)))
nil

nil

(disassemble (lambda (x) (+ x (* x 2) 1)))
nil

debug-on-error
debug-ignored-errors


eval-expression-debug-on-error

(setq lexical-binding nil)


(let ((debug-on-quit t))
(while t))

(defun fact (n)
  (or (and (zerop n) 1)
      (* n (fact (- n 1)))))


(debug-on-entry 'fact)
(cancel-debug-on-entry 'fact)
(fact 10)

(defun print-debg ()
  (print 1)
  (print 2)
  (print 3)
  (print 4))

(print-debg)

(with-output-to-temp-buffer "backtrace-output"
(let ((var 1))
  (save-excursion
    (setq var (eval '(progn
		       (1+ var)
		       (list 'testing (backtrace))))))))



aHR0cHM6Ly9jaGFuLnNhbmtha3Vjb21wbGV4LmNvbS9wb3N0L3Nob3cvMjUwNjUwNDI=



(cl-random 114514191981)
324739215

mzexnde

123327781

91539

(/ #b11001100110011001100 #xfffff 1.0)

(defun parrr (str i)
  (if (= i (length str)) 0
    (+ (* (- (aref str i) ?0) (expt 2 (- i)))
       (parrr str (+ i 1)))))


(parrr "000011001100110011001100" 0)
0.09999990463256836

(setq echo-keystrokes 0.1)

(defun yy (a b) (+ a b))

(trace-function 'yy)

(yy 1 2)
(yy 2 3)
(yy 1 -1)
(setq lexical-binding nil)
(untrace-all)
a
(setq echo-keystrokes 0.1)


(defun c () (/ 1 0))
(defun b () (c))
(defun a () (b))

(a)
(setq debug-on-error nil)
debug-on-error
(toggle-debug-on-error)

eval-expression-debug-on-error

(defun yy-fact (n)
  (or (and (zerop n) 1)
      (* n (yy-fact (- n 1)))))

(debug-on-entry 'yy-fact)

(yy-fact 5)
(cancel-debug-on-entry 'yy-fact)

(defun a ()
(print 1)
(print 2)
(print 3)
(print 4)
(debug)
(print 5))

(a)

(defun b ()
  (if (debug) 1 2))

(b)

(defun c ()
  (print 1)
  (print 1)
  (print 1)
  (print 1)
  (print 1)
  (print 1)
  (print 1)
  1)
(c)
(defun d ()
  (if nil 1
    (progn
      1
      2
      3
      (+ 1 2)
      (+ 2 3))))

(d)



(defun x ()
  (/ 1 2))

(defun y ()
  (x))

(defun z () (+ 1.5 (y)))

(z)

(defun yy-fact (n)
  (or (and (zerop n) 1)
      (* n (yy-fact (- n 1)))))

(yy-fact 5)


(defun fac (n)
  (if (< 0 n)
      (* n (fac (1- n)))
    1))

(setq edebug-sit-for-seconds 1)

(fac 5)

(setq echo-keystrokes 0.1)

aHR0cHM6Ly95YW5kZS5yZS9wb3N0L3Nob3cvNjczOTUy

1


(setq lexical-binding t)

(defmacro pand (x)
  `(+ a ,x))
(setq a 10)
(pand 1)


(let ((a 20))
  (pand 20))

(defun ttt (x)
  (pand x))

(let ((a 20))
  (ttt 10))

(symbol-function 'ttt)

(symbol-function 'pand)

(setq lexical-binding nil)

(defun ttt2 (x)
  (pand x))

(let ((a 20))
  (ttt2 10))



* a
a
b
c
d
** b
a
b
c
d



org-footnote-section



* b

** d

** e

* c
(require 'ert)
(should)


(require 'chart)

(chart-bar-quickie
 'vertical "Favorite Type of Movie" ;; Type & Title
 '("Comedy" "Action" "Romance" "Drama" "Sci-Fi") "Genre" ;; Keys & Label
 '(4 5 6 1 4) "People" ;; Values & Label
 )

(defun sierpinski (s)
  (pop-to-buffer (get-buffer-create "*sierpinski*"))
  (fundamental-mode) (erase-buffer)
  (cl-labels ((fill-p (x y)
                      (cond ((or (zerop x) (zerop y)) "0")
                            ((and (= 1 (mod x 3)) (= 1 (mod y 3))) "1")
                            (t (fill-p (/ x 3) (/ y 3))))))
    (insert (format "P1\n%d %d\n" s s))
    (dotimes (y s) (dotimes (x s) (insert (fill-p x y) " "))))
  (image-mode))

(sierpinski (expt 3 5))

(defun mandelbrot ()
  (pop-to-buffer (get-buffer-create "*mandelbrot*"))
  (let ((w 400) (h 300) (d 32))
    (fundamental-mode) (erase-buffer)
    (set-buffer-multibyte nil)
    (insert (format "P6\n%d %d\n255\n" w h))
    (dotimes (y h)
      (dotimes (x w)
        (let* ((cx (* 1.5 (/ (- x (/ w 1.45)) w 0.45)))
	       (cy (* 1.5 (/ (- y (/ h 2.0)) h 0.5)))
	       (zr 0) (zi 0)
	       (v (dotimes (i d d)
		    (if (> (+ (* zr zr) (* zi zi)) 4) (return i)
		      (psetq zr (+ (* zr zr) (- (* zi zi)) cx)
			     zi (+ (* (* zr zi) 2) cy))))))
	  (insert-char (floor (* 256 (/ v 1.0 d))) 3))))
    (image-mode)))

(mandelbrot)

(require 'app-launcher)

app-launcher-apps-directories

(app-launcher-parse-files (app-launcher-list-desktop-files))


(app-launcher-list-apps)

(app-launcher-run-app)

(app-launcher--action-function-default "racket")

app-launcher--cache
#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("racket" ((file . "d:/incf/home/.local/share/applications/1.desktop") (exec . "DrRacket.exe") (comment . "Flash card based learning tool") (visible . t))))

(decode-time)

(current-time)

(format-time-string "%m.%d 36.5℃")

(cl-loop
 for i from 14 downto 0
 do (insert (substring (format-time-string
			"%m.%d 36.5℃；"
			(time-subtract (current-time) (* i 86400)))
		       1)))
7.31 36.5℃；8.01 36.5℃；8.02 36.5℃；8.03 36.5℃；8.04 36.5℃；8.05 36.5℃；8.06 36.5℃；8.07 36.5℃；8.08 36.5℃；8.09 36.5℃；8.10 36.5℃；8.11 36.5℃；8.12 36.5℃；8.13 36.5℃；8.14 36.5℃；


(time-subtract (current-time) 1)

(defadvice)

(scroll-other-window-down)

(kbd "<perior>")

(global-set-key (kbd "<f5>") 'scroll-other-window)
(global-set-key (kbd "<f6>") 'scroll-other-window-down)

(kill-ring-save)

(ad-arg-binding-field )

(defun foo (x) (1+ x))

(defadvice foo (before fg-add2 first activate)
  "add 2"
  (setq x (1+ x)))

(foo 2)

(defadvice foo (before fg-cancel-add2 0 activate)
  "back to origin"
  (setq x (1- x)))

(foo 2)
3

(documentation 'foo)
"This function has :around advice: ‘ad-Advice-foo’.

(fn X)"

(defadvice foo (before fg-inter last act)
  "Use 5 as argmunet"
  (interactive (list 5)))

(call-interactively 'foo)
6

(defadvice foo (before fg-inter2 last act)
"Use 5 as argmunet"
(interactive (list 6)))
foo

(call-interactively 'foo)
6

(defadvice foo (around fg-times-2 act)
"Fist double 2"
(let ((x (* x 2)))
  ad-do-it))

(foo 2)
5

5

(defadvice foo (around fg-add-1 first act)
  "Add 1 around"
  (let ((x (1+ x))) ad-do-it))

(foo 2)
7

(defadvice foo (after fp-tims-x)
  "Multiply result with X"
  (setq ad-return-value (* ad-return-value x)))

(defadvice foo (after fp-tims-x-again act)
"Multiply result with X"
(setq ad-return-value (* ad-return-value x)))
foo

(foo 3)
81

(ad-disable-advice 'foo 'after "^fp*")
nil

nil

(ad-update 'foo)
foo

(foo 2)
7

(foo 3)
9
(ad-enable-advice 'foo 'after "fp-tims-x")
nil

(ad-update 'foo)
foo

(foo 2)
28
(foo 3)
81

(ad-update 'foo)
foo

(foo 3)
9

(ad-enable-advice 'foo 'after "^fp-tims-x$")
nil

(ad-update 'foo)
foo

(foo 3)
27

(ad-disable-regexp "^a")

(setq old-def (symbol-function 'ad-Advice-foo))
#[(ad--addoit-function x) "\303	S\211T\211T\211\304_
	!*	_\211)\207" [ad-return-value x ad--addoit-function nil 2] 3 nil (list 5)]

(defadvice bar (before fg-sub-1-more act)
"Sub one more"
(setq x (1- x)))
bar

(fboundp 'bar)
nil

(defun bar (x)
  (1- x))
bar

(bar 1)
-1

ad-redefinition-action
accept

(ad-disable-advice 'foo 'any "^.*")
nil

(foo 1)
5

(ad-update 'foo)
foo

(foo 1)
2

(foo 1)
3

(foo 2)
5
(foo 3 )
6

(foo 4)
7

(defadvice)


(foo 1)
8

(ad-deactivate 'foo)
foo

(foo 1)
2

(ad-activate 'foo)
foo

(foo 1)
8

(ad-disable-advice 'foo 'after 'foo-mul-2)
nil

(ad-update 'foo)
foo

(foo 1)
4
(ad-ac)


(foo 1)
8

(ad-deactivate 'foo)
foo

(foo 1)
2

(ad-disable-advice 'foo 'after ".+")
nil

(ad-update 'foo)
nil

(foo 1)
2

(ad-activate 'foo)
foo

(foo 1)
4

(identity 1)
1

(defun yyid (x) x)
(defadvice yyid (before bef-1 activate)
  (setq x (concat "1" x)))
yyid

yyid

yyid


(defadvice yyid (before bef-0 activate)
  (setq x (concat "0" x)))
yyid

yyid

yyid


(defadvice yyid (around aro-1 activate)
  ad-do-it
  (setq ad-return-value (concat ad-return-value "3")))
yyid

yyid


(defadvice yyid (around aro-0 activate)
  ad-do-it
  (setq ad-return-value (concat ad-return-value "2")))
yyid

yyid

(defadvice yyid (after aft-1 activate)
  (setq ad-return-value (concat ad-return-value "5")))

(defadvice yyid (after aft-0 activate)
  (setq ad-return-value (concat ad-return-value "4")))

(yyid "-")
"10-3245"

"10-245"

(defun yy-add (a b)
  (+ a b))
yy-add

(defadvice yy-add (before yy-tri (x y z) activate)
"Add three numbers"
(setq y (+ y z)))
yy-add

yy-add

yy-add

(yy-add 2 3 3)
8


8

(defun yyc (x) (+ x 1))
yyc

(defadvice yyc (before yyc1 activate)
  (setq x (+ x 1)))
yyc


(get)

(symbol-plist 'yyc)
nil

nil

(put 'yyc 'a 123)
123

(symbol-plist 'yyc)
(a 123 defalias-fset-function #[128 "\300\301\302#\207" [apply advice--defalias-fset #[128 "\300\301\302#\207" [apply ad--defalias-fset nil nil] 5 nil] nil] 5 nil] ad-advice-info ((active . t) (advicefunname . ad-Advice-yyc) (before (yyc1 nil t (advice lambda nil (setq x (+ x 1))))) (cache #[(ad--addoit-function x) "\303	T
	!\211)\207" [ad-return-value x ad--addoit-function nil] 3] (yyc1) nil nil nil (x) nil)) function-documentation (advice--make-docstring 'yyc))

(a 123)

(yyc 1)
3

(symbol-name 'yy)
"yy"
(delete )


(defun yy-mul (a b)
  (* a b))
yy-mul

(defadvice yy-mul (before yy-mulbef 2 (a b c) activate)
(setq b (+ b c)))
yy-mul

(defadvice yy-mul (around yy-mularo 1 (a b c d) activate)
  (setq a (+ a b))
  (setq b (+ b c))
  ad-do-it)
yy-mul

yy-mul

yy-mul

yy-mul

yy-mul

yy-mul

(yy-mul 1 2 3)

96

152

64

64

64

nil

(defadvice yysub (before yysub-1)
  (setq x (1- x)))
yysub

(defun yysub (x y)
  (- x y))

(defun yydiv (x y)
(/ x y))
yydiv


(defun yy-nothing (x) nil)
yy-nothing

(defadvice yy-nothing (before yy-no1 activate)
  (/ 1 0))
yy-nothing

(defadvice yy-nothing (around yy-no2 protect activate)
  (print "Hello")
  ad-do-it)
yy-nothing

(defadvice yy-nothing (around yy-no3 last activate)
  (print "world")
  ad-do-it)
yy-nothing

(defadvice yy-nothing (after yy-no4 protect activate)
(print "!!!"))
yy-nothing

(yy-nothing 1)

"Hello"

"world"

"!!!"

(defun yy-int (x) (* x x 0.5))
(defadvice yy-int (before yy-int-x compile activate)
  (setq x (1+ x)))

(symbol-function 'yy-int)
#[128 "\300\301\302#\207" [apply ad-Advice-yy-int (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (* x x 0.5)) nil] 5 nil]

(defadvice yy-tni (before yy-int-x compile activate)
  (setq x (1+ x)))
yy-tni

(defun yy-tni (x) x)
yy-tni

(symbol-function 'yy-tni)
#[128 "\300\301\302#\207" [apply ad-Advice-yy-tni (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) x) nil] 5 nil]

(defun yy-tot (x y) (min x y))
yy-tot

(defadvice yy-tot (after yy-tot-1 disable)
  (cl-incf ad-return-value))
yy-tot

yy-tot

(yy-tot 1 2)
1

1

(ad-activate 'yy-tot)
nil

nil

(yy-tot 1 2)
1

1

(ad-enable-advice 'yy-tot 'after "yy-tot")
nil

nil

nil

(ad-activate 'yy-tot)
yy-tot

nil

nil

nil
(yy-tot 1 2)
2

1

1
(symbol-function 'yy-tot)
#[128 "\300\301\302#\207" [apply ad-Advice-yy-tot (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x y) (min x y)) nil] 5 nil]
(symbol-function 'yy-int)
#[128 "\300\301\302#\207" [apply ad-Advice-yy-int (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (* x x 0.5)) nil] 5 nil]


(defun a (x) (+ x 1))
a

(symbol-function 'a)
(closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (+ x 1))

(compile-defun a)nil

(symbol-function 'a)
(closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (+ x 1))

(byte-compile 'a)
#[257 "\211T\207" [] 2 "

(fn X)"]


(defun b (x) (+ x 1))

(byte-compile 'b)
#[257 "\211T\207" [] 2 "

(fn X)"]

b

(setq ad-default-compilation-action 'never)
never


(defadvice yy-i (before yy-i-1 activate compile)
  (+ 1 2))
yy-i

(defun yy-i () 1)
yy-i

(symbol-function 'yy-i)
#[128 "\300\301\302#\207" [apply ad-Advice-yy-i (closure (t) nil 1) nil] 5 nil]

(defun yy-k () 2)
yy-k

yy-k

(defadvice yy-k (before yy-k-1 activate)
  (+ 1 2))
yy-k

yy-k

(symbol-function 'yy-k)
#[128 "\300\301\302#\207" [apply ad-Advice-yy-k (closure (t) nil 2) nil] 5 nil]

ad-default-compilation-action
never

(byte-compile-arglist-vars)

(setq ad-default-compilation-action 'never)
never

(byte-code-function-p 'yy-k)
nil

(symbol-function 'yy-k)
#[128 "\300\301\302#\207" [apply ad-Advice-yy-k (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) nil 2) nil] 5 nil]

(byte-compile 'yy-k)
#[128 "\300\301\302#\207" [apply ad-Advice-yy-k (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) nil 2) nil] 5 nil]

(byte-code-function-p 'yy-k)
nil

(functionp 'yy-k)
t

(defun yy-p () nil)
yy-p

(defadvice yy-p (before yy-p-1 activate compile)
nil)


(byte-code-function-p 'yy-p)
nil
(defun a (x) x)
a

(byte-compile 'a)
#[257 "\207" [] 2 "

(fn X)"]

(symbol-function 'yy-x)
#[128 "\300\301\302#\207" [apply ad-Advice-yy-x (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) nil nil) nil] 5 nil]

(symbol-function 'ad-Advice-yy-x)
#[(ad--addoit-function) "\302	 \211)\207" [ad-return-value ad--addoit-function nil] 2]

(byte-code-function-p 'yy-x)
nil

(byte-code-function-p (symbol-function 'yy-x))
t

(eq (symbol-function 'yy-x) (symbol-function 'ad-Advice-yy-x))
nil





(setq ad-default-compilation-action 'always)
always

(defun yy-x () nil)
yy-x

(defadvice yy-x (before yy-x-1 activate compile)
  (+ 1 2))

(symbol-function 'yy-x)
#[128 "\300\301\302#\207" [apply ad-Advice-yy-x (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) nil nil) nil] 5 nil]

(functionp (symbol-function 'yy-x))
t

(byte-code-function-p (symbol-function 'yy-x))
t

nil

(setq ad-default-compilation-action 'never)
never

(defun yy-z () nil)
yy-z

(defadvice yy-z (before yy-z-1 activate)
(+ 1 2))
yy-z

(symbol-function 'yy-z)
#[128 "\300\301\302#\207" [apply ad-Advice-yy-z (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) nil nil) nil] 5 nil]

(byte-code-function-p (symbol-function 'yy-z))
t

(yy-z)
nil

nil
nil
nil

(byte-code-function-p 'ad-Advice-yy-z)
nil

(symbol-function 'ad-Advice-yy-z)
(lambda (ad--addoit-function) (let (ad-return-value) (+ 1 2) (setq ad-return-value (with-no-warnings ...)) ad-return-value))

(defadvice )

(setq ad-default-compilation-action 'never)

(defun yy-int (x) (* x x 0.5))
yy-int
(byte-code-function-p 'yy-int)
nil

(defadvice yy-int (before yy-int-x compile activate)
(setq x (1+ x)))


yy-int

(byte-code-function-p 'yy-int)
nil
(byte-code-function-p 'ad-Advice-yy-int)
nil

(byte-code-function-p (symbol-function 'ad-Advice-yy-int))
t
(byte-code )
#[(ad--addoit-function x) "\303	T
	!\211)\207" [ad-return-value x ad--addoit-function nil] 3]

(symbol-function 'yy-int)
#[128 "\300\301\302#\207" [apply ad-Advice-yy-int (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (* x x 0.5)) nil] 5 nil]

(ad-deactivate 'yy-int)
yy-int

(symbol-function 'yy-int)
(closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (* x x 0.5))

(defun foo (x)
"Add 1 to X."
(1+ x))
foo


(defadvice foo (after fg-cleanup prot act comp)
"Do some protected cleanup."
(print "Let's clean up now!"))
foo

foo

(symbol-function 'foo)
#[128 "\300\301\302#\207" [apply ad-Advice-foo (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) "Add 1 to X." (1+ x)) nil] 5 nil]

(symbol-function 'ad-Advice-foo)
#[(ad--addoit-function x) "\303\304\216
!))\207" [ad-return-value ad--addoit-function x nil #[nil "\300\301!\207" [print "Let's clean up now!"] 2]] 2]

(byte-code-function-p 'ad-Advice-foo)
nil

(setq ad-default-compilation-action 'maybe)
maybe

(symbol-function 'foo)
#[128 "\300\301\302#\207" [apply ad-Advice-foo (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) "Add 1 to X." (1+ x)) nil] 5 nil]

(byte-code-function-p 'ad-Advice-foo)
nil

(ad-deactivate 'foo)
foo

(symbol-function 'foo)
(closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) "Add 1 to X." (1+ x))

(defun bar (x) (+ 1 x))
bar

(defadvice bar (after bar-1 activate compile)
(print "Hello"))
bar


nil
(byte-code-function-p 'ad-Advice-bar)
nil

(ad-deactivate 'bar)
bar

(byte-code-function-p (symbol-function 'bar))
nil

(defun yy-i (x) x)
yy-i

(byte-compile 'yy-i)
#[257 "\207" [] 2 "

(fn X)"]

(symbol-function 'yy-i)
#[257 "\207" [] 2 "

(fn X)"]

(byte-code-function-p 'yy-i)
nil

nil

(setq ad-default-compilation-action 'never)
never

(defun yy-a () nil)
yy-a

(defadvice yy-a (before yy-a-1 activate)
1)
yy-a

(symbol-function 'ad-Advice-yy-a)
(lambda (ad--addoit-function) (let (ad-return-value) (setq ad-return-value (with-no-warnings ...)) ad-return-value))
(symbol-function 'yy-a)
#[128 "\300\301\302#\207" [apply ad-Advice-yy-a (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) nil nil) nil] 5 nil]

(defadvice yy-inc (before yy-inc-1 compile activate)
  (+ 1 21))
yy-inc

(defun yy-inc () nil)
yy-inc

(symbol-function 'ad-Advice-yy-inc)
(lambda (ad--addoit-function) (let (ad-return-value) (+ 1 21) (setq ad-return-value (with-no-warnings ...)) ad-return-value))


3x + 4y = 36
x + y = 10

y = 6
x = 4


(defun yy-add (a b) (+ a b))
yy-add

(defadvice yy-add (before yy-add-1 activate)
(cl-incf a)
(cl-incf b))
yy-add

yy-add

(yy-add 1 1)
4
(symbol-plist 'yy-add)

(ad-disable-advice)

(ad-enable-regexp)

(ad-activate-regexp )

(ad-de)

ad-default-compilation-action
never

(defun yy-e (x) (+ x 1))
yy-e

(defadvice yy-e (before yy-e-1 activate)
(setq x (+ x 1)))
yy-e

(symbol-function 'yy-e)
#[128 "\300\301\302#\207" [apply ad-Advice-yy-e (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (+ x 1)) nil] 5 nil]

(symbol-function 'ad-Advice-yy-e)
(lambda (ad--addoit-function x) (let (ad-return-value) (setq x (+ x 1)) (setq ad-return-value (with-no-warnings ...)) ad-return-value))


(ad-unadvise-all )

(ad-un)

(ad-remove-advice )

(defun yy (x) (+ x 1))
yy
(setq abc 1)
1

(defadvice yy-h (activation yy-hk)
  (cl-incf abc))
yy-h

yy

yy

(ad-activate 'yy)
nil

(print 123)


abc
1

(ad-deactivate 'yy)
nil

(ad-activate 'yy)
nil

abc
1

(defun yy-g (x) (+ x 1))
yy-g

abc
1

(defun yy-h (x) (+ x 1))
yy-h

abc
1

(symbol-function 'yy-h)
(closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (+ x 1))

abc
1
(cl-incf abc)
3

(ad-activate 'yy-h)
nil

abc
3

(ad-deactivate 'yy-h)
nil

()


(defun yy-ef (x) (+ x 1))
yy-ef

(ad-activate 'yy-ef)

(defadvice yy-ef (before yy-ef-1 activate)
  (ad-set-arg 0 (+ (ad-get-arg 0) 1)))
yy-ef

yy-ef

(yy-ef 1)
3

(ad-deactivate 'yy-ef)
yy-ef

(ad-activate 'yy-ef)
yy-ef

(defadvice yy-ef (activation yy-ef-2)
(cl-incf abc))
yy-ef

(ad-deactivate 'yy-ef)
yy-ef

abc
3

(ad-activate 'yy-ef)
yy-ef

abc
4

(setq abc 0)
0

(defun yy-ad (x) (+ x 1))
yy-ad

(defadvice yy-ad (activation yy-ad-a)
  (cl-incf abc))

(ad-has-any-advice 'yy-ad)
t

(ad-is-active 'yy-ad)
nil

(defadvice yy-ac (activation yy-ac-a)
  (cl-incf abc))
yy-ac

(defun yy-ac (x) (+ x 1))

(defadvice hhh (activation hhh-hook)
(cl-incf abc))
hhh

(load-file "1.el")
t

(hhh 1)
2

(setq abc 0)
0

(defadvice yy-yy (activation yy-yy-a)
  (cl-incf abc))
yy-yy

(defadvice yy-yy (deactivation yy-yy-d)
  (cl-decf abc 2))
yy-yy

(defadvice yy-yy (before yy-yy-nothing))
yy-yy

abc
0

(defun yy-yy (x) (+ x 1))
yy-yy

abc
1

(ad-deactivate 'yy-yy)
yy-yy

abc
-1

(ad-activate 'yy-yy)
yy-yy

abc
0

(ad-compil)

(advice-add)

(defun add1 (x) (+ x 1))
add1

(add-function :filter-args (symbol-function 'add1) (lambda (x) (list (+ x 1))))
#[128 "\300\302\301!\"\207" [apply (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (list ...)) (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (+ x 1)) nil] 5 nil]

(add1 1)

(defun add2 (x) (+ x 2))
add2

(add-function :filter-args
(symbol-function 'add2)
(lambda (x)
  (-map (lambda (x) (+ x 1))
	x)))
#[128 "\300\302\301!\"\207" [apply (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (-map ... x)) (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (+ x 2)) nil] 5 nil]

(add2 1)
4

(remove-function (symbol-function 'add2) (lambda (x) (+ x 1)))
nil

(add2 1)
4

(defun add3 (x) (+ x 3))
add3

(add-function
:filter-args
(symbol-function 'add3)
(lambda (x) (-map '1+ x))
'((name . "yyy")))
#[128 "\300\302\301!\"\207" [apply (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (-map ... x)) (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (+ x 3)) ((name . "yyy"))] 5 nil]

(add3 2)
6

(remove-function (symbol-function 'add3) "yyy")
(closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (+ x 3))

(add3 1)
4

(defun add3 (x) (+ x 3))
(add-function :filter-args
	      (symbol-function 'add3)
	      (lambda (x) (-map '1+ x))
	      '((name . yyy)))
#[128 "\300\302\301!\"\207" [apply (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (-map ... x)) (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (+ x 3)) ((name . yyy))] 5 nil]

(add3 1)
5

(remove-function (symbol-function 'add3) 'yyy)

(closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (+ x 3))

(add3 1)
4

(defun add4 (x) (+ x 4))
add4

(add-function :filter-args (symbol-function 'add4) (lambda (x) (-map '1+ x)))
#[128 "\300\302\301!\"\207" [apply (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (-map ... x)) (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (+ x 4)) nil] 5 nil]

(add4 1)
6

(remove-function (symbol-function 'add3) (lambda (x) (-map '1+ x)))
nil

(defun add5 (x) (+ x 5))
add5

add5

(setq lexical-binding nil)
nil

(symbol-function 'add5)
(lambda (x) (+ x 5))

(add-function :filter-args (symbol-function 'add5) (lambda (x) (-map '1+ x)))
#[128 "\300\302\301!\"\207" [apply (lambda (x) (-map ... x)) (lambda (x) (+ x 5)) nil] 5 nil]

(add5 1)
7

(remove-function (symbol-function 'add5) (lambda (x) (-map '1+ x)))
(lambda (x) (+ x 5))

(add3 1)
4
#'add5
add5

(add-function :filter-args (symbol-function 'add5) 'my-1+)
#[128 "\300\302\301!\"\207" [apply my-1+ (lambda (x) (+ x 5)) nil] 5 nil]

(add5 1)
7

(remove-function (symbol-function 'add5) 'my-1+)
(lambda (x) (+ x 5))

(add5 1)
6

(setq lexical-binding t)
t

(add-function :filter-args (symbol-function 'add3) 'my-1+)
#[128 "\300\302\301!\"\207" [apply my-1+ (closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (+ x 3)) nil] 5 nil]

(add3 1)
5

(remove-function (symbol-function 'add3) 'my-1+)
(closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (+ x 3))

(add3 1)
4


fact

(fact 10)
3628800

fact

(fact 10)
(1 2 3 4 5 6 7 8 9 10)


(-map '* '(1 2 3))
(1 2 3)

(defun fact (n)
  (cl-loop
   with x = 1
   for i from 1 to n
   do (setq x (* x i))
   finally return x))

(define-advice fact (:before (x) yy-fact)
(print (format "x is %s" x)))
fact@yy-fact

(fact 10)

"x is 10"
3628800

(defun yy-fact-1 (n)
(print (1+ n)))
yy-fact-1

(advice-add 'fact :after 'yy-fact-1)
nil

(fact 10)

"x is 10"

11
3628800


(defun yy-fact-ar (fun n)
  (funcall fun (+ n 1)))
yy-fact-ar

(advice-add 'fact :around 'yy-fact-ar)
nil

(fact 10)

"x is 11"

12
39916800

(defun yy-fact-fre (ret-v)
(+ 1 ret-v))

(advice-add 'fact :filter-return 'yy-fact-fre)

(fact 10)

"x is 11"

12
39916801

nil

(fact 10)

"x is 11"

12
39916800


"x is 11"

12

(advice-remove 'fact 'yy-fact-fre)
nil
(progn
(advice-remove 'fact 'yy-fact-1)
(advice-remove 'fact 'yy-fact-ar)
(advice-remove 'fact 'yy-fact-fre)
(advice-remove 'fact 'fact@yy-fact))
nil

nil
(defadvice)
(fact 10)
3628800


"x is 10"
3628800

(symbol-plist 'a)

(wo 2)


(put 'a 'wo 2)

org-timer-c

(add-hook 'org-after-todo-state-change-hook
	  'my-org-beep)
(my-org-beep)

(defun my-org-beep ()
  (when (string-equal org-state)))

(add-hook 'org-timer-stop-hook 'my-beep)
(my-beep)

(defun my-beep ()
  (play-sound-file "~/1.mp3"))

(my-beep)

(beep)
(bee)
org-timer-stop-hook
(my-beep)
(bee)
(trace-function 'my-beep)
nil
(bee)
(run-with-timer 5 1 'my-beep)
[nil 24863 31441 345833 1 my-beep nil nil 0]

(run-with-timer 5 nil 'my-beep)

aHR0cHM6Ly93d3cucGl4aXYubmV0L2FydHdvcmtzLzkwMzY3NjEy
(bee)

,
`(,@(list 1 2 3))
(1 2 3)
(bee)
(backquote-delay-process )
(bee)
(defadvice)
(defvar a 2)
(bee)
(let ((a 1))
  (eval 'a))
(bee)
(cdr (symbol-function 'inctwo))
(macroexpand-all '(inc2 a b) `((inc . ,(cdr (symbol-function 'inctwo)))))



(defmacro tet (a b c)
  (declare (indent 5))
  `(progn
     (print ,a)
     (print ,b)
     (print ,c)))

(let ((a 1))
  (tet 1 2 3))


(symbol-plist 'tet)
(lisp-indent-function 5)


(require 'cl-macs)

(defmacro tet-2 (a b c)
  (declare (indent 1))
  `(progn (listt ,a ,b ,c)))

(tet-2 1
  2
  3)

(tet 1
    2
    3
    )

(require 'cycle-buffer)

(featurep 'cycle-buffer)

(defun a (x y)
""
(+ x y))

(defun b (x y z)
(declare (indent 2))
(+ x y z))

(ind_defun 1
  )

(b
    1
    2
  3)

lisp-body-indent
2



(defun
    a
    (x y z)
  1
  2
  3)

(a 1 2 3)
3

(defun c (x y z a)
  (+ a x y z))
c

b

(b
    1
    2
  3
  4)

(symbol-plist 'c)

(lisp-indent-function 2)

(closure (t) (x y z a) (+ a x y z))

(c
 1
 2
 3
 4)

(defun cecc (x y z a )
  (list x y z a))
cecc
(symbol-plist 'cecc)
nil

(cecc
 1
 2
 3
 4
 )
(1 2 3 4)

(cecc  1
       2
       3
       4)


(defun cedd (x y z a)
  (declare (indent 1))
  (list x y z a))
cedd

(cedd
    1
  2
  3
  4)
(symbol-plist 'cedd)
(lisp-indent-function 1)


(defun ceee(x y z a)
  (declare (indent 3))
  (list x))
ceee

(ceee
    1
    2
    3
  4)

(let
    ((a 1))
  )

(defun ind_dd (a b c d)
  (declare (indent defun))
  (list a b c d))
ind_dd

(ind_num 1 2
	 3
  4
  5)

(ind_num
    1
    2
    3
  4
  5)

(ind_num 1
    2
    3
  4
  5)

(ind_syn
    1
  2
    3
  4)

(ind_syn2
    1
  2
    3
  4)
(ind_syn3
 1
  2
   3
    4)
(line-number-at-pos (point))

(defun ind_ind2 (pos state)
  (let ((current-line-delta (- (line-number-at-pos pos)
			       (line-number-at-pos (nth 1 state)))))
    (if (zerop (% current-line-delta 2)) '(2) '(4))))

(ind_syn2
    1
  2
    3
  4)
(ind_syn3
 1
  2
   3
    4)
(line-number-at-pos (point))

(defun ind_ind2 (pos state)
(let ((current-line-delta (- (line-number-at-pos pos)
			     (line-number-at-pos (nth 1 state)))))
  (if (zerop (% current-line-delta 2)) '(2) '(4))))

(setq lexical-binding t)
(defmacro foo (a)
  (list 'setq (eval a) t))
(setq a 'c)

(foo a)

(setq b 'd)
(foo b)

d
t

(setq lexical-binding nil)
nil

(defmacro foo (a)
(list 'setq (eval a) t))
foo

(symbol-function 'foo)
(macro lambda (a) (list 'setq (eval a) t))

(setq lexical-binding t)
t

(defmacro foo-1 (a)
(list 'setq (eval a) t))
foo-1

(symbol-function 'foo-1)
(macro closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc yao yyy a a widget-example-repeat yycon yylst yyl1 yyl2 yyal yyco yyrad yyfun yyfun-val yyset1 yypt a b s1 s2 s3 s4 w1 w2 w3 w1 w2 w3 yy-test a t) (a) (list 'setq (eval a) t))

yyy
(setq lexical-binding t)
(require 'thunk)
(thunk-delay)

(cl-callf)
(setq a 1)
(cl-callf (lambda (x y z) (+ x 1 y z)) a 1 2)

(cl-callf )

[]
[]

()
nil

(cl-destructuring-bind
(a b c)
(list 1 2 3)
(+ a b c))
6


"^(def\\(un\\|macro\\) cl-{1,1}.+$"

'cl-destructuring-bind
'cl-symbol-macrolet

(defmacro add (x) `(+ 1 ,x))

(macroexpand '(cl-symbol-macrolet ((add (lambda (x) `(+ 1 ,x))))
		(+ 1 2 (add 2))))
(+ 1 2 (+ 1 2))

(setq a 1)
1

(cl-callf (lambda (x y z) (+ x y z)) a 2 3)
6

a
6

(macroexp--accumulate )

(macroexp-parse-body '(lambda (x) (declare (indent 1))(+ x 1)))
(nil lambda (x) (declare (indent 1)) (+ x 1))

(macroexp-let* '((a 1)) '(let* ((x 2)) (+ x 2)))
(let* ((a 1) (x 2)) (+ x 2))

(function '(lambda (x) (1+ x)))
'(lambda (x) (1+ x))

(lambda (x) (1+ x))

(closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (x) (1+ x))

(setq lexical-binding nil)
nil

(setq lexical-binding t)
t
lexical-binding
t

(let ((a 1))
  (defun x() a))
x

(x)
6
(defalias 'x (let ((a 1)) (lambda () a)))
x

x

(x)
6
a
6


(fset 'x (let ((a 1)) (lambda () a)))
(closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc yao yyy a a widget-example-repeat yycon yylst yyl1 yyl2 yyal yyco yyrad yyfun yyfun-val yyset1 yypt a b s1 s2 s3 s4 w1 w2 w3 w1 w2 w3 yy-test a t) nil a)

(x)
6

(funcall (let ((a 1))
	   (lambda () a)))
6
lexical-binding
t

(special-variable-p 'a)
nil

(defun x (a)
(let ((b a))
  (lambda (c) (+ c b))))
x

(funcall (x 1) 2)

(symbol-function 'x)
(closure (y yq yp ys yg a ty stream-null stream-null yyy cc cc ...) (a) (let ((b a)) #'(lambda ... ...)))

(x 1)
(closure ((a . 1) y yq yp ys yg a ty stream-null stream-null yyy cc ...) (c) (+ c b))

(cl-labels)
(macroexpand
'(cl-labels ((fact (n) (if (zerop n) 1 (* n (fact (- n 1))))))
   (fact 10)))
(let (--cl-fact--) (setq --cl-fact-- #'(lambda (n) (if (= 0 n) 1 (* n (funcall --cl-fact-- (- n 1)))))) (funcall --cl-fact-- 10))

3628800
