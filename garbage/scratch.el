(sequencep '(1 2 3))
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
"c:/Users/yiyue/Desktop/scratch.el"

buffer-file-name
"c:/Users/yiyue/Desktop/scratch.el"

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

(setq F 0.5
      l 0.25
      W 0.06
      T 0.003
      E 70e5
      K 2.1
      )
2.1

(setq M (* F l 0.5))
0.0625

(setq Iz (/ (* W T T T) 12.0))
1.3499999999999997e-10

(setq epl (/ (* 0.5 T M) E Iz))
(* 0.09920634920634923 K 120)
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

(error "That is an error -- try something else")
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
  "Hello every body
I'm include-yy

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
  (message "display-buffer called with args %S" args)
  (let ((res (apply orig-fun args)))
    (message "display-buffer returned %S" res)
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
(advice-eval-interactive-spec "r\nP")
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
("^Exit the snippet first!$" "^Invalid `winum-scope': .$" "^Got a dead window .$" "^No window numbered .$" beginning-of-line beginning-of-buffer end-of-line end-of-buffer end-of-file buffer-read-only file-supersession mark-inactive user-error)

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
"c:/Users/yiyue/myself/software/emacs/share/emacs/27.1/lisp/files.elc"

(symbol-plist 'find-file)
(group-documentation "Finding files." custom-group ((directory-abbrev-alist custom-variable) (find-file-existing-other-name custom-variable) (find-file-visit-truename custom-variable) (revert-without-query custom-variable) (find-file-run-dired custom-variable) (find-directory-functions custom-variable) (file-name-at-point-functions custom-variable) (find-file-hook custom-variable) (enable-local-variables custom-variable) (enable-local-eval custom-variable) (confirm-nonexistent-file-or-buffer custom-variable) (large-file-warning-threshold custom-variable) ...) event-symbol-element-mask (find-file 0) event-symbol-elements (find-file) modifier-cache ((0 . find-file)))

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

(read-from-minibuffer "Hello:-> " "asdf" nil nil '(a b c))
(read-from-minibuffer
 (concat
  (propertize "Bold" 'face '(bold default))
  (propertize " and normal: " 'face '(default))))
"1231231213123"

(propertize "abc" 'face '(bold default))
:#("abc" 0 3 (face (bold default)))



""

"asdf"

(+ 1 2)

asdf

(read-regexp "Hello")
"123"

(read-regexp "(a|b)")
"(a)|(b)"


(eval-minibuffer "w")
3

(read-passwd "QQ")
"12321312312312"

(execute-kbd-macro )
C-x onil

onil

(defun yy-1 (x) (interactive "P") (princ x))
yy-1

yy-1

(defun yy-2 (x y) (interactive "P\nbFib: ") (+ x y))
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

(defcustom ty '((1 . 2)) "Hello"
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
(closure ((cps-state-atom-352 closure #1 nil (setq cps-current-value-313 ...)) (cps-state-let*-351 closure #1 nil (progn ... ...)) (cps-state-atom-350 closure #1 nil (setq cps-current-value-313 ...)) (cps-state-let*-349 closure #1 nil (progn ... ...)) (cps-state-atom-348 closure #1 nil (setq cps-current-value-313 ...)) (cps-state-let*-346 closure #1 nil (progn ... ...)) (cps-state-atom-345 closure #1 nil (setq cps-current-value-313 ...)) (cps-state-let*-343 closure #1 nil (progn ... ...)) (cps-state-while-331 closure #1 nil (progn ...)) (cps-state-atom-342 closure #1 nil (setq cps-current-value-313 ...)) (cps-state-iter-yield-340 closure #1 nil (progn ... ...)) (cps-state-after-yield-339 closure #1 nil (setq cps-current-state-314 cps-state-let*-338)) ...) (op value) (cond ((eq op :stash-finalizer) (setq cps-iterator-finalizer-353 value)) ((eq op :get-finalizer) cps-iterator-finalizer-353) ((eq op :close) (let ... ... ...)) ((eq op :next) (setq cps-current-value-313 value) (let ... ...)) (t (error "unknown iterator operation %S" op))))

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
(closure ((cps-state-atom-426 closure #1 nil (setq cps-current-value-385 ...)) (cps-state-let*-425 closure #1 nil (progn ... ...)) (cps-state-atom-424 closure #1 nil (setq cps-current-value-385 ...)) (cps-state-let*-423 closure #1 nil (progn ... ...)) (cps-state-atom-422 closure #1 nil (setq cps-current-value-385 ...)) (cps-state-let*-420 closure #1 nil (progn ... ...)) (cps-state-atom-419 closure #1 nil (setq cps-current-value-385 ...)) (cps-state-let*-417 closure #1 nil (progn ... ...)) (cps-state-while-405 closure #1 nil (progn ...)) (cps-state-atom-416 closure #1 nil (setq cps-current-value-385 ...)) (cps-state-iter-yield-414 closure #1 nil (progn ... ...)) (cps-state-after-yield-413 closure #1 nil (setq cps-current-state-386 cps-state-let*-412)) ...) (op value) (cond ((eq op :stash-finalizer) (setq cps-iterator-finalizer-427 value)) ((eq op :get-finalizer) cps-iterator-finalizer-427) ((eq op :close) (let ... ... ...)) ((eq op :next) (setq cps-current-value-385 value) (let ... ...)) (t (error "unknown iterator operation %S" op))))

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
      (let ((fEvaled nil)
	    (theValue nil))
	(lambda ()
	  (if fEvaled theValue
	    (progn
	      (print "HHH")
              (setq theValue (+ 1 2))
              (setq fEvaled t)
              theValue)))))

a
a
(closure ((theValue) (fEvaled) y yq yp ys yg a ty t) nil (if fEvaled theValue (progn (print "HHH") (setq theValue ...) (setq fEvaled t) theValue)))

(a)
3


"HHH"
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
	(t (message "HEllo")))
      )

(error-message-string '(arith-error))
(setq debug-on-error t)
t

(define-error 'yy "yy's" 'error)

(symbol-plist 'yy)

(member 'error-conditions (symbol-plist 'wrong-number-of-arguments))
(error-conditions (wrong-number-of-arguments error) error-message "Wrong number of arguments")

(signal 'yy '(1 2 3))

(signal 'wrong-number-of-arguments '(x  y))
(member 'error-conditions (symbol-plist 'yy))

(define-error 'new-error "A new error" 'my-own-errors)

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


