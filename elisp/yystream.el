;;; yystream.el --- sicp's stream implemented in elisp -*- lexical-binding: t -*-

;; Author: include-yy
;; Maintainer: include-yy
;; Version: 0.1
;; Package-Requires: (thunk cl-lib)
;; Homepage: see https://github.com/include-yy 's gists
;; Keywords: thunk


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; it's totally a toy without any except check
;; just enjoy it
;; 2021-3-27, 17:42 utf+8

;;; Code:

(require 'thunk)
(require 'cl-lib)

(defconst yystream-null nil
  "the nil value of yystream")

(defun yystream-nullp (s)
  "if s is nil, return t"
  (null s))

(defun yystream-car (s)
  "get the car part of s
if s is nil, then return nil"
  (if (yystream-nullp s) yystream-null
    (car s)))

(defun yystream-cdr (s)
  "get the cdr part of s
if s is nil, then return nil"
  (if (yystream-nullp s) yystream-null
    (thunk-force (cdr s))))

(defmacro yystream-cons (a b)
  "construct stream's cons cell"
  `(cons ,a (thunk-delay ,b)))

(defun yystream-ref (s n)
  "get the nth value of stream s
n must be non-zero, and not exceed the length of s minus 1"
  (cl-loop for i from 0 to (- n 1)
	   do (setq s (yystream-cdr s)))
  (yystream-car s))

(defun yystream-map (func &rest ss)
  "map func to ss's each element
ss's length must be consistent with func's arg number"
  (if (yystream-nullp (car ss)) yystream-null
    (yystream-cons (apply func (mapcar 'yystream-car ss))
		   (apply 'yystream-map func (mapcar 'yystream-cdr ss)))))

(defun yystream-filter (func s)
  "use func filter elements in s
and collect the one satisfy func"
  (if (yystream-nullp s) yystream-null
    (if (funcall func (yystream-car s))
	(yystream-cons (yystream-car s) (yystream-filter func (yystream-cdr s)))
      (while (and (not (yystream-nullp s))
		  (not (funcall func (yystream-car s))))
	(setq s (yystream-cdr s)))
      (if (yystream-nullp s) yystream-null
	(yystream-cons (yystream-car s) (yystream-filter func (yystream-cdr s)))))))

(defun yystream-foldl (func v0 s &optional len)
  "just like fold-left, but use yystream other than list
func must be of the form (lambda (accu se))
where `se' is each element in stream and `accu' is the accumulate value
if len is present, this function just fold the first len element of stream"
  (if len
      (cl-loop for i from 0 to (- len 1)
	       do (setq v0 (funcall func v0 (yystream-car s)))
	       do (setq s (yystream-cdr s))
	       finally return v0)
    (while s
      (setq v0 (funcall func v0 (yystream-car s)))
      (setq s (yystream-cdr s)))
    v0))

(defun yystream-make-const (c)
  "create infinite stream with c as each element"
  (yystream-cons c (yystream-make-const c)))

(defun yystream-scale (n stm)
  "scale stream `stm' with n "
  (yystream-map (lambda (x) (* x n)) stm))

(defun yystream-add (s1 s2)
  "add two yystream, get a0 + b0, a1 + b1, a2 + b2 ......"
  (yystream-map '+ s1 s2))

(defun yystream-sub (s1 s2)
  "sub two yystream, get a0 - b0, a1 - b1, a2 - b2 ......"
  (yystream-map '- s1 s2))

(defun yystream-mul (s1 s2)
  "multiply two yystream, get a0 * b0, a1 * b1, a2 * b2 ......"
  (yystream-map '* s1 s2))

(defun yystream-div (s1 s2)
  "div two yystream, get a0/b0, a1/b1, a2/b2 ......
be caution that each element in s2 must be nonzero"
  (yystream-map '/ s1 s2 (yystream-make-const 1.0)))

(defun yystream-partial-sums (s)
  "get partial sums of s, get a0, a0 + a1, a0 + a1 + a2 ......"
  (yystream-cons (yystream-car s)
		 (yystream-add (yystream-partial-sums s)
			       (yystream-cdr s))))

(defun yystream-finite-difference (s)
  "get finite difference(also called `cha-fen' in chinese) of s
just like a0, a1 - a0, a2 - a1, a3 - a2 ......"
  (yystream-cons (yystream-car s)
		 (yystream-sub (yystream-cdr s) s)))

(defun yystream-integrate (s)
  "get the taylor series of int(s), so it's correspond to a0, 1/2a1, 1/3a2, ......
and with integrate constant"
  (yystream-div s yystream-std-natural-seq))

(defun yystream-derivative (s)
  "get the taylor series of d(s), so it's correspont to a1, 2a2, 3a3, 4a4 ......"
  (yystream-mul (yystream-cdr s) yystream-std-natural-seq))

(defun yystream-mul-series (s1 s2)
  "this function gives the taylor series of conv(s1, s2)"
  (yystream-cons (* (yystream-car s1) (yystream-car s2))
		 (yystream-add (yystream-mul-series s1 (yystream-cdr s2))
			       (yystream-scale (yystream-car s2)
					       (yystream-cdr s1)))))

(defun yystream-inv-series (s)
  "get the inverse taylor series of s"
  (yystream-add (yystream-cons (/ 1.0 (yystream-car s)) yystream-std-zero-seq)
		(yystream-scale (/ -1.0 (stream-car s))
				(yystream-mul-series (yystream-cons 0 (yystream-cdr s))))))

(defun yystream-div-series (s1 s2)
  "get the taylor series of conv(s1, 1/s2)"
  (yystream-mul-series s1 (yystream-inv-series s2)))


(defconst yystream-std-zero-seq (yystream-cons 0 yystream-std-zero-seq)
  "0, 0, 0, 0, 0 ...")

(defconst yystream-std-delta-seq (yystream-cons 1 yystream-std-zero-seq)
  "1, 0, 0, 0, 0 ...")

(defconst yystream-std-one-seq (yystream-cons 1 yystream-std-one-seq)
  "1, 1, 1, 1, 1 ...")

(defvaralias 'yystream-std-unit-seq 'yystream-std-one-seq
  "1, 1, 1, 1, 1 ...
The same as yystream-std-one-seq")

(defconst yystream-std-natural-seq (yystream-cons 1
						  (yystream-add  yystream-std-unit-seq
								 yystream-std-natural-seq))
  "1, 2, 3, 4, 5 ...")

(provide 'yystream)

;;; yystream.el ends here
