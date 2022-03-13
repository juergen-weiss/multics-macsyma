;; -*- Mode: Lisp; Lowercase: True -*-

;; runtime.incl.lisp - Loads lisp_runtime_ into the interpreter environment.
;; Written:  October 1982 by Carl Hoffman

(eval-when (eval compile)
  (or (status feature runtime)
      (load ">ldd>include>lisp_runtime_")))

(declare (*expr fboundp fmakunbound fsymeval fset
	      ldb dpb
	      firstn butlast nbutlast
	      mem find-position-in-list ass rassq rassoc
	      circular-list-last)
         (*lexpr make-list rem remq remove symbolconc del))
