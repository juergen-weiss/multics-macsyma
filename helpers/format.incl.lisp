;; -*- Mode: Lisp; Lowercase: True -*-

;; format.incl.lisp - Loads lisp_format_ into the interpreter environment.
;; Written:  October 1982 by Carl Hoffman

(eval-when (eval compile)
  (or (status feature format)
      (load ">ldd>include>lisp_format_")))

(declare (*lexpr format ferror))
