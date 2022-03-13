&version 2

&attach
lisp >unb>compiler

(cf "compincl.lisp")

(load "multicsm")

(cf "lmdcls.lisp")

;(mapc 'load '("mormac" "maxmac"))
(mapc 'load '("mormac" "defopt" "defcal" "maxmac" "mforma" #-Multics "mrgmac" "rzmac" #-Multics "displm" "ratmac" "mhayat" "mopers"))

(mapc 'cf '("opers.lisp" "utils.lisp" "sumcon.lisp" "sublis.lisp" "runtim.lisp" "merror.lisp" "mformt.lisp" "mutils.lisp" "outmis.lisp"))


(quit)
