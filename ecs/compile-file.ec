&version 2

&attach
lisp >unb>compiler

(cf "compincl.lisp")

(load "multicsm")

(cf "lmdcls.lisp")

(mapc 'load '("mormac" "defopt" "defcal" "maxmac" "mforma" #-Multics "mrgmac" "rzmac" #-Multics "displm" "ratmac" "mhayat" "mopers"))
;(mapc 'load '("mormac" "maxmac"))

(genprefix "&1.")

(cf "&1")

(quit)
&detach
