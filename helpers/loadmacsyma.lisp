(%include runtime)
(%include library)
(%include format)

(*rset t)
(nouuo t)
(sstatus uuolinks)

(load "compincl")

(load "multicsm")

(load "lmdcls")

(load "mormac")

(load "defopt")

(load "defcal")

(load "maxmac")

(load "mforma")

(load "mrgmac")

(load "rzmac")

(load "displm")

(load "ratmac")

(load "mhayat")

(load "mopers")

(load "transm")


(mapc 'load '("opers" "utils" "sumcon" "sublis" "runtim" "merror" "mformt" "mutils" "outmis"))

(load "displa")
(load "nforma")
(load "ldisp")
(load "grind")
(load "nparse")
(load "system")
(load "mload")
(load "suprv")
(load "mlisp")
(load "mmacro")
(load "buildq")
(load "comm")
(load "comm2")
(load "simp")
(load "float")
(load "csimp")
(load "csimp2")
(load "zero")
(load "logarc")
(load "rpart")
(load "rat3a")
(load "rat3b")
(load "rat3d")
(load "rat3c")
(load "rat3e")
(load "nrat4")
(load "ratout")
(load "lesfac")
(load "factor")
(load "algfac")
(load "nalgfa")
(load "ufact")
(load "result")
(load "spgcd")
(load "inmis")
(load "db")
(load "compar")
(load "askp")
(load "sinint")
(load "sin")
(load "risch")
(load "defint")
(load "residu")

(load "trigi")
(load "trigo")
(load "trgred")

(load "mat")
(load "matrix")

(load "sprdet")
(load "newinv")
(load "linnew")
(load "newdet")

(load "numerm")
(load "bessel")
(load "ellipt")
(load "numer")
(load "intpol")
(load "rombrg")

(load "matcom")
(load "matrun")
(load "nisimp")

(load "tlimit")
(load "limit")

(load "solve")
(load "psolve")
(load "algsys")
(load "polyrz")
(load "cpoly")

(load "transl")
(load "transs")
(load "trans1")
(load "trans2")
(load "trans3")
(load "trans4")
(load "trans5")
(load "transf")
(load "troper")
(load "trutil")
(load "trmode")
(load "trdata")
(load "trpred")
(load "transq")
(load "acall")
(load "fcall")
(load "evalw")
(load "trprop")
(load "mdefun")

(load "scs")
(load "asum")
(load "fortra")
(load "optim")
(load "array")
(load "mdot")
(load "irinte")
(load "series")
(load "hayat")
(load "schatc")
(load "numth")
(load "laplac")
(load "pade")
(load "homog")
(load "combin")
(load "mstuff")
(load "specfn")

(load "descri")
(load "test-batch")

;(*rset t)
;(nouuo t)

(*rset nil)
(nouuo nil)

(sstatus uuolinks)

(progn (sstatus toplevel '(continue)) (save "macsyma"))
