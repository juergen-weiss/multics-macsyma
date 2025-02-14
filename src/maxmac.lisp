;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp; Package: Macsyma -*- ;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macsyma-module maxmac macro)

;; This file contains miscellaneous macros used in Macsyma source files.
;; This file must run and compile in PDP10 Lisp, Multics Lisp, Franz Lisp,
;; and LMLisp.

;; General purpose macros which are used in Lisp code, but not widely enough
;; accepted to be a part of Lisp systems.

;; For evaluable declarations placed in macro files. This is a DWIM form
;; saying "evaluate this form if you think it matters." If we tried hard
;; we could come up with a better way to actually do it. -gjc

(defmacro for-declarations (&rest l)
  `(map-eval-for-declarations ',l))

(defun map-eval-for-declarations (l) (mapc #'eval-for-declarations l))

(defun eval-for-declarations (form)
  (if (and (not (atom form))
	   (symbolp (car form))
	   ;; we want an fboundp which gives T for special forms too.
	   (OR (fboundp (car form))
	       #+NIL (SI:MACRO-DEFINITION (CAR FORM))
	       #+NIL (EQ (CAR FORM) 'SPECIAL)))
      (eval form)))

;; All these updating macros should be made from the same generalized
;; push/pop scheme as I mentioned to LispForum. As they are defined now
;; they have inconsistent return-values and multiple-evaluations of
;; arguments. -gjc

(DEFMACRO ADDL (ITEM LIST)
	  `(OR (MEMQ ,ITEM ,LIST) (SETQ ,LIST (CONS ,ITEM ,LIST))))

#-Multics (PROGN 'COMPILE

#-NIL
(DEFMACRO INCREMENT (COUNTER &OPTIONAL INCREMENT)
  (IF INCREMENT
      `(SETF ,COUNTER (+ ,COUNTER ,INCREMENT))
      `(SETF ,COUNTER (1+ ,COUNTER))))

#-NIL
(DEFMACRO DECREMENT (COUNTER &OPTIONAL DECREMENT)
  (IF DECREMENT
      `(SETF ,COUNTER (- ,COUNTER ,DECREMENT))
      `(SETF ,COUNTER (1- ,COUNTER))))

(DEFMACRO COMPLEMENT (SWITCH) `(SETF ,SWITCH (NOT ,SWITCH)))

) ;; End of Lispm conditionalization.


;; 'writefilep' and 'ttyoff' are system independent ways of expressing
;; the Maclisp ^R and ^W.
;; In Franz Lisp, we make writefilep equivalent to ptport, which isn't
;; exactly correct since ptport is not just a boolean variable.  However
;; it works in most cases.  
;;
(eval-when (compile eval load)
   (defvar writefilep #-Franz '^R #+Franz 'ptport)
   (defvar ttyoff    '^W))

;; (IFN A B) --> (COND ((NOT A) B))
;; (IFN A B C D) --> (COND ((NOT A) B) (T C D))
;; (IFN A B) is equivalent to (OR A B) as (IF A B) is equivalent to (AND A B).

(DEFMACRO IFN (PREDICATE THEN . ELSE)
	  (COND ((NULL ELSE) `(COND ((NOT ,PREDICATE) ,THEN)))
		(T `(COND ((NOT ,PREDICATE) ,THEN) (T . ,ELSE)))))

(DEFMACRO FN (BVL &REST BODY)
	  `(FUNCTION (LAMBDA ,BVL . ,BODY)))

;; Like PUSH, but works at the other end.

(DEFMACRO TUCHUS (LIST OBJECT)
	  `(SETF ,LIST (NCONC ,LIST (NCONS ,OBJECT))))

;; Copy a single cons, the top level and all levels (repectively) of a piece of
;; list structure.  Something similar for strings, structures, etc. would be
;; useful.  These functions should all be open-coded subrs.

(DEFMACRO COPY-CONS (CONS)
  (IF (ATOM CONS)
      `(CONS (CAR ,CONS) (CDR ,CONS))
      (LET ((VAR (GENSYM)))
	   `(LET ((,VAR ,CONS)) `(CONS (CAR ,VAR) (CDR ,VAR))))))

(DEFMACRO COPY-TOP-LEVEL (LIST) `(APPEND ,LIST NIL))
(DEFMACRO COPY-ALL-LEVELS (LIST) `(SUBST NIL NIL ,LIST))

;; Old names kept around for compatibility.

(DEFMACRO COPY1* (LIST) `(APPEND ,LIST NIL))
(DEFMACRO COPY1 (LIST) `(APPEND ,LIST NIL))
#-Franz
(DEFMACRO COPY (LIST) `(SUBST NIL NIL ,LIST))

;; Use this instead of GETL when looking for "function" properties,
;; i.e. one of EXPR, SUBR, LSUBR, FEXPR, FSUBR, MACRO.
;; Use FBOUNDP, FSYMEVAL, or FMAKUNBOUND if possible.

(DEFMACRO GETL-FUN (FUN L)
	  #+MacLisp `(GETL ,FUN ,L)
	  #+LISPM   `(GETL-LM-FCN-PROP ,FUN ,L)
	  #+Franz   `(GETL-FRANZ-FCN-PROP ,FUN ,L)
	  #+NIL     `(GETL-NIL-FCN-PROP ,FUN ,L)
	  )

;; Non-destructive versions of DELQ and DELETE.  Already part of NIL
;; and LMLisp.  These should be rewritten as SUBRS and placed
;; in UTILS.  The subr versions can be more memory efficient.

#-(OR Lispm NIL Multics)
(DEFMACRO REMQ (ITEM LIST &OPTIONAL (COUNT () COUNTING?))
	  (IF COUNTING? `(DELQ ,ITEM (APPEND ,LIST NIL) ,COUNT)
	      `(DELQ ,ITEM (APPEND ,LIST NIL))))

#-(OR Lispm NIL Multics)
(DEFMACRO REMOVE (ITEM LIST &OPTIONAL (COUNT () COUNTING?))
	  (IF COUNTING? `(DELETE ,ITEM (APPEND ,LIST NIL) ,COUNT)
	      `(DELETE ,ITEM (APPEND ,LIST NIL))))

#-Lispm (DEFMACRO CATCH-ALL (FORM) `(*CATCH NIL ,FORM))

;; (EXCH A B) exchanges the bindings of A and B
;; Maybe it should turn into (PSETF A B B A)?

(DEFMACRO EXCH (X Y) `(SETF ,X (PROG1 ,Y (SETF ,Y ,X))))

;; These are here for old code only.
;; Use FIFTH rather than CADDDDR.  Better, use DEFSTRUCT.

#-Franz (DEFMACRO CADDADR (X) `(CAR (CDDADR ,X)))
#-Franz (DEFMACRO CADDDDR (X) `(CAR (CDDDDR ,X)))

;; The following is a bit cleaner than the kludgy (PROGN 'COMPILE . <FORMS>)

(DEFMACRO COMPILE-FORMS (&REST <FORMS>) `(PROGN 'COMPILE . ,<FORMS>))


;; The following macros pertain only to Macsyma.

;; Widely used macro for printing error messages.  We should be able
;; to come up with something better.  On large address space systems
;; this should signal -- hack later.  Soon to be flushed in favor
;; of new Macsyma error system.  Yea!

;; Obsolete.  Use MERROR.

(DEFMACRO ERLIST (MESSAGE)
  (ERROR "ERLIST is obsolete, all calls to it have been removed, so where
	 did you dig this one up loser?" message))

;; All functions are present on non-autoloading systems.  Definition
;; for autoloading systems is in SUPRV.

#-PDP10
(DEFMACRO FIND-FUNCTION (FUNCTION) NIL)

;; Facility for loading auxilliary macro files such as RATMAC or MHAYAT.
;; Global macro files are loaded by the prelude file.

#+LISPM (DEFUN MACRO-DIR (X) (FORMAT NIL "LMMAXQ;~A QFASL" X))
#+PDP10 (DEFUN MACRO-DIR (X) `((LIBMAX) ,X))
#+Franz (defun macro-dir (x)  (cond ((cdr (assoc x '((rzmac  . "rz//macros")
						     (mhayat . "rat//mhayat")
						     (ratmac . "rat//ratmac")))))
				    (t (concat "libmax//" x))))


(comment Sample definition only on
	 ITS   see "LIBMAX;MODULE"
	 LISPM see "LMMAX;SYSDEF"
	 NIL   see   "VAXMAX;VAXCL"
	 Multics see "???"
	 Franz see "/usr/lib/lisp/machacks.l"
()
(defmacro macsyma-module (name &rest options)
  (maybe-load-macros options)
  (maybe-load-declarations options)
  `(progn 'compile
	  (print '(loading ,name) msgfiles)
	  (defprop ,name t loaded?)
	  ,@(maybe-have-some-runtime-options options)))
)

;; Except on the Lisp Machine, load the specified macro files.
;; On the Lisp Machine, the DEFSYSTEM facility is used for loading
;; macro files, so just check that the file is loaded. This is
;; a useful error check, has saved a lot of time since Defsystem
;; is far from fool-proof. See LMMAX;SYSDEF for the Lispm
;; definition of MACSYMA-MODULE.

#+LISPM
(DEFUN LOAD-MACSYMA-MACROS-AT-RUNTIME (&REST L)
  (MAPCAR #'(LAMBDA (X)
	      (IF (GET X 'MACSYMA-MODULE)
		   X 
		   (FERROR NIL "Missing Macsyma macro file -- ~A" X)))
	  L))
#-LISPM
(DEFUN LOAD-MACSYMA-MACROS-AT-RUNTIME (&REST L)
  (MAPCAR #'(LAMBDA (X)
	      (OR (GET X 'VERSION) (LOAD (MACRO-DIR X)))
	      (LIST X (GET X 'VERSION)))
	  L))

(DEFMACRO LOAD-MACSYMA-MACROS (&REST MACRO-FILES)
  `(COMMENT *MACRO*FILES*
	    ,(APPLY #'LOAD-MACSYMA-MACROS-AT-RUNTIME MACRO-FILES)))

#+Multics
(defmacro find-documentation-file (x)
  (cond ((eq x 'manual)
	 `(let ((filep (probef (list (catenate macsyma-dir ">documentation")
				     "macsyma.manual"))))
	    (cond (filep filep)
		  (t (error "Cannot find the Macsyma manual")))))
	((eq x 'manual-index)
	 `(let ((filep (probef (list (catenate macsyma-dir ">documentation")
				     "macsyma.index.lisp"))))
	    (cond (filep filep)
		  (t (error "Cannot find the Macsyma manual index")))))
	(t (error "Unknown documentation: " x))))

#+Multics
(defmacro load-documentation-file (x)
  `(load (find-documentation-file ,x)))

;; Used to temporarily bind contexts in such a way as to not cause
;; the context garbage collector to run. Used when you don't want to
;; stash away contexts for later use, but simply want to run a piece
;; of code in a new context which will be destroyed when the code finishes.
;; Note that this code COULD use an unwind-protect to be safe but since
;; it will not cause out and out errors we leave it out.

(defmacro with-new-context (sub-context &rest forms)
  `(let ((context (context ,@sub-context)))
     (prog1 ,@forms
	    (context-unwinder))))


;; For creating a macsyma evaluator variable binding context.
;; (MBINDING (VARIABLES &OPTIONAL VALUES FUNCTION-NAME)
;;    ... BODY ...)

(DEFMACRO MBINDING (VARIABLE-SPECIFICATION &REST BODY &AUX (TEMP (GENSYM)))
  `(LET ((,TEMP ,(CAR VARIABLE-SPECIFICATION)))
     ;; Don't optimize out this temporary, even if (CAR VARIABLE-SPECICIATION)
     ;; is an ATOM. We don't want to risk side-effects.
     ,(CASEQ (LENGTH VARIABLE-SPECIFICATION)
	((1)
	 `(MBINDING-SUB ,TEMP ,TEMP NIL ,@BODY))
	((2)
	 `(MBINDING-SUB ,TEMP ,(CADR VARIABLE-SPECIFICATION) NIL ,@BODY))
	((3)
	 `(MBINDING-SUB ,TEMP ,(CADR VARIABLE-SPECIFICATION)
			,(CADDR VARIABLE-SPECIFICATION)
			,@BODY))
	(T
	  (ERROR "Bad variable specification:" variable-specification)))))

(DEFVAR MBINDING-USAGE
  #+(and PDP10 Maclisp)    'PROG1
  #+(and Multics Maclisp)  'UNWIND-PROTECT
  #+Franz                  'PROG1
  #+LISPM                  'UNWIND-PROTECT
  #+NIL                    'UNWIND-PROTECT
  )
  
(DEFMACRO MBINDING-SUB (VARIABLES VALUES FUNCTION-NAME &REST BODY
				  &AUX (WIN (GENSYM)))
  (CASEQ MBINDING-USAGE
    ((PROG1)
     `(PROG1 (PROGN (MBIND ,VARIABLES ,VALUES ,FUNCTION-NAME) ,@BODY)
	     (MUNBIND ,VARIABLES)))
    ((UNWIND-PROTECT)
     `(LET ((,WIN NIL))
	(UNWIND-PROTECT
	 (PROGN (MBIND ,VARIABLES ,VALUES ,FUNCTION-NAME)
		(SETQ ,WIN T)
		,@BODY)
	 (IF ,WIN (MUNBIND ,VARIABLES)))))
    ((PROGV)
     `(LET ((,WIN (MBINDING-CHECK ,VARIABLES ,VALUES ,FUNCTION-NAME)))
	(PROGV ,VARIABLES
	       ,WIN
	       ,@BODY)))
    (T
     (ERROR "Unknown setting of MBINDING-USAGE" MBINDING-USAGE))))

#+NIL
(DEFMACRO MDEFPROP (A B C) `(MPUTPROP ',A ',B ',C))

;; How About MTYPEP like (MTYPEP EXP 'TAN) or (MTYPEP EXP '*) - Jim.
;; Better, (EQ (MTYPEP EXP) 'TAN).

(DEFMACRO MTANP (X) `(AND (NOT (ATOM ,X)) (EQ (CAAR ,X) '%TAN)))

(DEFMACRO MATANP (X) `(AND (NOT (ATOM ,X)) (EQ (CAAR ,X) '%ATAN)))

;; Macros used in LIMIT, DEFINT, RESIDU.
;; If we get a lot of these, they can be split off into a separate macro
;; package.

(DEFMACRO REAL-INFINITYP (X) `(MEMQ ,X REAL-INFINITIES))

(DEFMACRO INFINITYP (X) `(MEMQ ,X INFINITIES))

(DEFMACRO REAL-EPSILONP (X) `(MEMQ ,X INFINITESIMALS))

(DEFMACRO FREE-EPSILONP (X)
  `(DO ((ONE-EPS INFINITESIMALS (CDR ONE-EPS)))
       ((NULL ONE-EPS) T)
     (IF (NOT (FREE (CAR ONE-EPS) ,X))  (RETURN ()))))

(DEFMACRO FREE-INFP (X)
  `(DO ((ONE-INF INFINITIES (CDR ONE-INF)))
       ((NULL ONE-INF) T)
     (IF (NOT (FREE (CAR ONE-INF) ,X))  (RETURN ()))))

(DEFMACRO INF-TYPEP (X)
  `(CAR (AMONGL INFINITIES ,X)))

(DEFMACRO HOT-COEF (P)
 `(PDIS (CADDR (CADR (RAT-NO-RATFAC ,P)))))

;; Special form for declaring Macsyma external variables.  It may be used for
;; User level variables, or those referenced by other Lisp programs.

;; Syntax is:
;; (DEFMVAR <name> &OPTIONAL <initial-value> <documentation> . <flags>) See
;; MC:LIBMAX;DEFINE > for complete documentation of syntax.  The code in this
;; file for DEFMVAR is for non-ITS systems only.  LIBMAX;DEFINE contains code
;; for ITS.  Other systems may process the documentation information as they
;; wish.

;; Be sure to expand into DEFVAR and not into (DECLARE (SPECIAL ...)) as
;; certain systems do other things with DEFVAR.  The Lisp Machine, for
;; instance, annotates the file name.  On Multics and the Lisp Machine, expand
;; into DEFCONST since the entire Macsyma system is present before user files
;; are loaded, so there is no need to do the BOUNDP check.

#-(or Franz ITS)
(DEFMACRO DEFMVAR (VARIABLE &OPTIONAL (INITIAL-VALUE NIL IV-P) DOCUMENTATION
                            &REST FLAGS &AUX DEFINER TYPE)
  DOCUMENTATION FLAGS ;; Ignored certain places.
  (SETQ DEFINER #+(or Multics Lispm) 'DEFCONST
		#-(or Multics Lispm) 'DEFVAR)
  #-Lispm
  (SETQ TYPE (COND ((MEMQ 'FIXNUM FLAGS) 'FIXNUM)
		   ((MEMQ 'FLONUM FLAGS) 'FLONUM)
		   (T NIL)))
  `(PROGN 'COMPILE
	  ,(IF IV-P
	       `(,DEFINER ,VARIABLE ,INITIAL-VALUE)
	       `(,DEFINER ,VARIABLE #+LISPM () ))
	  ,@(IF TYPE `((DECLARE (,TYPE ,VARIABLE))))))

;; Special form for declaring Macsyma external procedures.  Version for ITS
;; is in LIBMAX;DEFINE.
;; Franz version is in libmax/vdefine.l

#-(or Franz ITS)
(DEFMACRO DEFMFUN (FUNCTION . REST) `(DEFUN ,FUNCTION . ,REST))

#-(or Franz ITS)
(DEFMACRO DEFMSPEC (FUNCTION . REST)
  `(DEFUN (,FUNCTION MFEXPR*) . ,REST))

;;;	The following MAUTOLOAD macro makes setting up autoload props for files
;;; on "standard" Macsyma directories easy, and clean. As an example, the
;;; code in SUPRV would look as folllows:
;;;
;;; (MAUTOLOAD (PURCOPY '(FASL DSK MACSYM))
;;;  (LIMIT   $LIMIT $LDEFINT)
;;;  (IRINTE  INTE)
;;;  (MATCOM  $MATCHDECLARE $DEFMATCH $TELLSIMP $TELLSIMPAFTER $DEFRULE)
;;;  (MATRUN  $DISPRULE $REMRULE $APPLY1 $APPLYB1 $APPLY2 $APPLYB2
;;;	      FINDBE FINDFUN FINDEXPON FINDBASE PART+ PART*)
;;;   ...
;;;
;;;  ((LISPT FASL DSK LIBLSP) $TECO $TSTRING $TECMAC $EMACS $EDIT)
;;;
;;;   ... )
;;;
;;;	The reason the file-spec list evals, is so that one may do a PURCOPY as
;;; above, and also one could imagine having a status request here to obtain
;;; the canonical file spec's.
;;;	Note that the first arg must be of the form (FN2 DEV DIR) if a file
;;; mask is being used; this macro could be much more elaborate.

#+ITS
(DEFMACRO MAUTOLOAD (FN2-DEV-DIR &REST MASTER-LIST)
  `(DOLIST (L ',MASTER-LIST)
     (DO ((FILE (IF (ATOM (CAR L))
		    (CONS (CAR L) ,FN2-DEV-DIR)
		    (CAR L)))
	  (FUNLIST (CDR L) (CDR FUNLIST)))
	 ((NULL FUNLIST))
       (PUTPROP (CAR FUNLIST) FILE 'AUTOLOAD))))

(defmacro sys-user-id ()
  #+Franz '(getenv '|USER|)
  #+lispm 'user-id
  #+Multics '(status uname)
  #-(or Franz Multics lispm) '(status userid))

(defmacro sys-free-memory ()
  #-(or Multics lispm NIL) '(status memfree)
  #+(or Multics lispm NIL) 10000.) ;This should look at the pdir size
                               ;and mung it to give a good approximation.

;; Setf hacking.
;;
;;
;;(defsetf GET ((() sym tag) value) T 
;;   (eval-ordered* '(nsym ntag nvalue)
;;		  `(,sym ,tag ,value)
;;		  '`((PUTPROP ,nsym ,nvalue ,ntag))))

#+PDP10
(defsetf MGET ((() sym tag) value) T 
  (eval-ordered* '(nsym ntag nvalue)
		 `(,sym ,tag ,value)
		 '`((MPUTPROP ,nsym ,nvalue ,ntag))))

#+PDP10
(defsetf $GET ((() sym tag) value) T 
  (eval-ordered* '(nsym ntag nvalue)
		 `(,sym ,tag ,value)
		 '`(($PUT ,nsym ,nvalue ,ntag))))

#+Franz
(defsetf mget (expr value)
   `(mputprop ,(cadr expr) ,value ,(caddr expr)))

#+Franz
(defsetf $get (expr value)
   `($put ,(cadr expr) ,value ,(caddr expr)))

#+NIL
(DEFPROP MGET SETF-MGET SI:SETF-SUBR)
#+NIL
(DEFPROP $GET SETF-$GET SI:SETF-SUBR)

;;DIFFERENT version of setf on Multics and LM ...Bummer... -JIM 3/4/81
#+MULTICS
(defsetf MGET (sym tag) value
  `(MPUTPROP ,sym ,value ,tag))

#+MULTICS
(defsetf $GET (sym tag) value
  `($PUT ,sym ,value ,tag))

#+LISPM
(DEFUN (:PROPERTY MGET SI:SETF) (REF VAL)
  `(MPUTPROP ,(SECOND REF) ,VAL ,(THIRD REF)))

#+LISPM
(DEFUN (:PROPERTY $GET SI:SETF) (REF VAL)
  `($PUT ,(SECOND REF) ,VAL ,(THIRD REF)))


(defmacro initialize-random-seed ()
  #+PDP10 '(sstatus random 0)
  #+LISPM () ;;(si:random-initialize si:random-array) obsolete. what now?
  #+NIL '(si:random-number-seed 0)
  )

;; These idiot macros are used in some places in macsyma.
;; The LISPM doesn't "go that high" with the series. DO NOT USE THESE
;; in new code. -gjc

(DEFMACRO EIGHTH  (FORM) `(CADDDR (CDDDDR ,FORM)))
(DEFMACRO NINTH   (FORM) `(CAR (CDDDDR (CDDDDR ,FORM))))
(DEFMACRO TENTH	  (FORM) `(CADR (CDDDDR (CDDDDR ,FORM))))

(DEFMACRO REST5 (FORM) `(CDR (CDDDDR ,FORM)))
(DEFMACRO REST6 (FORM) `(CDDR (CDDDDR ,FORM)))

