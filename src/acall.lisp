;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp; Package: Macsyma -*- ;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Run-time support for translated code.
;;; GJC: Experimental macsyma array lisp level support for translated
;;; code.
;;; To quickly handle the array reference and setting syntax in macsyma,

;;; In macsyma arrays go by an atomic name. Lists and matricies 
;;; may be hacked with the array syntax, which is convient.

(macsyma-module acall)

#+PDP10
(EVAL-WHEN (EVAL COMPILE) (SSTATUS FEATURE JPG))

(TRANSL-MODULE ACALL)

(DEFMFUN INTERVAL-ERROR (FUN LOW HIGH)
  (MERROR "Lower bound to ~:@M : ~M, not less than upper bound: ~M"
	  FUN LOW HIGH))

(DEFMFUN MFUNCALL (F &REST L)
  (COND ((AND (SYMBOLP F)
	      (FBOUNDP F))
	 ;; This is unfortunately NOT correct.
	 ;; A complicated interplay of the setting of $TRANSRUN,
	 ;; and MGET '$TRACE, GET 'TRANSLATED and MGET 'MEXPR
	 ;; interacts to determine if a function can be called
	 ;; via APPLY.
	 (APPLY F L))
	(T
	 (MAPPLY F L NIL))))

(DECLARE (*LEXPR LIST-REF))

;;; ((MQAPPLY ARRAY) X Y) is a strange form, meaning (X)[Y].

(DEFMFUN MARRAYREF (ARRAY IND1 &REST INDS &AUX AP)
  (CASEQ (TYPEP ARRAY)
    ((ARRAY)
     (CASEQ (ARRAY-TYPE ARRAY)
       ((FLONUM FIXNUM #+LISPM ART-Q)
	(LEXPR-FUNCALL ARRAY IND1 INDS))
       ((T)
	(MARRAYREF-GENSUB ARRAY IND1 INDS))
       (T
	(MERROR "BUG: Non-handled array created. ~M" ARRAY))))
    ((SYMBOL)
     (SIMPLIFY (COND ((SETQ AP (GET ARRAY 'ARRAY))
		      (LET ((VAL (COND ((NULL INDS)
					(FUNCALL AP IND1))
				       (T
					(LEXPR-FUNCALL AP IND1 INDS)))))
			;; Check for KLUDGING array function implementation.
			(IF (CASEQ (ARRAY-TYPE AP)
			      ((FLONUM) (= VAL FLOUNBOUND))
			      ((FIXNUM) (= VAL FIXUNBOUND))
			      ((T) (EQ VAL MUNBOUND))
			      (T (MERROR "BUG: Array pointer of unknown type: ~S"
					 AP)))
			    (ARRFIND `((,ARRAY ,ARRAY) ,IND1 ,@INDS))
			    VAL)))
		     ((SETQ AP (MGET ARRAY 'ARRAY))
		      #+JPG
		      (AND (MFILEP AP) (I-$UNSTORE (LIST ARRAY)))
		      (ARRFIND `((,ARRAY ARRAY) ,IND1 ,@INDS)))
		     ((SETQ AP (MGET ARRAY 'HASHAR))
		      #+JPG
		      (AND (MFILEP AP) (I-$UNSTORE (LIST ARRAY)))
		      (HARRFIND `((,ARRAY ARRAY) ,IND1  ,@INDS)))
		     ((EQ ARRAY 'MQAPPLY)
		      (LEXPR-FUNCALL #'MARRAYREF IND1 INDS))
		     (T
		      `((,ARRAY  ARRAY) ,IND1  ,@INDS)))))
    ((LIST)
     (SIMPLIFY (COND ((MEMQ (CAAR ARRAY) '(MLIST $MATRIX))
		      (LIST-REF ARRAY (CONS IND1 INDS)))
		     (T
		      `((MQAPPLY ARRAY) ,ARRAY ,IND1 ,@INDS)))))
    (T
     (MERROR "Bad object to reference as an array: ~M" ARRAY))))

(DEFMFUN $ARRAYAPPLY (AR INDS)
  (OR ($LISTP INDS)
      (MERROR "The second arg to ARRAYAPPLY must be a list."))
  (LEXPR-FUNCALL #'MARRAYREF AR (CDR INDS)))

(DEFMFUN $ARRAYSETAPPLY (AR INDS VAL)
  (OR ($LISTP INDS)
      (MERROR "The second arg to ARRAYAPPLY must be a list."))
  (LEXPR-FUNCALL #'MARRAYSET VAL AR (CDR INDS)))

(DEFMFUN MARRAYSET (VAL ARRAY IND1 &REST INDS &AUX AP)
  (CASEQ (TYPEP ARRAY)
    ((ARRAY)
     (CASEQ (ARRAY-TYPE ARRAY)
       ((FIXNUM FLONUM #+LISPM ART-Q)
	(STORE (LEXPR-FUNCALL ARRAY IND1 INDS) VAL))
       ((T)
	(MARRAYSET-GENSUB VAL ARRAY IND1 INDS))
       (T
	(MERROR "BUG: unhandled array type. ~M" ARRAY))))
    ((SYMBOL)
     (COND ((SETQ AP (GET ARRAY 'ARRAY))
	    (COND ((null inds)
		   (STORE (FUNCALL AP IND1) VAL))
		  (t
		   (STORE (LEXPR-FUNCALL AP IND1 INDS) VAL))))
	   ((SETQ AP (MGET ARRAY 'ARRAY))
	    #+JPG
	    (AND (MFILEP AP) (I-$UNSTORE (LIST ARRAY)))
	    ;; the macsyma ARRAY frob is NOT an array pointer, it
	    ;; is a GENSYM with a lisp array property, don't
	    ;; ask me why.
	    (COND ((null inds)
		   (store (funcall ap ind1) val))
		  (t
		   (STORE (APPLY AP (CONS IND1 INDS)) VAL))))
	   ((SETQ AP (MGET ARRAY 'HASHAR))
	    #+JPG
	    (AND (MFILEP AP) (I-$UNSTORE (LIST ARRAY)))
	    (ARRSTORE `((,ARRAY ARRAY)
			,@(MAPCAR #'(LAMBDA (U)
				      `((MQUOTE SIMP) ,U))
				  (CONS IND1 INDS)))
		      VAL))
	   ((EQ ARRAY 'MQAPPLY)
	    (APPLY #'MARRAYSET `(,VAL ,IND1 ,@INDS)))
	   (T
	    (ARRSTORE `((,ARRAY ARRAY) ,@(MAPCAR #'(LAMBDA (U)
						     `((MQUOTE SIMP) ,U))
						 (CONS IND1 INDS)))
		      VAL))))
    ((LIST)
     (COND ((MEMQ (CAAR ARRAY) '(MLIST $MATRIX))
	    (LIST-REF ARRAY (CONS IND1 INDS) T VAL))
	   ('else
	    (MERROR "Bad use of "":"" on~%~M" ARRAY))))
    (T
     (MERROR "Bad argument to set as an array.~%~M" ARRAY)))
  VAL)

;;; Note that all these have HEADERS on the list. The CAR of a list I
;;; will call element 0. So [1,2][1] => 1

(DEFUN LIST-REF (L INDEXL &OPTIONAL SET-FLAG VAL)
  (COND ((ATOM L)
	 (MERROR "ERROR-> tried to take part of an atom."))
	((NULL (CDR INDEXL))
	 (LET ((N (CAR INDEXL)))
	   (COND ((AND (FIXP N) (PLUSP N)
		       (OR (EQ (CAAR L) 'MLIST)
			   (EQ (CAAR L) '$MATRIX)))
		  (LET ((RET (DO ((J 1 (1+ J))
				  (N (FIXNUM-IDENTITY N))
				  (L (CDR L) (CDR L)))
				 ((OR (NULL L) (= J N))
				  (COND ((NULL L)
					 (MERROR "Improper index to list or matrix: ~M" N))
					(SET-FLAG
					 (RPLACA L VAL))
					(T
					 (CAR L))))
			       (DECLARE (FIXNUM J N)))))
		    (COND (SET-FLAG L)
			  (T RET))))
		 (T
		  (MERROR "ERROR-> ~M  bad part subscript." N)))))
	(SET-FLAG
	 (LIST-REF (LIST-REF L `(,(CAR INDEXL)))
		   (CDR INDEXL)
		   SET-FLAG
		   VAL)
	 L)
	(T
	 (LIST-REF (LIST-REF L `(,(CAR INDEXL))) (CDR INDEXL)))))

;;; 3 guesses where this code is from.
;;;(DEFUN DISP1 (LL LABLIST EQNSP)
;;; (COND (LABLIST (SETQ LABLIST (NCONS '(MLIST SIMP)))))
;;; (DO ((LL LL (CDR LL)) (L) (ANS) ($DISPFLAG T) (TIM 0))
;;;     ((NULL LL) (OR LABLIST '$DONE))
;;;     (SETQ L (CAR LL) ANS (MEVAL L))
;;;     (COND ((AND EQNSP (OR (ATOM ANS) (NOT (EQ (CAAR ANS) 'MEQUAL))))
;;;	    (SETQ ANS (LIST '(MEQUAL) (DISP2 L) ANS))))
;;;     (COND (LABLIST (COND ((NOT (CHECKLABEL $LINECHAR))
;;;                           (SETQ $LINENUM (1+ $LINENUM))))
;;;		    (MAKELABEL $LINECHAR) (NCONC LABLIST (NCONS LINELABLE))
;;;		    (COND ((NOT $NOLABELS) (SET LINELABLE ANS)))))
;;;     (SETQ TIM (RUNTIME))
;;;     (DISPLA (LIST '(MLABLE) (COND (LABLIST LINELABLE)) ANS))
;;;     (MTERPRI)
;;;     (TIMEORG TIM)))

(DECLARE (SPECIAL $DISPFLAG))
(DEFMFUN DISPLAY-FOR-TR (LABELSP EQUATIONSP &REST ARGL)
       (DO ((ARGL ARGL (CDR ARGL))
	    (LABLIST NIL)
	    (TIM 0))
	   ((NULL ARGL)
	    (COND (LABELSP
		   `((MLIST) ,@LABLIST))
		  (T '$DONE)))
	   (LET ((ANS (CAR ARGL)))
		(COND ((AND EQUATIONSP
			    ;; ((MEQUAL) FOO BAR)
			    (NOT (ATOM (CADDR ANS)))
			    (EQ (CAAR (CADDR ANS)) 'MEQUAL))
		       ;; if the ANS evaluats to something with an "="
		       ;; allready then of course he really meant to use
		       ;; DISP, but we might as well do what he means right?
		       (SETQ ANS (CADDR ANS))))
		(COND (LABELSP
		       (OR (CHECKLABEL $LINECHAR)
			   (SETQ $LINENUM (1+ $LINENUM)))
		       (MAKELABEL $LINECHAR)
		       ;; setqs the free variable LINELABLE, what a win,
		       ;; how convenient, now I don't need to use LET !
		       (PUSH LINELABLE ;; note the spelling
			     LABLIST)
		       (OR  $NOLABELS
			    (SET LINELABLE ;; SET !!!!
				 ANS))))
		(SETQ TIM (RUNTIME))
		(DISPLA `((MLABLE) ,(COND (LABELSP LINELABLE)) ,ANS))
		(MTERPRI)
		(TIMEORG TIM))))


(DEFMFUN INSURE-ARRAY-PROPS (FNNAME IGNORE-MODE NUMBER-OF-ARGS &AUX ARY)
	 ;; called during load or eval time by the defining forms
	 ;; for translated array-functions.
	 ;; this duplicates code in JPG;MLISP (however, the code in MLISP
	 ;; is not callable because it is in a big piece of so-called
	 ;; multi-purpose code).

	 ;; This code is incredibly kludgy. For example, what if
	 ;; the function FOO[J] had a lisp array property gotten
	 ;; by ARRAY(FOO,FIXNUM,33), how is *THAT* detected by this code?
	 ;; Well, it is because that will also put an MPROP ARRAY of $FOO,
	 ;; and (ARRAYDIMS '$FOO) works! (Also checks the array property).
	 ;; Isn't that something. Shit, I never knew that ARRAYDIMS worked
	 ;; on symbols. What a crock.
	 (COND ((PROG2 (ADD2LNC FNNAME $ARRAYS)
		       (SETQ ARY (MGETL FNNAME '(HASHAR ARRAY))))
		#+JPG
		(COND ((MFILEP (CADR ARY))
		       (I-$UNSTORE (NCONS FNNAME))
		       (SETQ ARY (MGETL FNNAME '(HASHAR ARRAY)))))
		(COND ((NOT (= (COND ((EQ (CAR ARY) 'HASHAR) (FUNCALL (CADR ARY) 2))
				     (T (LENGTH (CDR (ARRAYDIMS (CADR ARY))))))
			       NUMBER-OF-ARGS))
		       (MERROR
			"~:@M Array already defined with different dimensions"
			FNNAME))))
	       (T (MPUTPROP FNNAME (SETQ ARY (GENSYM)) 'HASHAR)
		  (*ARRAY ARY T 7)
		  (STORE (FUNCALL ARY 0) 4)
		  (STORE (FUNCALL ARY 1) 0)
		  (STORE (FUNCALL ARY 2) NUMBER-OF-ARGS))))

;;; An entry point to $APPLY for translated code.

(DEFMFUN MAPPLY-TR (FUN LIST)
	 (OR ($LISTP LIST)
	     (MERROR "Second arg to APPLY was not a list:~%~M" LIST))
	 (MAPPLY FUN (CDR LIST) '|the first arg to a translated APPLY|))


(DEFMFUN ASSIGN-CHECK (VAR VAL)
  (LET ((A (GET VAR 'ASSIGN)))
    (IF A (FUNCALL A VAR VAL))))


(DECLARE (SPECIAL MAPLP))

(DEFMFUN MAPLIST_TR (FUN &REST L)
  (SIMPLIFY (LET ((MAPLP T) RES)
	      (SETQ RES (LEXPR-FUNCALL #'MAP1 (GETOPR FUN) L))
	      (COND ((ATOM RES) (LIST '(MLIST) RES))
		    ((EQ (CAAR RES) 'MLIST) RES)
		    (T (CONS '(MLIST) (MARGS RES)))))))


;;; Entry point into DB for translated code. The main point here
;;; is that evaluation of a form takes place first, (using the lisp
;;; evaluator), and then the trueness is checked. It is not correct
;;; to call the function IS because double-evaluation will then
;;; result, which is wrong, not to mention being incompatible with
;;; the interpreter. 
;;;
;;; This code is take from the COMPAR module, and altered such that calls to
;;; the macsyma evaluator do not take place. It would be a lot
;;; better to simply modify the code in COMPAR! However, mumble...
;;; Anyway, be carefull of changes to COMPAR that break this code.

(DEFMFUN IS-BOOLE-CHECK (FORM)
  (COND ((NULL FORM) NIL)
	((EQ FORM T) T)
	('ELSE
	 ;; We check for T and NIL quickly, otherwise go for the database.
	 (MEVALP_TR FORM T NIL))))

(DEFMFUN MAYBE-BOOLE-CHECK (FORM)
  (MEVALP_TR FORM NIL NIL))

;; The following entry point is for querying the database without
;; the dubious side effects of using PREDERROR:FALSE.

(DEFMSPEC $MAYBE (FORM) (MEVALP_TR (FEXPRCHECK FORM) NIL T))

(DECLARE (SPECIAL PATEVALLED))

(defun mevalp_tr (pat error? meval?)
  (let (patevalled ans)
    (setq ans (mevalp1_tr pat error? meval?))
    (cond ((memq ans '(t nil)) ans)
	  (error?
	   (pre-err patevalled))
	  ('else '$UNKNOWN))))

(defun mevalp1_tr (pat error? meval?)
  (cond ((and (not (atom pat)) (memq (caar pat) '(mnot mand mor)))
	 (cond ((eq 'mnot (caar pat)) (is-mnot_tr (cadr pat) error? meval?))
	       ((eq 'mand (caar pat)) (is-mand_tr (cdr pat) error? meval?))
	       (t (is-mor_tr (cdr pat) error? meval?))))
	((atom (setq patevalled (if meval? (meval pat) pat))) patevalled)
	((memq (caar patevalled) '(mnot mand mor)) (mevalp1_tr patevalled
							       error?
							       meval?))
	(t (mevalp2 (caar patevalled) (cadr patevalled) (caddr patevalled)))))

(defun is-mnot_tr (pred error? meval?)
  (setq pred (mevalp_tr pred error? meval?))
  (cond ((eq t pred) nil)
	((not pred))
	(t (pred-reverse pred))))

(defun is-mand_tr (pl error? meval?)
  (do ((dummy) (npl))
      ((null pl) (cond ((null npl))
		       ((null (cdr npl)) (car npl))
		       (t (cons '(mand) (nreverse npl)))))
    (setq dummy (mevalp_tr (car pl) error? meval?)
	  pl (cdr pl))
    (cond ((eq t dummy))
	  ((null dummy) (return nil))
	  (t (setq npl (cons dummy npl))))))

(defun is-mor_tr (pl error? meval?)
  (do ((dummy) (npl))
      ((null pl) (cond ((null npl) nil)
		       ((null (cdr npl)) (car npl))
		       (t (cons '(mor) (nreverse npl)))))
    (setq dummy (mevalp_tr (car pl) error? meval?)
	  pl (cdr pl))
    (cond ((eq t dummy) (return t))
	  ((null dummy))
	  (t (setq npl (cons dummy npl))))))


;; Some functions for even faster calling of arrays.
(DECLARE (FLONUM (MARRAYREF1$ NIL NIL)
		 (MARRAYSET1$ FLONUM NIL NIL)))

(DEFUN MARRAYREF1$ (ARRAY INDEX)
  (CASEQ (TYPEP ARRAY)
    ((ARRAY)
     (CASEQ (ARRAY-TYPE ARRAY)
       ((FLONUM) (ARRAYCALL FLONUM ARRAY INDEX))
       (T (MERROR "Bad type of array to call for FLOAT value: ~M" ARRAY))))
    (T
     (FLOAT (MARRAYREF ARRAY INDEX)))))

(DEFUN MARRAYSET1$ (VALUE ARRAY INDEX)
  (CASEQ (TYPEP ARRAY)
    ((ARRAY)
     (CASEQ (ARRAY-TYPE ARRAY)
       ((FLONUM) (STORE (ARRAYCALL FLONUM ARRAY INDEX) VALUE))
       (T (MERROR "Bad type of array to set FLOAT into: ~M" ARRAY))))
    (T
     (FLOAT (MARRAYSET VALUE ARRAY INDEX)))))


(DEFMFUN APPLICATION-OPERATOR (FORM *IGNORE* *IGNORE*)
  (APPLY (CAAR FORM) (CDR FORM)))

(DEFMFUN MAKE-ALAMBDA (FORMALS BODY)
  (LET ((NAME (GENSYM)))
    ;; on LISPM we can use closures after we fix up MEVAL and MAPPLY.
    ;; This isn't much more expensive, GENSYMs get garbage collected
    ;; just like any other object.
    (PUTPROP NAME 'APPLICATION-OPERATOR 'OPERATORS)
    (EVAL `(DEFUN ,NAME ,FORMALS ,BODY))
    NAME))

;; more efficient operators calls.

(DEFUN *MMINUS (X)
  (IF (NUMBERP X)
      (MINUS X)
      (SIMPLIFY (LIST '(MMINUS) X))))

(DEFUN RETLIST_TR N
  (DO ((J (1- N) (- J 2))
       (L () (CONS (LIST '(MEQUAL SIMP) (ARG J) (ARG (1+ J))) L)))
      ((< J 0) (CONS '(MLIST SIMP) L))))
