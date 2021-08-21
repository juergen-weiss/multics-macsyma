;;;;;;;;;;;;;; -*- Mode: Lisp; Package: Macsyma; Ibase 10 -*- ;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macsyma-module schatc)

(EVAL-WHEN (EVAL COMPILE)
	   (SETQ OLD-IBASE IBASE IBASE 10.))

(DECLARE (SPECIAL VAR SPLIST DICT ANS *SCHATFACTOR BINDLIST SPECLIST)
	 (*LEXPR $DIVIDE $FACTOR)
	 (GENPREFIX SCHAT))

(DEFMACRO PUSH-CONTEXT () '(SETQ ANS (CONS NIL ANS)))

(DEFMACRO PUSH-LOOP-CONTEXT () '(RPLACD ANS (CONS '*LOOP (CDR ANS))))

(DEFMACRO PRESERVE (Z)
	  `(RPLACD ANS (CONS (CONS ,Z (CDR ,Z)) (CDR ANS))))

(DEFMACRO ADD-TO (VAR VAL)
	  `(RPLACD ANS (CONS (CONS ,VAR ,VAL) (CDR ANS))))

(DEFMACRO VAR-PAT (X) `(ATOM (CAR ,X)))

(SETQ *SCHATFACTOR NIL)		;DETERMINES WHETHER FACTORING SHOULD BE USED.


;;;
;;;	VARIOUS MACROS WHICH PERMIT STAND-ALONE SCHATCHEN'S
;;;	Stand-alone Schatchen's are only needed on ITS, so define them
;;;	there only.

#+ITS (PROGN 'COMPILE

(DEFUN COMPILING MACRO (L)
       (AND (BOUNDP 'COMPILER-STATE)
	    (NOT (EQ COMPILER-STATE 'TOPLEVEL))))

;; If compiling or the function already defined, punt.

(DEFMACRO DEFINE L
       (COND ((OR (COMPILING) (FBOUNDP (CADR L)))
	      NIL)
	     (T (CONS 'DEFUN L))))

(DEFINE MPLUSP (X)
	(AND (NULL (ATOM X))
	     (EQ (CAAR X) 'MPLUS)))

(DEFINE MTIMESP (X)
	(AND (NULL (ATOM X))
	     (EQ (CAAR X) 'MTIMES)))

(DEFINE MEXPTP (X)
	(AND (NULL (ATOM X))
	     (EQ (CAAR X) 'MEXPT)))

(DEFINE FREE (E X)
	(COND ((EQUAL E X) NIL)
	      ((ATOM E))
	      ((DO ((L (CDR E) (CDR L)))
		   ((NULL L) T)
		   (OR (FREE (CAR L) X)
		       (RETURN NIL))))))

(DEFINE ALIKE (X Y)
	(COND ((ATOM X) (EQUAL X Y))
	      ((ATOM Y) NIL)
	      (T (AND (ALIKE1 (CAR X) (CAR Y))
		      (ALIKE (CDR X) (CDR Y))))))

(DEFINE ALIKE1 (X Y)
	(COND ((EQ X Y))
	      ((ATOM X) (EQUAL X Y))
	      ((ATOM Y) NIL)
	      (T (AND (EQ (CAAR X) (CAAR Y))
		      (ALIKE (CDR X) (CDR Y))))))

) ;; End of ITS conditionalization


;
;VARIOUS SIMPLE PATTERNS
;

(DEFUN FREE1 (A)
	(AND (NULL (SIGNP E A)) (FREE A VAR)))

(DEFUN NOT-ZERO-FREE (A VAR) (FREE1 A))

(DEFUN LINEAR* (E VAR)
	(PROG (A N)
		(SETQ N ($RATCOEF E VAR))
		(COND ((NULL (FREE N VAR))
		       (RETURN NIL)))
		(SETQ A (SIMPLUS (LIST '(MPLUS) E (LIST '(MTIMES) -1 N VAR)) 1 NIL))
		(RETURN
		   (COND ((FREE A VAR) (CONS A N))))))

(DEFUN DVCOE (E PAT ARGS)
    (M1 ($RATSIMP (LIST '(MTIMES) E ARGS)) PAT))

(DEFUN SCHATCHEN (E P)
	(M2 E P NIL))

;THE RESTORE FUNCTIONS RESTORE THE SPEC-VAR ANS
;AND RETURN TRUE OR FALSE AS FOLLOWS
;RESTORE - FLASE
;RESTORE1 - TRUE AND CLEARS UP ANS
;RESTORE2 - TRUE AND CLEARS OFF *LOOP INDICATORS
;	    DOES NOT FIX UP THE EXPRESSION AND
;	    IS THUS TO BE USED ONLY INTERNALLY
;
;TO INSURE THAT THERE IS NO CONFLICT IN SPECIAL VARIABLES,
;ESPECIALLY WITH THE VAR* (SET) MODE ALL SCHATCHEN VARIABLES
;ARE TO BE PRECEEDED BY A "%"

(DEFUN M2 (E P SPLIST)
   ((LAMBDA (ANS)
	(COND ((NULL (M1 (COPY E) P)) NIL)
	      ((NULL (CDR ANS)))
	      ((CDR ANS))))
    (LIST NIL)))

(DEFUN SAV&DEL (X)
	(PRESERVE X)
	(RPLACD X (CDDR X)))

(DEFUN M1 (E P)
	(COND ((EQ E P) T)
	      ((ATOM P) NIL)
	      ((VAR-PAT P)
	       (PUSH-CONTEXT)
	       (COND ((TESTA P E NIL)
		      (RESTORE1))
		     ((RESTORE))))
	      ((ATOM (CAAR P))
	       (COND ((MEMQ 'SIMP (CDAR P)) (ALIKE1 E P))
		     ((MEMQ (CAAR P) '(MPLUS MTIMES))
		      (LOOPP E P))
		     ((MEMQ (CAAR P) '(MEXPT ZEPOW)) (ZEPOW E P T))
		     ((AND (NOT (ATOM E)) (EQ (CAAR E) (CAAR P))) (EACHP E P)) 
		     ((EQ (CAAR P) 'COEFFT) (COEFFT E P T))
		     ((EQ (CAAR P) 'COEFFPT) (COEFFPT E P T))
		     ((EQ (CAAR P) 'COEFFP) (COEFFP E P T))
		     ((EQ (CAAR P) 'COEFFTT)
		      (COEFFTT E (CADR P) T 'MTIMES))
		     ((EQ (CAAR P) 'COEFFPP)
		      (COEFFTT E (CADR P) T 'MPLUS))))
	      ((VAR-PAT (CAAR P))					;HAIRY OPERATOR MATCHING SCHEME
	       (COND ((ATOM E) NIL)				;NO OPERATOR TO MATCH
		     ((PROG2 (PUSH-CONTEXT)			;BIND THE CONTEXT
			     (TESTA (CAAR P) (CAR E) NIL))	;TRY IT
		      (COND ((MEMQ (CAAR E) '(MPLUS MTIMES))	;CHECK FOR COMMUTIVITY
			     (COND ((LOOPP E (CONS (CAR E) (CDR P)))
				    (RESTORE1))
				   ((RESTORE))))
			    ((EACHP E P)
			     (RESTORE1))
			    ((RESTORE))))
		     ((RESTORE))))))

(DEFUN LOOPP (E P)
  (PROG (X Z)
	(SETQ E (COND  ((ATOM E) (LIST (CAR P) E))
		       ((NULL (EQ (CAAR P) (CAAR E)))
			(COND ((AND *SCHATFACTOR (EQ (CAAR E) 'MPLUS)
				    (MTIMESP (SETQ X ($FACTOR E))))
			       X)
			      ((LIST (CAR P) E))))
		       (E)))
	(PUSH-CONTEXT)
	(SETQ Z P)
  LOOP	(SETQ Z (CDR Z))
	(COND ((NULL Z)
	       (RETURN
		(COND ((NULL (CDR E)) (RESTORE1))
		      ((RESTORE))))))
	(SETQ X E)
  L5	(COND ((NULL (CDR X)) 
	       ((LAMBDA (IDENT)
		   (COND ((AND IDENT (M1 IDENT (CAR Z)))
			  (GO LOOP))
			 ((RETURN (RESTORE)))))
		(OPIDENT (CAAR P))))
	      ((OR (ATOM (CAR Z)) (VAR-PAT (CAR Z)))
	       (COND ((M1 (CADR X) (CAR Z))
		      (SAV&DEL X) (GO LOOP))))
	      ((EQ (CAAAR Z) 'COEFFT)
	       (COND ((COEFFT E (CAR Z) NIL)
		      (GO LOOP))
		     ((RETURN (RESTORE)))))
	      ((EQ (CAAAR Z) 'COEFFP)
	       (COND ((COEFFP E (CAR Z) NIL)
		      (GO LOOP))
		     ((RETURN (RESTORE)))))
	      ((EQ (CAAAR Z) 'COEFFPT)
	       (COND ((COEFFPT E (CAR Z) NIL) (GO LOOP))
		     ((RETURN (RESTORE)))))
	      ((EQ (CAAAR Z) 'COEFFTT)
	       (COND ((COEFFTT E (CADAR Z) NIL 'MTIMES) (GO LOOP))
		     ((RETURN (RESTORE)))))
	      ((EQ (CAAAR Z) 'COEFFPP)
	       (COND ((COEFFTT E (CADAR Z) NIL 'MPLUS) (GO LOOP))
		     ((RETURN (RESTORE)))))
	      ((MEMQ (CAAAR Z) '(MEXPT ZEPOW)) 
	       (COND ((ZEPOW (CADR X) (CAR Z) T) 
		      (SAV&DEL X) (GO LOOP))))
	      ((EQ (CAAAR Z) 'LOOP)
	       (COND ((SCH-LOOP E (CDAR Z)) (GO LOOP))
		     ((RETURN (RESTORE)))))
	      ((M1 (CADR X) (CAR Z)) 
	       (SAV&DEL X) (GO LOOP)))
	(SETQ X (CDR X))
	(GO L5)))

;;; IND = T MEANS AN INTERNAL CALL (USUALLY FROM LOOPP)

(DEFUN COEFFP (E P IND)
    (PUSH-CONTEXT)
    (COND ((OR (AND (NULL (MPLUSP E))	;;;WITH IND SET, OR E = (PLUS <EXPR>)
		    (SETQ E (LIST '(MPLUS) E)))
	       IND (NULL (CDDR E)))
	   (COEFFPORT E P 0 IND))	;;; USE COEFFPORT
	  ((AND (NULL (CDDR P))		;;; P = ((COEFFP) (<VAR> <PRED> . . .))
		(VAR-PAT (CADR P)))	;;; SO CALL TESTA
	   (COND ((TESTA (CADR P) E NIL)
		  (COND ((MPLUSP E)
			 (PRESERVE E)
			 (RPLACD E NIL)
			 T)
			((merror "(BAD ARGS TO COEFFP -- REPORT BUG TO RZ)"))))))
	  ((DO ((X E (CDR X)))
	       ((NULL (CDR X))
		(COND ((M1 0 P) (RESTORE2))
		      ((RESTORE))))
	       (COND ((COEFFP (CADR X) P T)
		      (SAV&DEL X)
		      (RETURN (RESTORE2))))))))

(DEFUN COEFFT (E P IND)
    (PUSH-CONTEXT)
    (COND ((AND (NULL IND) (NULL (ATOM E)) (MEMQ (CAAR E) '(MPLUS MTIMES)))
	   (DO ((X E (CDR X)))
	       ((NULL (CDR X))
		(COND ((M1 1 P) (RESTORE2))
		      ((RESTORE))))
	       (COND ((COEFFT (CADR X) P T)
		      (SAV&DEL X)
		      (RETURN (RESTORE2))))))
	  ((AND (MPLUSP E) (CDDR E))
	   (COND ((AND *SCHATFACTOR (MTIMESP (SETQ E ($FACTOR E))))
		  (COEFFPORT E P 1 IND))
		 ((RESTORE))))
	  (T (COEFFPORT (COND ((MTIMESP E) E) ((LIST '(MTIMES) E)))
			P 1 IND))))

(DEFUN COEFFPORT (E P IDENT IND)
    (DO ((Z (CDDR P) (CDR Z))
	 (X E E))
	((NULL Z)
	 (COEFFRET E (CADR P) IDENT IND))
     L	;;; EACH TIME HERE WE HAVR CDR'D DOWN THE EXP.
        (COND ((NULL (CDR X))
	       (AND (NULL (M1 IDENT (CAR Z)))
		    (RETURN (RESTORE))))
	      ((OR (ATOM (CAR Z))
		   (VAR-PAT (CAR Z))))
	      ((EQ (CAAAR Z) 'COEFFTT)
	       (AND (NULL (COEFFTT E (CADAR Z) NIL 'MTIMES))
		    (RETURN (COEFFRET E P IDENT IND))))
	      ((EQ (CAAAR Z) 'COEFFPP)
	       (AND (NULL (COEFFTT E (CADAR Z) NIL 'MPLUS))
		    (RETURN (COEFFRET E P IDENT IND)))))
	(COND ((NULL (CDR X)))
	      ((M1 (CADR X) (CAR Z))
	       (SAV&DEL X))
	      (T (SETQ X (CDR X))
		   (GO L)))))

(DEFUN COEFFRET (E P IDENT IND)
    (COND ((NULL (CDR E))
	   (COND ((TESTA P IDENT NIL)
		  (COND (IND (RESTORE1))
			((RESTORE2))))
		 ((RESTORE))))
	  ((TESTA P (COND ((CDDR E) (APPEND E NIL))
			  ((CADR E)))
		 NIL)
	   (COND (IND (RESTORE1))
		 (T (PRESERVE E)
		    (RPLACD E NIL)
		    (RESTORE2))))
	  ((RESTORE))))

(DEFUN COEFFPT (E P IND)	;THE PATTERN LIST (P) MUST BE OF VAR-PATTERNS
	(PUSH-CONTEXT)
	(DO ((Z (COND ((MPLUSP E) E) ((LIST '(MPLUS) E))))
	     (ZZ (CONS '(COEFFT) (CDR P))))			;THIS ROUTINE IS THE ONE WHICH PUTS
								;MOST OF THE THE GARBAGE ON ANS IT
	    ((NULL (CDR Z))					;IT CANNOT USE THE SPLIST HACK 
	     (SETQ Z (FINDIT (COND ((EQ (CAADR P) 'VAR*)	;BECAUSE IT COULD BE USING
				    (CAR (CDDADR P)))		;MANY DIFFERENT VARIABLES ALTHOUGH 
				   ((CAADR P)))))		;THOUGHT THE FIRST IS THE ONLY ONE
	     ((LAMBDA (Q FL)					;WHICH BECOMES A SUM AND MIGHT BE RESET
		(COND ((NULL (TESTA (CADR P) Q FL))
		       (RESTORE))
		      (IND (RESTORE1))
		      (T (RESTORE2) Q)))
	      (COND ((NULL Z) 0)
		    ((NULL (CDR Z)) (CAR Z))
		    ((SIMPLUS (CONS '(MPLUS) Z) 1 NIL)))
	      (COND ((AND Z (CDR Z)) 'COEFFPT))))
	    (COND ((NULL (M1 (CADR Z) ZZ))	;THIS IS THE DO BODY
		   (SETQ Z (CDR Z)))
		  ((SAV&DEL Z)))))
(DEFUN ZEPOW (E P FL)				;FL=NIL INDICATES A RECURSIVE CALL
	(AND FL (PUSH-CONTEXT))	;SO ANS SHOULD NOT BE MARKED
	(COND ((ATOM E)
	       (COND ((EQUAL E 1)
		      (COND ((NOT (OR (M1 0 (CADDR P)) (M1 1 (CADR P))))
			     (RESTORE))
			    ((RESTORE1))))
		     ((EQUAL E 0)
		      (COND ((NULL (M1 0 (CADR P))) (RESTORE))
			    ((RESTORE1))))
		     ((AND (M1 E (CADR P)) (M1 1 (CADDR P)))
		      (RESTORE1))
		     ((RESTORE))))
	      ((AND *SCHATFACTOR (MPLUSP E) (SETQ E ($FACTOR E)) NIL))
	      ((AND (EQ (CAAR E) 'MTIMES)
		    (MEXPTP (CADR E)))
	       (DO ((E (CDDR E) (CDR E))
		    (B (CADADR E))
		    (X (CADDR (CADR E)))
		    (Z))
		   ((NULL E)			;OK NOW LETS TRY AGAIN
		    (ZEPOW (LIST '(MEXPT) (SIMPLIFYA B T)
					  (SIMPLIFYA X T)) P NIL))
		   (COND ((MEXPTP (CAR E))
			  (COND ((ALIKE1 (CADAR E) B)
				 (SETQ X (SIMPLUS (LIST '(MPLUS) X (CADDAR E)) 1 NIL)))
				((ALIKE1 (CADDAR E) X)
				 (SETQ B (SIMPTIMES (LIST '(MTIMES) B (CADAR E)) 1 NIL)))
				((SIGNP E (CADDR (SETQ Z ($DIVIDE X (CADDAR E)))))
				 (SETQ B (SIMPTIMES (LIST '(MTIMES) B
							  (LIST '(MEXPT) (CADAR E)
								(LIST '(MTIMES) (CADDAR E) (CADR Z)))) 1 NIL)))
				((RETURN (RESTORE)))))
			 ((ALIKE1 B (CAR E))
			  (SETQ X (SIMPLUS (LIST '(MPLUS) 1 X) 1 T)))
			 ((RETURN (RESTORE))))))
	      ((OR (AND (EQ (CAAR E) 'MEXPT)
			(M1 (CADR E) (CADR P))
			(M1 (CADDR E) (CADDR P)))
		   (AND (M1 E (CADR P))
			(M1 1 (CADDR P))))
	       (RESTORE1))
	      ((RESTORE))))

(DEFUN EACHP (E P)
	(COND ((= (LENGTH E) (LENGTH P))
	       (PUSH-CONTEXT)
	       (DO ((E (CDR E) (CDR E)))
		   ((NULL E) (RESTORE1))
		   (AND (NULL (M1 (CAR E) (CADR P))) (RETURN (RESTORE)))
		   (SETQ P (CDR P))))))

(DEFUN SCH-LOOP (E LP)
    (PUSH-CONTEXT) (PUSH-LOOP-CONTEXT)
    (DO ((X LP) (Z E) (Y))			;Y A PSEUDO SAVE
	(NIL)
	(COND ((NULL (M1 (CADR Z) (CAR X)))	;DIDN'T MATCH
	       (SETQ Z (CDR Z))			;NEXT ARG FOR LOOP
	       (COND ((CDR Z))
		     ((EQ X LP) (RETURN (RESTORE)))
		     (T (SETQ X (CAAR Y) Z (CDAR Y))
			(SETQ Y (CDR Y) ANS (CDR ANS))
			(POP-LOOP-CONTEXT))))
	      (T (SETQ Y (CONS (CONS X Z) Y))
		 (SAV&DEL Z)
		 (SETQ X (CDR X))
 		 (COND ((NULL X) (RETURN (RESTORE2)))
		       (T (PUSH-LOOP-CONTEXT)
			  (SETQ Z E)))))))

(DEFUN COEFFTT (EXP PAT IND OPIND)	;OPIND IS MPLUS OR MTIMES
    (PUSH-CONTEXT)
    (COND ((OR (ATOM EXP) (AND IND (NOT (EQ (CAAR EXP) OPIND))))
	   (SETQ EXP (LIST (LIST OPIND) EXP))))
    (SETQ SPLIST (CONS (CAR PAT) SPLIST))	;SAVE VAR NAME HERE
    (DO ((Z EXP) (RES))
	((NULL (CDR Z))
	 (SETQ SPLIST (CDR SPLIST))	;KILL NAME SAVED
	 (COND (RES (SETQ RES (COND ((CDR RES) (CONS (LIST OPIND) RES))
				    ((CAR RES))))
		    (COND ((AND (EQ (CAR PAT) 'VAR*)
				(MEMQ 'SET (CADR PAT)))
			   (ADD-TO (CADDR PAT) (SET (CADDR PAT) (SIMPLIFYA RES NIL))))
			   ((ADD-TO (CAR PAT) (SIMPLIFYA RES NIL))))
		    (COND (IND (RESTORE1))
			  ((RESTORE2))))
	      ((NULL (TESTA PAT (OPIDENT OPIND) NIL))
	       (RESTORE))
              (IND (RESTORE1))
              ((RESTORE2))))
	(COND ((TESTA PAT (CADR Z) NIL)
	       (SETQ RES (CONS (CADR Z) RES))
	       (SAV&DEL Z))
	       (T (SETQ Z (CDR Z))))))
(DEFUN RESTORE NIL
	(DO ((Y (CDR ANS) (CDR Y)))
	    ((NULL Y) NIL)
		(COND ((EQ (CAR Y) '*LOOP)
		       (RPLACA Y (CADR Y))
		       (RPLACD Y (CDDR Y)))
		      ((NULL (CAR Y))
		       (SETQ ANS Y)
		       (RETURN NIL))
		      ((NULL (ATOM (CAAR Y)))
		       (RPLACD (CAAR Y) (CDAR Y))))))

(DEFUN RESTORE1 NIL
    (DO ((Y ANS) (L))				;L IS A LIST OF VAR'S NOTED
	((NULL (CDR Y)) T)
	(COND ((NULL (CADR Y))			;END OF CONTEXT
               (RPLACD Y (CDDR Y))		;SPLICE OUT THE CONTEXT MARKER
	       (RETURN T))
	      ((NOT (ATOM (CAADR Y)))		;FIXUP NECESSARY
	       (RPLACD (CAADR Y) (CDADR Y))	
	       (RPLACD Y (CDDR Y)))
	      ((MEMQ (CAR Y) L)			;THIS VAR HAS ALREADY BEEN SEEN
	       (RPLACD Y (CDDR Y)))		;SO SPLICE IT OUT TO KEEP ANS CLEAN
	      ((SETQ Y (CDR Y)
		     L (CONS (CAAR Y) L))))))

(DEFUN RESTORE2 NIL
     (DO ((Y (CDR ANS) (CDR Y)))
	 ((NULL (CDR Y)) T)
	 (COND ((EQ (CADR Y) '*LOOP)
		(RPLACD Y (CDDR Y)))
	       ((NULL (CADR Y))
		(RPLACD Y (CDDR Y))
		(RETURN T)))))

(DEFUN POP-LOOP-CONTEXT NIL
	(DO ((Y ANS))
	    ((EQ (CADR Y) '*LOOP) NIL)
	    (OR (ATOM (CAADR Y))
		(RPLACD (CAADR Y) (CDADR Y)))
	    (RPLACD Y (CDDR Y))))
;WHEN THE CAR OF ALA IS VAR* THE CADR IS A LIST OF
;THE VARIOUS SWITCHES WHICH MAY BE SET.  
;UVAR- INDICATES THIS SHOULD MATCH SOMETHING WHICH IS ALREADY ON ANS.
;SET - ACTUALLY SET THIS VARIABLE TO ITS VALUE IF IT MATCHES.
;COEFFPT - SPECIAL ARGUMENT IF IN COEFFPT.

(DEFUN TESTA (ALA EXP B)
	(COND ((EQ (CAR ALA) 'MVAR*)
	       (TESTA* ALA EXP T))
	      ((EQ (CAR ALA) 'VAR*)
	       (DO ((Z (CADR ALA) (CDR Z))
		    (ALA (CDDR ALA))
		    (Y) (SET) (UVAR))
		   ((NULL Z)
		    (SETQ Y (COND (UVAR (M1 EXP Y))
				  ((TESTA* ALA EXP NIL))))
		    (COND ((NULL Y) NIL)
			  (SET (SET (CAR ALA) EXP))
			  (Y)))
		   (COND ((EQ (CAR Z) 'SET) (SETQ SET T))
			 ((EQ (CAR Z) 'UVAR)
			  (COND ((SETQ Y (CDR (ASSOC (CAR ALA) ANS)))
				 (SETQ UVAR T))))
			 ((EQ (CAR Z) 'COEFFPT)
			  (AND (EQ B 'COEFFPT)
			       (SETQ ALA (CADR Z)))
			  (SETQ Z (CDR Z)))
			 ((MERROR "(INVALID SWITCH IN PATTERN TO SCHATCHEN)")))))
	      ((TESTA* ALA EXP NIL))))

; ALA IS THE PREDICATE LIST (VAR PREDFN ARG2 ARG3 ARG4 . . .)

(DEFUN TESTA* (ALA EXP LOC)
	(COND ((COND ((EQ (CADR ALA) 'FREEVAR)
		      (COND ((EQ VAR '*NOVAR) (EQUAL EXP 1))
			    ((FREE EXP VAR))))
		     ((EQ (CADR ALA) 'NUMBERP) (MNUMP EXP))
		     ((EQ (CADR ALA) 'TRUE) T)
		     ((EQ (CADR ALA) 'LINEAR*)
		      (SETQ EXP (LINEAR* EXP (CADDR ALA))))
		     ((NULL LOC)
		      (COND ((ATOM (CADR ALA))
			     (COND ((FBOUNDP (CADR ALA))
				    (APPLY (CADR ALA)
					   (FINDTHEM EXP (CDDR ALA))))
				   ((MGET (CADR ALA) 'MEXPR)
				    (MAPPLY (CADR ALA)
					    (FINDTHEM EXP (CDDR ALA))
					    (CADR ALA)))))
			    ((MEMQ (CAADR ALA) '(LAMBDA FUNCTION *FUNCTION QUOTE))
			     ;;;THE LAMBDA IS HERE ONLY BECAUSE OF SIN!!!
			     (APPLY (CADR ALA) (FINDTHEM EXP (CDDR ALA))))
			    ((EVAL-PRED (CADR ALA) (CAR ALA) EXP)))))
	       (COND ((MEMQ (CAR ALA) SPLIST))
		     ((ADD-TO (CAR ALA) EXP))))
	      ((COND ((AND LOC (ATOM (CADR ALA))
			   (FBOUNDP (CADR ALA)))
	       (MAPC '(LAMBDA (Q V) (AND (NULL (MEMQ Q SPLIST))
					 (ADD-TO Q V)))
		     (CAR ALA)
		     (APPLY (CADR ALA) (FINDTHEM EXP (CDDR ALA)))))))))

(DEFUN EVAL-PRED (EXP %VAR VALUE)
       (PROGV (LIST %VAR) (LIST VALUE)
	      (EVAL EXP)))

(DEFUN FINDTHEM (EXP ARGS)  
    (CONS EXP
	  (MAPCAR '(LAMBDA (Q)
			  (COND ((ATOM Q)
				 (OR (CDR (ASSQ Q ANS))
				     (EVAL Q)))
				( Q )))
		 ARGS)))

(DEFUN FINDIT (A)
	(DO ((Y ANS) (Z))
	    ((OR (NULL (CDR Y)) (NULL (CADR Y))) Z)
	    (COND ((EQ (CAADR Y) A)
		   (SETQ Z (NCONC Z (LIST (CDADR Y))))
		   (RPLACD Y (CDDR Y)))
		  ((SETQ Y (CDR Y))))))

(DEFUN SCH-REPLACE (DICT EXP1) (REPLAC EXP1))

(DEFUN REPLAC (EXP1)
   ((LAMBDA (W1)
	(COND ((NULL EXP1) NIL)
	      ((NOT (ATOM EXP1))
	       (COND ((EQ (CAR EXP1) 'EVAL)
		      (SIMPLIFYA (EVAL (REPLAC (CADR EXP1))) NIL))
		     ((EQ (CAR EXP1) 'QUOTE) (CADR EXP1))
		     (T (SETQ W1 (MAPCAR 'REPLAC (CDR EXP1)))
			(COND ((EQUAL W1 (CDR EXP1))
			       EXP1)
			      ((SIMPLIFYA (CONS (LIST (CAAR EXP1)) W1) T))))))
	      ((NUMBERP EXP1) EXP1)
	      ((SETQ W1 (ASSQ EXP1 DICT))
	       (CDR W1))
	      (EXP1)))
     NIL))

(DECLARE (UNSPECIAL VAR SPLIST DICT ANS BINDLIST SPECLIST))

(EVAL-WHEN (EVAL COMPILE) (SETQ IBASE OLD-IBASE))