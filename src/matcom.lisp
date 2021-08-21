;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp; Package: Macsyma -*- ;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macsyma-module matcom)

;; This is the Match Compiler.

(DECLARE (GENPREFIX MC_)
	 (SPECIAL *EXPR *RULES *RULELIST $RULES ALIST $PROPS 
		  *AFTERFLAG ARGS BOUNDLIST *A* PT
		  REFLIST TOPREFLIST PROGRAM $NOUNDISP))

(SETQ *AFTERFLAG NIL)

(DEFMSPEC $MATCHDECLARE (FORM)
  (LET ((META-PROP-P NIL))
    (PROC-$MATCHDECLARE (CDR FORM))))

(DEFUN PROC-$MATCHDECLARE (X)
 (IF (ODDP (LENGTH X))
     (MERROR "MATCHDECLARE takes an even number of arguments."))
 (DO X X (CDDR X) (NULL X)
     (COND ((EQ (TYPEP (CAR X)) 'SYMBOL)
	    (COND ((AND (NOT (EQ (TYPEP (CADR X)) 'SYMBOL))
		        (OR (NUMBERP (CADR X))
			    (MEMQ (CAAADR X) '(MAND MOR MNOT MCOND MPROG))))
		   (IMPROPER-ARG-ERR (CADR X) '$MATCHDECLARE)))
	    (META-ADD2LNC (CAR X) '$PROPS)
	    (META-MPUTPROP (CAR X) (NCONS (CADR X)) 'MATCHDECLARE))
	   ((NOT ($LISTP (CAR X)))
	    (IMPROPER-ARG-ERR (CAR X) '$MATCHDECLARE))
	   (T (DO L (CDAR X) (CDR L) (NULL L)
		  (PROC-$MATCHDECLARE (LIST (CAR L) (CADR X)))))))
'$DONE)

(DEFUN COMPILEATOM (E P) 
  (PROG (D) 
	(SETQ D (GETDEC P E))
	(RETURN (COND ((NULL D)
		       (EMIT (LIST 'COND
				   (LIST (LIST 'NOT
					       (LIST 'EQUAL
						     E
						     (LIST 'QUOTE P)))
					 '(MATCHERR)))))
		      ((MEMQ P BOUNDLIST)
		       (EMIT (LIST 'COND
				   (LIST (LIST 'NOT (LIST 'EQUAL E P))
					 '((MATCHERR))))))
		      (T (SETQ BOUNDLIST (CONS P BOUNDLIST)) (EMIT D))))))

(DEFUN EMIT (X) (SETQ PROGRAM (NCONC PROGRAM (LIST X))))

(DEFUN MEMQARGS (X)
  (COND ((OR (NUMBERP X) (MEMQ X BOUNDLIST)) X)
	((AND (SYMBOLP X) (GET X 'OPERATORS)) `(QUOTE ,X))
	;; ((NULL BOUNDLIST) (LIST 'SIMPLIFYA (LIST 'QUOTE X) NIL))
	(T `(MEVAL (QUOTE ,X)))))

(DEFUN MAKEPREDS (L GG) 
       (COND ((NULL L) NIL)
	     (T (CONS (COND ((ATOM (CAR L))
			     (LIST 'LAMBDA (LIST (SETQ GG (GENSYM))) (GETDEC (CAR L) GG)))
			    (T (DEFMATCH1 (CAR L) (GENSYM))))
		      (MAKEPREDS (CDR L) NIL)))))

(DEFUN DEFMATCH1 (PT E) 
       (PROG (TOPREFLIST PROGRAM) 
	     (SETQ TOPREFLIST (LIST E))
	     (COND ((ATOM (ERRSET (COMPILEMATCH E PT)))
		    (merror "Match processing aborted~%"))
		   (T (mtell
"~M Will be matched uniquely since sub-parts would otherwise be ambigious.~%" 
  
PT)
		      (RETURN (LIST 'LAMBDA
				    (LIST E)
				  (LIST '*CATCH ''MATCH
					(NCONC (LIST 'PROG)
					       (LIST (CDR (REVERSE TOPREFLIST)))
					       PROGRAM
					       (LIST (LIST 'RETURN T))))))))))

(DEFUN COMPILEPLUS (E P) 
       (PROG (REFLIST F G H FLAG LEFTOVER) 
	A    (SETQ P (CDR P))
	A1   (COND ((NULL P)
		    (COND ((NULL LEFTOVER)
			   (RETURN (EMIT (LIST 'COND
					       (LIST (LIST 'NOT (LIST 'EQUAL E 0.))
						     '(MATCHERR))))))
			  ((NULL (CDR LEFTOVER)) (RETURN (COMPILEMATCH E (CAR LEFTOVER))))
			  ((SETQ F (INTERSECT LEFTOVER BOUNDLIST))
			   (EMIT (LIST 'SETQ
				       E
				       (LIST 'MEVAL
					     (LIST 'QUOTE
						   (LIST '(MPLUS)
							 E
							 (LIST '(MMINUS) (CAR F)))))))
			   (DELETE (CAR F) LEFTOVER)
			   (GO A1))
			  (T
			   (MTELL "~M partitions SUM"
				  (CONS '(MPLUS) LEFTOVER)
				  )
			   (SETQ BOUNDLIST (APPEND BOUNDLIST (ATOMSON LEFTOVER)))
			     (RETURN (EMIT (LIST 'COND
						 (LIST (LIST 'PART+
							     E
							     (LIST 'QUOTE LEFTOVER)
							     (LIST 'QUOTE
								   (MAKEPREDS LEFTOVER NIL))))
						 '(T (MATCHERR))))))))
		   ((FIXEDMATCHP (CAR P))
		    (EMIT (LIST 'SETQ
				E
				(LIST 'MEVAL
				      (LIST 'QUOTE
					    (LIST '(MPLUS)
						  E
						  (LIST '(MMINUS) (CAR P))))))))
		   ((ATOM (CAR P))
		    (COND ((CDR P) (SETQ LEFTOVER (CONS (CAR P) LEFTOVER)) (SETQ P (CDR P)) (GO A1))
			  (LEFTOVER (SETQ LEFTOVER (CONS (CAR P) LEFTOVER)) (SETQ P NIL) (GO A1)))
		    (SETQ BOUNDLIST (CONS (CAR P) BOUNDLIST))
		    (EMIT (GETDEC (CAR P) E))
		    (COND ((NULL (CDR P)) (RETURN NIL)) (T (GO A))))
		   ((EQ (CAAAR P) 'MTIMES)
		    (COND ((AND (NOT (OR (NUMBERP (CADAR P))
					 (AND (NOT (ATOM (CADAR P)))
					      (EQ (CAAR (CADAR P)) 'RAT))))
				(FIXEDMATCHP (CADAR P)))
			   (SETQ FLAG NIL)
			   (EMIT `(SETQ ,(GENREF)
					(RATDISREP
					 (RATCOEF ,E ,(MEMQARGS (CADAR P))))))
			   (COMPILETIMES (CAR REFLIST) (CONS '(MTIMES) (CDDAR P)))
			   (EMIT `(SETQ ,E (MEVAL
					    (QUOTE
					     (($RATSIMP)
					      ((MPLUS) ,E
						       ((MTIMES) -1 ,(CAR REFLIST)
								    ,(CADAR P)))))))))
			  ((NULL FLAG)
			   (SETQ FLAG T) (RPLACD (CAR P) (REVERSE (CDAR P))) (GO A1))
			  (T (SETQ LEFTOVER (CONS (CAR P) LEFTOVER)) (GO A))))
		   ((EQ (CAAAR P) 'MEXPT)
		    (COND ((FIXEDMATCHP (CADAR P))
			   (SETQ F 'FINDEXPON)
			   (SETQ G (CADAR P))
			   (SETQ H (CADDAR P)))
			  ((FIXEDMATCHP (CADDAR P))
			   (SETQ F 'FINDBASE)
			   (SETQ G (CADDAR P))
			   (SETQ H (CADAR P)))
			  (T (GO FUNCTIONMATCH)))
		    (EMIT (LIST 'SETQ
				(GENREF)
				(LIST F E (SETQ G (MEMQARGS G)) ''MPLUS)))
		    (EMIT (LIST 'SETQ
				E
				(LIST 'MEVAL
				      (LIST 'QUOTE
					    (LIST '(MPLUS)
						  E
						  (LIST '(MMINUS)
							(COND ((EQ F 'FINDEXPON)
							       (LIST '(MEXPT)
								     G
								     (CAR REFLIST)))
							      (T (LIST '(MEXPT)
								       (CAR REFLIST)
								       G)))))))))
		    (COMPILEMATCH (CAR REFLIST) H))
		   ((NOT (FIXEDMATCHP (CAAAR P)))
		    (COND ((CDR P)
			   (SETQ LEFTOVER (CONS (CAR P) LEFTOVER))
			   (SETQ P (CDR P))
			   (GO A1)))
		    (SETQ BOUNDLIST (CONS (CAAAR P) BOUNDLIST))
		    (EMIT (LIST 'MSETQ
				(CAAAR P)
				(LIST 'KAR (LIST 'KAR (GENREF)))))
		    (GO FUNCTIONMATCH))
		   (T (GO FUNCTIONMATCH)))
	     (GO A)
	FUNCTIONMATCH
	     (EMIT (LIST 'SETQ
			 (GENREF)
			 (LIST 'FINDFUN E (MEMQARGS (CAAAR P)) ''MPLUS)))
	     (COND ((EQ (CAAAR P) 'MPLUS)
		    (MTELL "~M~%Warning: + within +~%" (CAR P))
		    (COMPILEPLUS (CAR REFLIST) (CAR P)))
		   (T (EMIT (LIST 'SETQ (GENREF) (LIST 'KDR (CADR REFLIST))))
		      (COMPILEEACH (CAR REFLIST) (CDAR P))))
	     (EMIT (LIST 'SETQ
			 E
			 (LIST 'MEVAL
			       (LIST 'QUOTE
				     (LIST '(MPLUS) E (LIST '(MMINUS) (CAR P)))))))
	     (GO A)))

(DEFUN COMPILETIMES (E P) 
       (PROG (REFLIST F G H LEFTOVER) 
	A    (SETQ P (CDR P))
	A1   (COND ((NULL P)
		    (COND ((NULL LEFTOVER)
			   (RETURN (EMIT (LIST 'COND
					       (LIST (LIST 'NOT (LIST 'EQUAL E 1.))
						     '(MATCHERR))))))
			  ((NULL (CDR LEFTOVER)) (RETURN (COMPILEMATCH E (CAR LEFTOVER))))
			  ((SETQ F (INTERSECT LEFTOVER BOUNDLIST))
			   (EMIT (LIST 'SETQ
				       E
				       (LIST 'MEVAL
					     (LIST 'QUOTE
						   (LIST '(MQUOTIENT) E (CAR F))))))
			   (DELETE (CAR F) LEFTOVER)
			   (GO A1))
			  (T
			   (MTELL "~M partitions PRODUCT"
				  (CONS '(MTIMES) LEFTOVER)
				  )
			   (SETQ BOUNDLIST (APPEND BOUNDLIST (ATOMSON LEFTOVER)))
			     (RETURN (EMIT (LIST 'COND
						 (LIST (LIST 'PART*
							     E
							     (LIST 'QUOTE LEFTOVER)
							     (LIST 'QUOTE
								   (MAKEPREDS LEFTOVER NIL))))
						 '(T (MATCHERR))))))))
		   ((FIXEDMATCHP (CAR P))
		    (EMIT (LIST 'SETQ
				E
				(LIST 'MEVAL
				      (LIST 'QUOTE (LIST '(MQUOTIENT) E (CAR P)))))))
		   ((ATOM (CAR P))
		    (COND ((CDR P) (SETQ LEFTOVER (CONS (CAR P) LEFTOVER)) (SETQ P (CDR P)) (GO A1))
			  (LEFTOVER (SETQ LEFTOVER (CONS (CAR P) LEFTOVER)) (SETQ P NIL) (GO A1)))
		    (SETQ BOUNDLIST (CONS (CAR P) BOUNDLIST))
		    (EMIT (GETDEC (CAR P) E))
		    (COND ((NULL (CDR P)) (RETURN NIL)) (T (GO A))))
		   ((EQ (CAAAR P) 'MEXPT)
		    (COND ((FIXEDMATCHP (CADAR P))
			   (SETQ F 'FINDEXPON)
			   (SETQ G (CADAR P))
			   (SETQ H (CADDAR P)))
			  ((FIXEDMATCHP (CADDAR P))
			   (SETQ F 'FINDBASE)
			   (SETQ G (CADDAR P))
			   (SETQ H (CADAR P)))
			  (T (GO FUNCTIONMATCH)))
		    (EMIT (LIST 'SETQ
				(GENREF)
				(LIST F E (SETQ G (MEMQARGS G)) ''MTIMES)))
		    (COND ((EQ F 'FINDBASE)
			   (EMIT (LIST 'COND
				       (LIST (LIST 'EQUAL (CAR REFLIST) 0)
					     '(MATCHERR))))))
		    (EMIT (LIST 'SETQ
				E
				(LIST 'MEVAL
				      (LIST 'QUOTE
					    (LIST '(MQUOTIENT)
						  E
						  (COND ((EQ F 'FINDEXPON)
							 (LIST '(MEXPT) G (CAR REFLIST)))
							(T (LIST '(MEXPT)
								 (CAR REFLIST)
								 G))))))))
		    (COMPILEMATCH (CAR REFLIST) H))
		   ((NOT (FIXEDMATCHP (CAAAR P)))
		    (COND ((CDR P)
			   (SETQ LEFTOVER (CONS (CAR P) LEFTOVER))
			   (SETQ P (CDR P))
			   (GO A1)))
		    (SETQ BOUNDLIST (CONS (CAAAR P) BOUNDLIST))
		    (EMIT (LIST 'MSETQ
				(CAAAR P)
				(LIST 'KAR (LIST 'KAR (GENREF)))))
		    (GO FUNCTIONMATCH))
		   (T (GO FUNCTIONMATCH)))
	     (GO A)
	FUNCTIONMATCH
	     (EMIT (LIST 'SETQ
			 (GENREF)
			 (LIST 'FINDFUN E (MEMQARGS (CAAAR P)) ''MTIMES)))
	     (COND ((EQ (CAAAR P) 'MTIMES)
		    (MTELL "~M~%Warning: * within *" (CAR P))
		    (COMPILETIMES (CAR REFLIST) (CAR P)))
		   (T (EMIT (LIST 'SETQ (GENREF) (LIST 'KDR (CADR REFLIST))))
		      (COMPILEEACH (CAR REFLIST) (CDAR P))))
	     (EMIT (LIST 'SETQ
			 E
			 (LIST 'MEVAL
			       (LIST 'QUOTE (LIST '(MQUOTIENT) E (CAR P))))))
	     (GO A)))


(DEFMSPEC $DEFMATCH (FORM)
  (LET ((META-PROP-P NIL))
    (PROC-$DEFMATCH (CDR FORM))))

(DEFUN PROC-$DEFMATCH (L) 
  (PROG (PT PT* ARGS *A* BOUNDLIST REFLIST TOPREFLIST PROGRAM NAME) 
	(SETQ NAME (CAR L))
	(SETQ PT (COPY (SETQ PT* (SIMPLIFY (CADR L)))))
	(COND ((ATOM PT)
	       (SETQ PT (COPY (SETQ PT* (MEVAL PT))))
	       (MTELL "~M~%Is the pattern~%" PT)
	       ))
	(SETQ ARGS (CDDR L))
	(COND ((NULL (ALLATOMS ARGS)) (MTELL "Non-atomic pattern variables")
				      (RETURN NIL)))
	(SETQ BOUNDLIST ARGS)
	(SETQ *A* (GENREF))
	(COND ((ATOM (ERRSET (COMPILEMATCH *A* PT)))
	       (merror "Match processing aborted~%"))
	      (T (META-FSET NAME
		       (LIST 'LAMBDA
			     (CONS *A* ARGS)
			     (LIST '*CATCH ''MATCH
				   (NCONC (LIST 'PROG)
					  (LIST (CDR (REVERSE TOPREFLIST)))
					  PROGRAM
					  (LIST (LIST 'RETURN
						      (COND (BOUNDLIST (CONS 'RETLIST
									     BOUNDLIST))
							    (T T))))))))
		 (META-ADD2LNC NAME '$RULES)
		 (META-MPUTPROP NAME (LIST '(MLIST) PT* (CONS '(MLIST) ARGS)) '$RULE)
		 (RETURN NAME)))))

(DEFUN ATOMSON (L) 
       (COND ((NULL L) NIL)
	     ((ATOM (CAR L)) (CONS (CAR L) (ATOMSON (CDR L))))
	     (T (ATOMSON (CDR L)))))


(DEFMSPEC $TELLSIMP (FORM)
  (LET ((META-PROP-P NIL))
    (PROC-$TELLSIMP (CDR FORM))))

(DEFUN PROC-$TELLSIMP (L) 
 (PROG (PT RHS BOUNDLIST REFLIST TOPREFLIST *A* PROGRAM NAME
	   OLDSTUFF PGNAME ONAME RULENUM) 
  (SETQ PT (COPY (SIMPLIFYA (CAR L) NIL)))
  (SETQ NAME PT)
  (SETQ RHS (COPY (SIMPLIFYA (CADR L) NIL)))
  (COND ((ALIKE1 PT RHS) (MERROR "Circular rule attempted - TELLSIMP"))
	((OR (ATOM PT) (MGET (SETQ NAME (CAAR PT)) 'MATCHDECLARE))
	 (MERROR "~%~A unsuitable~%" (FULLSTRIP1 (GETOP NAME))))
	((MEMQ NAME '(MPLUS MTIMES))
	 (MTELL "Warning: Putting rules on '+' or '*' is inefficient, and may not work.~%")))
  (SETQ *A* (GENREF))
  (COND ((ATOM (ERRSET (COMPILEEACH *A* (CDR PT))))
	 (MERROR "Match processing aborted~%")))
  (SETQ OLDSTUFF (GET NAME 'OPERATORS))
  (SETQ RULENUM (MGET NAME 'RULENUM))
  (COND ((NULL RULENUM) (SETQ RULENUM 1.)))
  (SETQ ONAME (GETOP NAME))
  (SETQ PGNAME (IMPLODE (APPEND (%TO$ (EXPLODEC ONAME))
				'(R U L E)
				(MEXPLODEN RULENUM))))
  (META-MPUTPROP PGNAME NAME 'RULEOF)
  (META-ADD2LNC PGNAME '$RULES)
  (META-MPUTPROP NAME (ADD1 RULENUM) 'RULENUM)
  (META-FSET PGNAME
	(LIST 'LAMBDA '(X A2 A3)
	      (LIST 'PROG
		    (LIST 'ANS *A*)
		    (LIST 'SETQ
			  'X
			  (LIST 'CONS
				'(CAR X)
				(LIST 'SETQ
				      *A*
				      '(COND (A3 (CDR X)) 
					     (T (MAPCAR #'(LAMBDA (H) (SIMPLIFYA H A3))
							(CDR X)))))))
		    (LIST
		     'SETQ
		     'ANS
		     (LIST '*CATCH ''MATCH
			   (NCONC (LIST 'PROG)
				  (LIST (NCONC BOUNDLIST
					       (CDR (REVERSE TOPREFLIST))))
				  PROGRAM
				  (LIST (LIST 'RETURN
					      (MEMQARGS RHS))))))
		    (COND ((NOT (MEMQ NAME '(MTIMES MPLUS)))
			   (LIST 'RETURN
				 (LIST 'COND
				       '(ANS) '((AND (NOT DOSIMP) (MEMQ 'SIMP (CDAR X)))X)
				       (LIST T
					     (COND (OLDSTUFF (CONS OLDSTUFF
								   '(X A2 T)))
						   (T '(EQTEST X X)))))))
			  ((EQ NAME 'MTIMES)
			   (LIST 'RETURN
				 (LIST 'COND
				       '((AND (EQUAL 1. A2) ANS))
				       '(ANS (MEVAL '((MEXPT) ANS A2)))
				       (LIST T
					     (COND (OLDSTUFF (CONS OLDSTUFF
								   '(X A2 A3)))
						   (T '(EQTEST X X)))))))
			  ((EQ NAME 'MPLUS)
			   (LIST 'RETURN
				 (LIST 'COND
				       '((AND (EQUAL 1. A2) ANS))
				       '(ANS (MEVAL '((MTIMES) ANS A2)))
				       (LIST T
					     (COND (OLDSTUFF (CONS OLDSTUFF
								   '(X A2 A3)))
						   (T '(EQTEST X X)))))))))))
  (META-MPUTPROP PGNAME (LIST '(MEQUAL) PT RHS) '$RULE)
  (COND ((NULL (MGET NAME 'OLDRULES))
	 (META-MPUTPROP NAME
		   (LIST (GET NAME 'OPERATORS))
		   'OLDRULES)))
  (META-PUTPROP NAME PGNAME 'OPERATORS)
  (RETURN (CONS '(MLIST)
		(META-MPUTPROP NAME
			  (CONS PGNAME (MGET NAME 'OLDRULES))
			  'OLDRULES)))))

(DEFUN %TO$ (L) (COND ((EQ (CAR L) '%) (RPLACA L '$)) (L)))


(DEFMSPEC $TELLSIMPAFTER (FORM)
  (LET ((META-PROP-P NIL))
    (PROC-$TELLSIMPAFTER (CDR FORM))))

(DEFUN PROC-$TELLSIMPAFTER (L) 
  (PROG (PT RHS BOUNDLIST REFLIST TOPREFLIST *A* PROGRAM NAME OLDSTUFF PLUSTIMES PGNAME ONAME
	 RULENUM) 
	(SETQ PT (COPY (SIMPLIFYA (CAR L) NIL)))
	(SETQ NAME PT)
	(SETQ RHS (COPY (SIMPLIFYA (CADR L) NIL)))
	(COND ((ALIKE1 PT RHS) (MERROR "Circular rule attempted - TELLSIMPAFTER"))
	      ((OR (ATOM PT) (MGET (SETQ NAME (CAAR PT)) 'MATCHDECLARE))
	       (MERROR "~%~A unsuitable~%" (FULLSTRIP1 (GETOP NAME)))))
	(SETQ *A* (GENREF))
	(SETQ PLUSTIMES (MEMQ NAME '(MPLUS MTIMES)))
	(IF (ATOM (IF PLUSTIMES (ERRSET (COMPILEMATCH *A* PT))
				(ERRSET (COMPILEEACH *A* (CDR PT)))))
	    (MERROR "Match processing aborted~%"))
	(SETQ OLDSTUFF (GET NAME 'OPERATORS))
	(SETQ RULENUM (MGET NAME 'RULENUM))
	(IF (NULL RULENUM) (SETQ RULENUM 1))
	(SETQ ONAME (GETOP NAME))
	(SETQ PGNAME (IMPLODE (APPEND (%TO$ (EXPLODEC ONAME))
				      '(R U L E) (MEXPLODEN RULENUM))))
	(META-MPUTPROP PGNAME NAME 'RULEOF)
	(META-ADD2LNC PGNAME '$RULES)
	(META-MPUTPROP NAME (ADD1 RULENUM) 'RULENUM)
	(META-FSET
	 PGNAME
	 (LIST
	  'LAMBDA
	  '(X ANS A3)
	  (IF OLDSTUFF (LIST 'SETQ 'X (LIST OLDSTUFF 'X 'ANS 'A3)))
	  (LIST
	   'COND
	   '(*AFTERFLAG X)
	   (LIST 'T
		 (NCONC (LIST 'PROG)
			(LIST (CONS *A* '(*AFTERFLAG)))
			(LIST '(SETQ *AFTERFLAG T))
			(COND (OLDSTUFF (SUBST (LIST 'QUOTE NAME)
					       'NAME
					       '((COND ((OR (ATOM X) (NOT (EQ (CAAR X) NAME)))
							(RETURN X)))))))
			(LIST (LIST 'SETQ
				    *A*
				    (COND (PLUSTIMES 'X) (T '(CDR X)))))
			(LIST (LIST 'SETQ
				    'ANS
				  (LIST '*CATCH ''MATCH
					(NCONC (LIST 'PROG)
					       (LIST (NCONC BOUNDLIST
							    (CDR (REVERSE TOPREFLIST))))
					       PROGRAM
					       (LIST (LIST 'RETURN
							   (MEMQARGS RHS)))))))
			(LIST '(RETURN (OR ANS (EQTEST X X)))))))))
	(META-MPUTPROP PGNAME (LIST '(MEQUAL) PT RHS) '$RULE)
	(COND ((NULL (MGET NAME 'OLDRULES))
	       (META-MPUTPROP NAME (LIST (GET NAME 'OPERATORS)) 'OLDRULES)))
	(META-PUTPROP NAME PGNAME 'OPERATORS)
		(RETURN (CONS '(MLIST)
		      (META-MPUTPROP NAME
				(CONS PGNAME (MGET NAME 'OLDRULES))
				'OLDRULES)))))

(DEFMSPEC $DEFRULE (FORM)
  (LET ((META-PROP-P NIL))
    (PROC-$DEFRULE (CDR FORM))))

(DEFUN PROC-$DEFRULE (L) 
 (PROG (PT RHS BOUNDLIST REFLIST TOPREFLIST NAME *A* PROGRAM LHS* RHS*) 
       (IF (NOT (= (LENGTH L) 3)) (WNA-ERR '$DEFRULE))
       (SETQ NAME (CAR L))
       (IF (OR (NOT (SYMBOLP NAME)) (MOPP NAME) (MEMQ NAME '($ALL $%)))
	   (MERROR "Improper rule name:~%~M" NAME))
       (SETQ PT (COPY (SETQ LHS* (SIMPLIFY (CADR L)))))
       (SETQ RHS (COPY (SETQ RHS* (SIMPLIFY (CADDR L)))))
       (SETQ *A* (GENREF))
       (COND ((ATOM (ERRSET (COMPILEMATCH *A* PT)))
	      (MERROR "Match processing aborted~%"))
	     (T (META-FSET NAME
		      (LIST 'LAMBDA
			    (LIST *A*)
			    (LIST '*CATCH ''MATCH
				  (NCONC (LIST 'PROG)
					 (LIST (NCONC BOUNDLIST
						      (CDR (REVERSE TOPREFLIST))))
					 PROGRAM
					 (LIST (LIST 'RETURN
						     (MEMQARGS RHS)))))))
		(META-ADD2LNC NAME '$RULES)
		(META-MPUTPROP NAME (SETQ L (LIST '(MEQUAL) LHS* RHS*)) '$RULE)
		(META-MPUTPROP NAME '$DEFRULE '$RULETYPE)
		(RETURN (LIST '(MSETQ) NAME (CONS '(MARROW) (CDR L))))))))

(DEFUN GETDEC (P E) 
  (LET (X Z) 
       (COND ((SETQ X (MGET P 'MATCHDECLARE))
	      (COND ((NOT (ATOM (CAR X))) (SETQ X (CAR X))))
	      (SETQ Z (NCONC (MAPCAR 'MEMQARGS (CDR X)) (NCONS E)))
	      (SETQ X (CAR X))
	      (COND ((NOT (ATOM X)) (SETQ X (CAR X))))
	      (SETQ Z
		    (COND ((OR (MEMQ X '($TRUE T $ALL))
			       (AND (FBOUNDP X) (NOT (GET X 'TRANSLATED))))
			   (CONS X Z))
			  (T (LIST 'IS (LIST 'QUOTE (CONS (NCONS X) Z))))))
	      (COND ((MEMQ (CAR Z) '($TRUE T $ALL)) (LIST 'MSETQ P E))
		    (T (LIST 'COND
			     (LIST Z (LIST 'MSETQ P E))
			     '((MATCHERR)))))))))
(DEFUN COMPILEMATCH (E P) 
       (PROG (REFLIST) 
	     (COND ((FIXEDMATCHP P)
		    (EMIT (LIST 'COND
				(LIST (LIST 'NOT
					    (LIST 'ALIKE1
						  E
						  (LIST 'MEVAL (LIST 'QUOTE
							P))))
				      '(MATCHERR)))))
		   ((ATOM P) (COMPILEATOM E P))
		   ((EQ (CAAR P) 'MPLUS) (COMPILEPLUS E P))
		   ((EQ (CAAR P) 'MTIMES) (COMPILETIMES E P))
		   ((AND (EQ (CAAR P) 'MEXPT)
			 (FIXEDMATCHP (CADR P)))
		    (EMIT (LIST 'SETQ
				(GENREF)
				(LIST 'FINDEXPON
				      E
				      (MEMQARGS (CADR P))
				      ''MEXPT)))
		    (COMPILEMATCH (CAR REFLIST) (CADDR P)))
		   ((AND (EQ (CAAR P) 'MEXPT)
			 (FIXEDMATCHP (CADR P)))
		    (EMIT (LIST 'SETQ
				(GENREF)
				(LIST 'FINDBASE
				      E
				      (MEMQARGS (CADDR P))
				      ''MEXPT)))
		    (COMPILEMATCH (CAR REFLIST) (CADR P)))
		   ((EQ (CAAR P) 'MEXPT)
		    (EMIT (LIST 'SETQ
				(GENREF)
				(LIST 'FINDBE E)))
		    (EMIT (LIST 'SETQ
				(GENREF)
				(LIST 'KAR (CADR REFLIST))))
		    (COMPILEMATCH (CAR REFLIST) (CADR P))
		    (EMIT (LIST 'SETQ
				(CADR REFLIST)
				(LIST 'KDR (CADR REFLIST))))
		    (COMPILEMATCH (CADR REFLIST) (CADDR P)))
		   (T (COMPILEATOM (LIST 'KAR
					 (LIST 'KAR E))
				   (CAAR P))
		      (EMIT (LIST 'SETQ
				  (GENREF)
				  (LIST 'KDR E)))
		      (COMPILEEACH (CAR REFLIST) (CDR P))))
	     (RETURN PROGRAM)))

(DEFUN GENREF NIL 
	(PROG (A) 
	   (SETQ A (GENSYM))
	   (SETQ TOPREFLIST (CONS A TOPREFLIST))
	   (RETURN (CAR (SETQ REFLIST (CONS A REFLIST))))))
(DEFUN COMPILEEACH (ELIST PLIST) 
	 (PROG (REFLIST COUNT) 
	       (SETQ COUNT 0)
	       (SETQ REFLIST (CONS ELIST REFLIST))
	  A    (SETQ COUNT (ADD1 COUNT))
	       (COND ((NULL PLIST)
		      (RETURN (EMIT (LIST 'COND
					  (LIST (LIST 'NTHKDR ELIST (SUB1 COUNT))
						'(MATCHERR)))))))
	       (EMIT (LIST 'SETQ (GENREF) (LIST 'KAR (CADR REFLIST))))
	       (COMPILEMATCH (CAR REFLIST) (CAR PLIST))
	       (SETQ PLIST (CDR PLIST))
	       (SETQ REFLIST (CONS (LIST 'KDR (CADR REFLIST)) REFLIST))
	       (GO A)))

(DEFUN FIXEDMATCHP (X)
  (COND ((NUMBERP X) T)
	((ATOM X)
	 (IF (OR (MEMQ X BOUNDLIST) (NULL (MGET X 'MATCHDECLARE))) T))
	(T (AND (OR (MEMQ (CAAR X) BOUNDLIST)
		    (NULL (MGET (CAAR X) 'MATCHDECLARE)))
		(FMP1 (CDR X))))))

(DEFUN FMP1 (X) (IF (NULL X) T (AND (FIXEDMATCHP (CAR X)) (FMP1 (CDR X)))))