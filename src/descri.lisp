;;; -*- Mode:LISP; Package:MACSYMA -*-

;	** (c) Copyright 1981 Massachusetts Institute of Technology **

(macsyma-module descri)

(DECLARE (SPLITFILE DESCR))

;;; Updated for New-I/O by KMP, 5:31pm  Tuesday, 8 August 1978
;;; Updated for FILEPOSing by RLB, 20 December 1978
;;; Updated for Multics by putting the index to the doc on the plist of the
;;; symbol being doc'ed by JIM 25 Oct. 1980.

;;; This version will allow  (control-Q) to quote an & in the
;;; doc file.  It first reads MANUAL;MACSYM BINDEX (prepared by doing
;;; :L MANUAL;MANDEX) to find out where in 
;;; MANUAL;MACSYM DOC to look.  It then reads the latter file
;;; for the entries found in the index.  The entry is printed by TYI'ing
;;; chars to the next (non-quoted) "&" in the file. Elements which are
;;; not Macsyma keywords will not be searched for. Any elements which are 
;;; not found will be noted explicitly.
;;; The format of the index file is found in comments in RLB;MANDEX .

;;; This version runs most of the old $DESCRIBE (here named ODESCRIBE)
;;; as a fallback if the index info is out of date.

(DEFMSPEC $DESCRIBE (NODES) (SETQ NODES (CDR NODES))
  (DO ((N NODES (CDR N)) (L) (X))
      ((NULL N) (SETQ NODES (NREVERSE L)))
      (SETQ X (CAR N))
      (COND ((SYMBOLP X) (PUSH (prepare-a-node x) L))
	    (T (MTELL "~&Non-atomic arg being ignored: ~M" X)
	       )))
  (COND ((NULL NODES) (SETQ NODES (NCONS 'DESCRIBE))))
  (CURSORPOS 'A)
  (LET ((L (LOCATE-INDEX-INFO NODES #+ITS'((DSK MAXOUT) MACSYM BINDEX)
			            #-ITS ()))
	(F))
       (SETQ F (CAR L) L (CDR L))
       (COND ((NULL F) 
	      (PRINC
	       "Description index is out of date, this may take a lot longer.")
	      (ODESCRIBE NODES))
	     ('T (DO ((L L (CDR L))) ((NULL L) (CLOSE F))
		     (COND ((ATOM (CAR L))
			    (MTELL "No info for ~A~%" (CAR L)))
			   ((DO POS (CAR L) (CDR POS) (NULL POS)
				(TERPRI)
				(FILEPOS F (CAR POS))
				(DO C (TYI F -1) (TYI F -1) ()
				    (CASEQ C
					   (#/ (TYO (TYI F)))
					   ((#/& -1) (RETURN 'T))
					   (#o14 () )	;^L
					   (T (TYO C)))))))))))
  '$DONE)

#-Multics
(DEFUN UPCASE-FULLSTRIP1 (X)
  (IMPLODE
   (MAP #'(LAMBDA (CHS)
	    (COND ((< (CAR CHS) #/a))
		  ((> (CAR CHS) #/z))
		  (T (RPLACA CHS (- (CAR CHS)
				    #.(- #/a #/A))))))
	(EXPLODEN (FULLSTRIP1 X)))))

#-Multics
(DEFUN LH-BITS MACRO (FORM) `(BOOLE 1 #o777777 (LSH ,(CADR FORM) -18.)))
#-Multics
(DEFUN RH-BITS MACRO (FORM) `(BOOLE 1 #o777777 ,(CADR FORM)))

#-Multics
(defun prepare-a-node (x)
  (COND ((= (GETCHARN X 1) #/&) (UPCASE-FULLSTRIP1 X))
        (T (FULLSTRIP1 X))))

#+Multics 
(defun prepare-a-node (x)
  (setq x (downcase-it (fullstrip1 x)));For strings and to get the alias's.
  (implode (cons #/$ (explode x))))

#+Multics
(defun downcase-it (x)
  (IMPLODE
   (MAP #'(LAMBDA (CHS)
	    (COND ((< (CAR CHS) #/A))
		  ((> (CAR CHS) #/Z))
		  (T (RPLACA CHS (+ (CAR CHS)
				    #.(- #/a #/A))))))
	     (EXPLODEN X))))

;;;Return
;;; (open-file-obj-or-NIL . (list of (list of starting pos's) or losing-atom))
#+Multics
(defun locate-index-info (nodes f)
  f ;IGNORED
  (cond ((not (get '$describe 'user-doc))
	 (mtell "Loading DESCRIBE data-base, please be patient.~%")
	 (load-documentation-file manual-index)))
  (setq nodes (sort (append nodes ()) 'alphalessp))
  (do ((l nodes (cdr l))
       (locations ()))
      ((null l) (return (cons (open (find-documentation-file manual)
				    '(in ascii)) 
			      locations)))
    (let ((item-location (and (symbolp (car l))
			      (get (car l) 'user-doc))))
      (push (if (not (null item-location)) 
	      (ncons item-location) 
	      (car l))
	    locations))))

#-Multics
(DEFUN LOCATE-INDEX-INFO (NODES F)
  (SETQ NODES (SORT (APPEND NODES ()) 'ALPHALESSP) F (OPEN F '(IN FIXNUM)))
  (LET ((FILE (DO ((I (IN F) (1- I)) (L))	;Grab file name
		  ((< I 1) (PNPUT (NREVERSE L) 7))
		(PUSH (IN F) L)))
	(CDATE (IN F)) (FPINDEX (FILEPOS F)))
    CDATE
    (DO ((L NODES (CDR L)) (PN) (1STCH 0) (NENT 0) (RET))
	((NULL L))
      ;(DECLARE (FIXNUM NENT 1STCH))
      (SETQ 1STCH (GETCHARN (CAR L) 1) PN (PNGET (CAR L) 7))
      (FILEPOS F (+ FPINDEX 1STCH))	;Pos to index-to-the-index
      (SETQ NENT (IN F))
      (COND ((NOT (= 0 NENT))
	     (FILEPOS F (RH-BITS NENT))	;Pos to the entries
	     (SETQ NENT (LH-BITS NENT))
	     (DO I 1 (1+ I) (> I NENT)	;Check all entries
		 (LET ((LPNAME (IN F)) (NSTARTS 0) (FOUND 'T))
		   (SETQ NSTARTS (RH-BITS LPNAME)
			 LPNAME (LH-BITS LPNAME))
		   ;;Read in LPNAME file entry pname words,
		   ;;comparing word-by-word with pname list of the
		   ;;symbol.  Assume they all match (FOUND=T) unless
		   ;;(a) a mismatch is found
		   ;;(b) pname list of symbol ran out before LPNAME
		   ;;    words were read from the file
		   ;;(c) any pname list words left when all words
		   ;;    read from the file
		   (DO ((I 1 (1+ I)) (PN PN (CDR PN)))
		       ((> I LPNAME)		;Read pname of entry
			(AND PN (SETQ FOUND ())))
		     (COND ((NULL PN) (SETQ FOUND ()) (IN F))
			   ((NOT (= (CAR PN) (IN F)))
			    (SETQ FOUND ()))))
		   ;;If we found the one, read in all the starts and
		   ;;return a list of them.  If we didn't find it, we
		   ;;need too read in all the starts anyway (dumb
		   ;;filepos) but remember that simple DO returns nil.
		   (COND (FOUND (DO ((I 1 (1+ I)) (L))
				    ((> I NSTARTS)
				     (SETQ RET (NREVERSE L)))
				  (PUSH (IN F) L)))
			 ((SETQ RET (DO I 1 (1+ I) (> I NSTARTS)
					(IN F))))))
		 (COND (RET (RPLACA L RET) (RETURN 'T)))))))
    (CLOSE F)
    (SETQ FILE '((DSK MAXOUT) MACSYM DOC))
    (SETQ F (OPEN FILE '(IN ASCII)))
;    (COND ((NOT (= CDATE (CAR (SYSCALL 1 'RFDATE F)))) ; Twenex doesn't like
;	   (CLOSE F) (SETQ F ()))) ;this and we don't need it anyway.
    (CONS F NODES)))

(DEFMFUN MDESCRIBE (X) (MEVAL `(($DESCRIBE) ,X)))

;;;ODESCRIBE is mostly like the old $DESCRIBE, except the arg checking
;;; has already been done, and it is a SUBR.

(DEFUN ODESCRIBE (NODES)
  (TERPRI)
  (COND ((NOT NODES) (ERROR "Nothing to describe!")))
  (CURSORPOS 'A)
  (PRINC "Checking...")
  (TERPRI)
  (PROG (STREAM EOF)
   (SETQ STREAM (OPEN "documentation>macsyma.manual" '(IN ASCII)))
   (SETQ EOF (GENSYM))
   (*CATCH 'END-OF-FILE
	   (DO ((FORM (READ STREAM EOF) (READ STREAM EOF)))
	       ((OR (NULL NODES) (EQ FORM EOF)))
	     (COND ((MEMQ FORM NODES)
		    (SETQ NODES (DELETE FORM NODES))
		    (CURSORPOS 'A)
		    (PRINC FORM)
		    (DO ((C (TYI STREAM -1.) (TYI STREAM -1.)))
			((= C 38.))             ; "&"  = End of entry
			(COND ((= C -1.)	     ; -1   = EOF
			       (*THROW 'END-OF-FILE T))
			      ((= C 17.)	     ; "" = Quote
			       (SETQ C (TYI STREAM))
			       (TYO C))
			      ((NOT (MEMBER C '(3. 12.)))
			       (TYO C)))))
		   (T (DO ((C (TYI STREAM -1.) (TYI STREAM -1.)))
			  ((= C 38.))
			(COND ((= C -1.)
			       (*THROW 'END-OF-FILE T))
			      ((= C 17.)
			       (SETQ C (TYI STREAM)))))))))
    (CLOSE STREAM))
  (COND (NODES 
	       (MTELL "Information missing: ~%~M"
		      (CONS '(MLIST) NODES))
	       ))
       '$DONE)

(DEFUN GEN-INDEX ()
  (PROG (STREAM OSTREAM EOF)
   (SETQ STREAM (OPEN "documentation>macsyma.manual" '(IN ASCII)))
   (SETQ OSTREAM (OPEN "macsyma.index.lisp" '(OUT ASCII)))
   (SETQ EOF (GENSYM))
   (*CATCH 'END-OF-FILE
	   (DO ((POS (FILEPOS STREAM) (FILEPOS STREAM))
	        (FORM (READ STREAM EOF) (READ STREAM EOF)))
	       ((EQ FORM EOF))
	     (print `(putprop ',(prepare-a-node form) ,pos 'user-doc) ostream)
	     (DO ((C (TYI STREAM -1.) (TYI STREAM -1.)))
	         ((= C 38.))
	         (COND ((= C -1.)
		      (*THROW 'END-OF-FILE T))
		     ((= C 17.)
		      (SETQ C (TYI STREAM)))))))
    (CLOSE STREAM)
    (CLOSE OSTREAM))
    '$DONE)

(DEFMSPEC $HELP (X) X (MDESCRIBE '$HELP))

(DECLARE (SPLITFILE EXAMPL))

;In essence,  example(func):=DEMO([manual,demo,dsk,macsym],OFF,'func,OFF);

(DEFMSPEC $example (func)
 (setq func (FEXPRCHECK func))
 (NONSYMCHK func '$example)
 (let (($change_filedefaults ()))
   (batch1 `(#-Multics((MLIST) manual demo dsk macsym)
             #+Multics((mlist) ,(string-to-mstring 
				 (string-append macsyma-dir
						">demo>manual.demo")))
	     NIL ((MQUOTE) ,func) NIL) 
	   t nil nil))
 '$done)

(defmspec $apropos (form)
  (let ((apropos-search (mapcar #'(lambda (u) (format nil "~A" (fullstrip1 u))) (cdr form))))
    (cond ((null apropos-search)
	   (format standard-output
		   "~
                    ~%APROPOS takes arguments which should be a symbols or strings.~
                    ~%It searches the symbol table, returning a list of all symbols~
                    ~%which contain the arguments as substrings of their print name.~%")
	   '((MLIST)))
	  ('ELSE
	   (let ((apropos-found ())
		 (apropos-string-length (apply 'max (mapcar #'string-length apropos-search))))
	     (setq apropos-string-length (1+ apropos-string-length))
	     (mapatoms #'(lambda (symbol)
			   (LET ((STRING (GET-PNAME SYMBOL)))
			     (if (and (>= (string-length string) apropos-string-length)
				      (memq (character string) '(#/$ #/% #/&)))
				 (do ((l apropos-search (cdr l)))
				     ((null l)
				      (push symbol apropos-found))
				   (or (string-search (car l) string 1)
				       (return nil)))))))
	     `((MLIST),@(SORT APROPOS-FOUND #'STRING-LESSP)))))))
