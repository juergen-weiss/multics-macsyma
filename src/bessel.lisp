;;;; -*- Mode:Lisp; Package:Macsyma; -*-

;;; Bessel, Gamma, Zeta, and Expint functions
;;; Copied from a similar file by CFFK.

(DEFUN J[0]-BESSEL (X) 
   (LET ((XA (ABS X)) (P 0.0) (Q 0.0) (SI 0.0) (CO 0.0)
	 (SQ 0.0) (SX0 0.0) (Y 0.0) (Z 0.0))
	(COND ((> XA 8.0)
	       (SETQ Y (+ -0.7853982 XA) SI (SIN Y) CO (COS Y))
	       (SETQ Y (// 8.0 XA) Z (* Y Y) SQ (SQRT Y))
	       (SETQ P (+ 0.2820948 (* Z (+ -3.096437E-4 (* 6.943574E-6 Z)))))
	       (SETQ Q (// (* Y (+ 7.030992 (* 0.7550996 Z)))
			   (+ 1595.15 (* Z (+ 185.9156 Z)))))
	       (* (+ (* CO P) (* Q SI)) SQ))
	      ((> XA 4.0)
	       (SETQ Y (* 0.015625 (* XA XA)) Z (- (1- Y)))
	       (SETQ SX0 5.5200781 P (- XA SX0))
	       (// (* P
		      (+ SX0 XA)
		      (+ 0.1920038 (* Z (+ 0.2025329 Y (* Z (+ 0.2290394 Y
							       (* Z (+ -0.3228404
								       (* -0.70066 Z)))))))))
		   (+ 12.18896 (* Y (+ 13.64497 (* Y (+ 7.894887 (* Y (+ 2.775489 Y)))))))))
	      (T (SETQ Y (* 0.0625 (* XA XA)) SX0 2.40482554)
		 (SETQ P (- XA SX0))
		 (// (* P (+ SX0 XA) (+ -6.171667 (* Y (+ 5.953519
							  (* Y (+ -1.754611
								  (* 0.173663 Y)))))))
		     (+ 35.6919 (* Y (+ 9.590446 Y)))))))) 

(DEFUN $J0 ($X) (IF (NUMBERP $X) (J[0]-BESSEL $X) `(($J0 SIMP) ,(SIMPLIFY $X))))


(DEFUN J[1]-BESSEL (X) 
       (LET ((XA (ABS X)) (P 0.0) (Q 0.0) (SI 0.0) (CO 0.0)
	     (SQ 0.0) (SX0 0.0) (RJ1 0.0) (Y 0.0) (Z 0.0)) 
	    (SETQ XA (ABS X))
	    (COND ((> XA 8.0)
		   (SETQ Y (+ -2.356194 XA) SI (SIN Y) CO (COS Y))
		   (SETQ Y (// 8.0 XA) Z (* Y Y) SQ (SQRT Y))
		   (SETQ P (+ 0.2820948 (* Z (+ 5.162034E-4 (* -9.002696E-6 Z)))))
		   (SETQ Q (// (* Y (+ 50.53199 (* 4.999898 Z)))
			       (+ 3821.467 (* Z (+ 394.4419 Z)))))
		   (SETQ RJ1 (* (- (* CO P) (* Q SI)) SQ)))
		  ((> XA 4.0)
		   (SETQ Y (* 0.015625 (* XA XA)) Z (- (1- Y)))
		   (SETQ SX0 7.0155867 P (- XA SX0))
		   (SETQ RJ1
			 (// (* P XA (+ SX0 XA)
				(+ 0.04297259 (* Z (+ 0.06689943
						      (* Z (+ -0.05380065
							      (* Z (+ -0.1045012
								      (* -0.04185412 Z)))))))))
			     (+ 8.886393 (* Y (+ 8.204713 (* Y (+ 3.566279 Y))))))))
		  (T (SETQ Y (* 0.0625 (* XA XA)) SX0 3.83170596 P (- XA SX0))
		     (SETQ RJ1
			   (// (* P XA (+ SX0 XA) (+ -4.665107
						     (* Y (+ 2.497075 (* -0.3222962 Y)))))
			       (+ 136.9859 (* Y (+ 51.3648 (* Y (+ 9.447542 Y)))))))))
	    (AND (< X 0.0) (SETQ RJ1 (- RJ1)))
	    RJ1)) 

(DEFUN $J1 ($X) (IF (NUMBERP $X) (J[1]-BESSEL $X) `(($J1 SIMP) ,(SIMPLIFY $X))))

(DEFVAR J-BESSEL-ARRAY (MAKE-ARRAY NIL 'ART-FLOAT 100.))

(DEFUN J[N]-BESSEL (X N) 
   (PROG (A0 A1 AK B0 B1 BK CK DEN FM FI GN NS QK RJ0 RJN M) 
	 (SETQ NS (COND ((< N 0.) -1.) (T 1.)) N (ABS N))
	 (IF (> N 100.) (SETQ J-BESSEL-ARRAY (MAKE-ARRAY NIL 'ART-FLOAT (1+ N))))
	 (SETQ FM (FLOAT (1+ N)) FI (* 1.25 (ABS X)))
	 (SETQ M (FIX (COND ((> FM FI) FM) (T FI))))
	 (SETQ FI (FLOAT (+ M M)) FM FI QK (// X FM))
	 (COND ((> (ABS X) 2.0E-4)
		(SETQ A0 -1.0 A1 0.0 B0 0.0 B1 1.0)
		(DO NIL
		    (NIL)
		    (SETQ FI (+ 2.0 FI) 
			  CK (// FI (ABS X)) 
			  AK (- (* A1 CK) A0))
		    (SETQ BK (- (* B1 CK) B0) GN QK A0 A1 A1 AK B0 B1 B1 BK)
		    (SETQ QK (// AK BK))
		    (OR (> (ABS (// (- QK GN) QK)) 1.0E-6) (RETURN NIL)))
		(AND (< X 0.0) (SETQ QK (- QK)))))
	 (DO ((I M (1- I)))
	     ((> 1. I))
	     (SETQ DEN (- FM (* QK X)))
	     (AND (= DEN 0.0) (SETQ DEN (* 1.0E-7 FM)))
	     (SETQ QK (// X DEN))
	     (OR (< N I) (ASET QK J-BESSEL-ARRAY I))
	     (SETQ FM (+ -2.0 FM)))
	 (COND ((> 1.0 (ABS QK)) (SETQ RJ0 (J[0]-BESSEL X) RJN (* QK RJ0)))
	       (T (SETQ RJN (J[1]-BESSEL X) RJ0 (// RJN QK))))
	 (ASET RJ0 J-BESSEL-ARRAY 0)
	 (OR (> N 0.) (RETURN (AREF J-BESSEL-ARRAY 0)))
	 (ASET RJN J-BESSEL-ARRAY 1)
	 (OR (> N 1.)
	     (RETURN (ASET (* (FLOAT NS) (AREF J-BESSEL-ARRAY 1.)) J-BESSEL-ARRAY 1)))
	 (AND (= X 0.0) (RETURN 0.0))
	 (DO ((I 2. (1+ I)))
	     ((> I N))
	     (COND ((OR (> (ABS (AREF J-BESSEL-ARRAY I)) 1.0)
			(> (ABS RJN) (// 1.0E-38 (ABS (AREF J-BESSEL-ARRAY I)))))
		    (SETQ RJN (* (AREF J-BESSEL-ARRAY I) RJN)))
		   (T (SETQ RJN 0.0)))
	     (ASET RJN J-BESSEL-ARRAY I))
	 (AND (< NS 0.)
	      (DO ((I 1. (+ I 2.)))
		  ((> I N))
		  (ASET (- (AREF J-BESSEL-ARRAY I)) J-BESSEL-ARRAY I)))
	 (RETURN (AREF J-BESSEL-ARRAY N)))) 

(DEFVAR $JARRAY)

(DEFUN $JN ($X $N)
  (COND ((AND (NUMBERP $X) (FIXP $N))
	 (J[N]-BESSEL (FLOAT $X) $N)
	 (APPLY '$ARRAY `($JARRAY $FLOAT ,(ABS $N)))
	 (SETQ $JARRAY (MGET '$JARRAY 'ARRAY))
	 (FILLARRAY $JARRAY J-BESSEL-ARRAY)
	 (AREF $JARRAY (ABS $N)))
	(T `(($JN SIMP) ,(SIMPLIFY $X),(SIMPLIFY $N)))))


(DEFUN I[0]-BESSEL (X) 
       (LET ((XA (ABS X)) (Y 0.0) (Z 0.0)) 
	    (SETQ XA (ABS X))
	    (COND ((> 4.0 XA)
		   (SETQ Z (* 0.0625 (* XA XA)))
		   (// (+ -162.6391 (* Z (+ -585.5938 (* Z (+ -402.5407 (* -75.72017 Z))))))
		       (+ -162.6391 (* Z (+ 64.96299 (* Z (+ -11.84469 Z)))))))
		  (T (SETQ Y (// 4.0 XA) Z (- (1- Y)))
		     (* (EXP XA)
			(SQRT Y)
			(// (+ 2.67093 (* Z (+ 2.470948 (* Z (+ 6.271432 Z))))))
			(+ 0.5528884 (* Z (+ 0.4861227 (* Z (+ 1.281496
							       (* 0.1555914 Z))))))))))) 

(DEFUN $I0 ($X) (IF (NUMBERP $X) (I[0]-BESSEL $X) `(($I0 SIMP) ,(SIMPLIFY $X))))


(DEFUN I[1]-BESSEL (X) 
   (LET ((XA (ABS X)) (Y 0.0) (Z 0.0) (RI1 0.0)) 
	(COND ((> 4.0 XA)
	       (SETQ Y (* 0.25 XA) Z (* Y Y))
	       (SETQ RI1
		     (// (* Y
			    (+ -569.784
			       (* Z
				  (+ -947.9975
				     (* Z
					(+ -405.4861
					   (* -53.66977 Z)))))))
			 (+ -284.892
			    (* Z
			       (+ 95.78535 (* Z (+ -14.45951 Z))))))))
	      (T (SETQ Z (1+ (// -4.0 XA)))
		 (SETQ RI1
		       (* (EXP XA)
			  (// (SQRT XA))
			  (// (+ 0.9980789
				 (* Z
				    (+ -0.3663376
				       (* Z (+ 2.818702 Z))))))
			  (+ 0.3568149
			     (* Z
				(+ -0.08379694
				   (* Z
				      (+ 0.9826178
					 (* Z
					    (+ 0.4946486
					       (* 0.0251859
						  Z))))))))))))
	(AND (< X 0.0) (SETQ RI1 (- RI1)))
	RI1)) 

(DEFUN $I1 ($X) (IF (NUMBERP $X) (I[1]-BESSEL $X) `(($I1 SIMP) ,(SIMPLIFY $X))))


(DEFVAR I-BESSEL-ARRAY (MAKE-ARRAY NIL 'ART-FLOAT 100.))

(DEFUN I[N]-BESSEL (X N) 
   (PROG (A A0 A1 AN B B0 B1 FI FN Q0 Q1) 
	 (SETQ N (ABS N))
	 (IF (> N 100.) (SETQ I-BESSEL-ARRAY (MAKE-ARRAY NIL 'ART-FLOAT (1+ N))))
	 (AND (= N 0.) (GO $L9))
	 (SETQ FN (FLOAT (+ N N)))
	 (SETQ Q1 (// X FN))
	 (COND ((> (ABS X) 3.0E-4)
		(SETQ A0 1.0 A1 0.0 B0 0.0 B1 1.0 FI FN)
		(DO NIL
		    (NIL)
		    (SETQ FI (+ 2.0 FI) 
			  AN (// FI (ABS X)) 
			  A (+ A0 (* A1 AN)))
		    (SETQ B (+ B0 (* AN B1)))
		    (SETQ A0 A1 B0 B1 A1 A B1 B Q0 Q1 Q1 (// A B))
		    (OR (> (ABS (// (- Q1 Q0) Q1)) 1.0E-6) (RETURN NIL)))
		(AND (< X 0.0) (SETQ Q1 (- Q1)))))
	 (DO ((I N (1- I)))
	     ((> 0. I))
	     (SETQ Q1 (// X (+ FN (* Q1 X))))
	     (ASET Q1 I-BESSEL-ARRAY I)
	     (SETQ FN (+ -2.0 FN)))
	 $L9  (SETQ FI (I[0]-BESSEL X))
	 (ASET FI I-BESSEL-ARRAY 0)
	 (AND (OR (= X 0.0) (= N 0.)) (RETURN (AREF I-BESSEL-ARRAY N)))
	 (DO ((I 1. (1+ I)))
	     ((> I N))
	     (COND ((OR (> (ABS (AREF I-BESSEL-ARRAY I)) 1.0)
			(> (ABS FI) (// 1.0E-38 (ABS (AREF I-BESSEL-ARRAY I)))))
		    (SETQ FI (* FI (AREF I-BESSEL-ARRAY I))))
		   (T (SETQ FI 0.0)))
	     (ASET FI I-BESSEL-ARRAY I))
	 (RETURN (AREF I-BESSEL-ARRAY N))))

(DEFVAR $IARRAY)

(DEFUN $IN ($X $N)
  (COND ((AND (NUMBERP $X) (FIXP $N))
	 (I[N]-BESSEL (FLOAT $X) $N)
	 (APPLY '$ARRAY `($IARRAY $FLOAT ,(ABS $N)))
	 (SETQ $IARRAY (MGET '$IARRAY 'ARRAY))
	 (FILLARRAY $IARRAY I-BESSEL-ARRAY)
	 (AREF $IARRAY (ABS $N)))
	(T `(($IN SIMP) ,(SIMPLIFY $X) ,(SIMPLIFY $N)))))


(DEFUN G[0]-BESSEL (X) 
       (LET ((XA (ABS X)) (Y 0.0) (Z 0.0)) 
	    (COND ((> 4.0 XA)
		   (SETQ Z (* 0.0625 (* XA XA)))
		   (// (+ -162.6391 (* Z (+ -585.5938 (* Z (+ -402.5407 (* -75.72017 Z))))))
		       (+ -162.6391 (* Z (+ 64.96299 (* Z (+ -11.84469 Z)))))
		       (EXP XA)))
		  (T (SETQ Y (// 4.0 XA))
		     (SETQ Z (- (1- Y)))
		     (* (SQRT Y)
			(// (+ 2.67093 (* Z (+ 2.470948 (* Z (+ 6.271432 Z))))))
			(+ 0.5528884 (* Z (+ 0.4861227 (* Z (+ 1.281496
							       (* 0.1555914 Z))))))))))) 

(DEFUN $G0 ($X) (IF (NUMBERP $X) (G[0]-BESSEL $X) `(($G0 SIMP) ,(SIMPLIFY $X))))

(DEFUN G[1]-BESSEL (X) 
       (LET ((XA (ABS X)) (Y 0.0) (Z 0.0) (RI1 0.0)) 
	    (COND ((> 4.0 XA)
		   (SETQ Y (* 0.25 XA) Z (* Y Y))
		   (SETQ RI1
			 (// (* Y (+ -569.784 (* Z (+ -947.9975 (* Z (+ -405.4861
									(* -53.66977 Z)))))))
			     (+ -284.892 (* Z (+ 95.78535 (* Z (+ -14.45951 Z)))))
			     (EXP XA))))
		  (T (SETQ Z (1+ (// -4.0 XA)))
		     (SETQ RI1
			   (// (+ 0.3568149
				  (* Z (+ -0.08379694 (* Z (+ 0.9826178
							      (* Z (+ 0.4946486
								      (* 0.0251859 Z))))))))
			       (+ 0.9980789 (* Z (+ -0.3663376 (* Z (+ 2.818702 Z)))))
			       (SQRT XA)))))
	    (AND (< X 0.0) (SETQ RI1 (- RI1)))
	    RI1)) 

(DEFUN $G1 ($X) (IF (NUMBERP $X) (G[1]-BESSEL $X) `(($G1 SIMP) ,(SIMPLIFY $X))))

(DEFVAR G-BESSEL-ARRAY (MAKE-ARRAY NIL 'ART-FLOAT 100.))

(DEFUN G[N]-BESSEL (X N) 
   (PROG (A A0 A1 AN B B0 B1 FI FN Q0 Q1) 
	 (SETQ N (ABS N))
	 (IF (> N 100.) (SETQ G-BESSEL-ARRAY (MAKE-ARRAY NIL 'ART-FLOAT (1+ N))))
	 (AND (= N 0.) (GO $L9))
	 (SETQ FN (FLOAT (+ N N)) Q1 (// X FN))
	 (COND ((> (ABS X) 3.0E-4)
		(SETQ A0 1.0 A1 0.0 B0 0.0 B1 1.0 FI FN)
		(DO NIL
		    (NIL)
		    (SETQ FI (+ 2.0 FI) 
			  AN (// FI (ABS X)) 
			  A (+ A0 (* A1 AN)))
		    (SETQ B (+ B0 (* AN B1)))
		    (SETQ A0 A1 B0 B1 A1 A B1 B Q0 Q1 Q1 (// A B))
		    (OR (> (ABS (// (- Q1 Q0) Q1)) 1.0E-6) (RETURN NIL)))
		(AND (< X 0.0) (SETQ Q1 (- Q1)))))
	 (DO ((I N (1- I)))
	     ((> 0. I))
	     (SETQ Q1 (// X (+ FN (* Q1 X))))
	     (ASET Q1 G-BESSEL-ARRAY I)
	     (SETQ FN (+ -2.0 FN)))
	 $L9  (SETQ FI (G[0]-BESSEL X))
	 (ASET FI G-BESSEL-ARRAY 0)
	 (AND (OR (= X 0.0) (= N 0.)) (RETURN (AREF G-BESSEL-ARRAY N)))
	 (DO ((I 1. (1+ I)))
	     ((> I N))
	     (COND ((OR (> (ABS (AREF G-BESSEL-ARRAY I)) 1.0)
			(> (ABS FI) (// 1.0E-38 (ABS (AREF G-BESSEL-ARRAY I)))))
		    (SETQ FI (* FI (AREF G-BESSEL-ARRAY I))))
		   (T (SETQ FI 0.0)))
	     (ASET FI G-BESSEL-ARRAY I))
	 (RETURN (AREF G-BESSEL-ARRAY N)))) 

(DEFVAR $GARRAY)

(DEFUN $GN ($X $N)
  (COND ((AND (NUMBERP $X) (FIXP $N))
	 (G[N]-BESSEL (FLOAT $X) $N)
	 (APPLY '$ARRAY `($GARRAY $FLOAT ,(ABS $N)))
	 (SETQ $GARRAY (MGET '$GARRAY 'ARRAY))
	 (FILLARRAY $GARRAY G-BESSEL-ARRAY)
	 (AREF $GARRAY (ABS $N)))
	(T `(($GN SIMP) ,(SIMPLIFY $X) ,(SIMPLIFY $N)))))

(DEFVAR RJ-BESSEL-ARRAY)
(DEFVAR CJ-BESSEL-ARRAY)

(DEFUN BESSEL (RZ CZ A) 
  (PROG (N Y $T T0 T1 K1 D R1 RP SQRP RNPA R2 M TA RN RL MPO LN RNP RR CR RS CS
	 RLAM CLAM QLAM IND S PHI RSUM CSUM L)
	(SETQ N (FIX A) A (- A (FLOAT N)) LN (1+ N) Y (ABS CZ))
	(SETQ RJ-BESSEL-ARRAY (MAKE-ARRAY NIL 'ART-FLOAT LN)
	      CJ-BESSEL-ARRAY (MAKE-ARRAY NIL 'ART-FLOAT LN))
	(GO L13)
L9	(COND
	 ((NOT (< 10.0 $T))
	  (SETQ $T (+ (* (+ (* (+ (* (+ (* (+ (* $T 5.794E-5) -1.76148E-3) $T) 0.0208641)
				     $T)
				  -0.129013)
			       $T)
			    0.85777)
			 $T)
		      1.0125)))
	 (T (SETQ T0  (+ -0.5 (LOG $T))
		  T1 (//  (- 0.5 (LOG T0)) (1+ T0))
		  $T (// $T (+ (* T1 T0) T0)))))
	(COND ((> K1 0.) (SETQ R2 (* $T R2)) (GO L25))
	      (T (SETQ R1 (* $T (FLOAT N))) (GO L22)))
   L13  (COND ((AND (= RZ 0.0) (= Y 0.0))
	       (OR (= A 0.0) (ERROR "Bessel Function Evaluated At Branch Point"))
	       (ASET 1.0 RJ-BESSEL-ARRAY 0)
	       (RETURN (COND ((= N 0.) 1.0) (T 0.0)))))
	(SETQ D 17.5045 R1 0.0)
	(COND ((> N 0.) (SETQ $T (// D (FLOAT (* 2 N))) K1 0.) (GO L9)))
   L22	(SETQ RP (+ (* Y Y) (* RZ RZ)) SQRP (SQRT RP) R2 (* SQRP 1.3591))
	(COND ((> D Y)
	       (SETQ $T (// (* (- D Y) 0.356) SQRP) K1 1.)
	       (GO L9)))
L25	(COND ((> R2 R1) (SETQ R1 R2)))
	(SETQ M (1+ (FIX R1)) TA (* A 2.0) RN 1.0 RL 1.0)
	(ASET 1.0 RJ-BESSEL-ARRAY 0)
	(SETQ MPO (1+ M))
	(DO ((K 2. (+ K 1.)))
	    ((> K MPO))
	    (SETQ RNP (1+ RN) RL (// (* (+ TA RN) RL) RNP))
	    (COND ((NOT (< LN K)) (ASET RL RJ-BESSEL-ARRAY (1- K))))
	    (SETQ RN RNP))
	(SETQ RR 0.0 CR 0.0 RS 0.0 CS 0.0)
	(DO
	 ((K 1. (1+ K)))
	 ((> K M))
	 (SETQ L (- MPO K) RN (1- RNP) RNPA (+ TA (* RN 2.0)))
	 (SETQ RLAM (- (+ (* Y CR) RNPA) (* RZ RR)) CLAM (+ (* Y RR) (* RZ CR)))
	 (SETQ QLAM (+ (* RLAM RLAM) (* CLAM CLAM)))
	 (COND ((= QLAM 0.0) (SETQ QLAM (* RNPA 1.0E-17))))
	 (SETQ RR (// (- (* RZ RNPA) (* RR RP)) QLAM)
	       CR (// (+ (* Y RNPA) (* RP CR)) QLAM))
	 (COND ((> L LN) (SETQ RL (* RNP RL (// (+ TA RN)))))
	       (T (COND ((> LN L)
			 (ASET RR RJ-BESSEL-ARRAY L)
			 (ASET CR CJ-BESSEL-ARRAY L)))
		  (SETQ RL (AREF RJ-BESSEL-ARRAY (1- L)))))
	 (SETQ QLAM (* RNPA RL) RLAM 0.0 CLAM 0.0 IND (\ L 4.))
	 (COND ((= IND 0.) (SETQ RLAM QLAM))
	       ((= IND 1.) (SETQ CLAM (SETQ QLAM (- QLAM))))
	       ((= IND 2.) (SETQ RLAM (SETQ QLAM (- QLAM))))
	       (T (SETQ CLAM QLAM)))
	 (SETQ S (- (* (+ RS RLAM) RR) (* (+ CS CLAM) CR)))
	 (SETQ CS (+ (* (+ RS RLAM) CR) (* RR (+ CS CLAM))))
	 (SETQ RS S RNP RN))
	(SETQ 
	 QLAM
	 (* (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* (+ A -13.812104) A) 50.569126) A) 122.48542)
				    A)
				 -968.33451)
			      A)
			   -203.72512)
			A)
		     5452.1006)
		  A)
	       4630.389)
	     (// (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* A 0.12949573) 1.61000405) A)
						  12.473999)
					       A)
					    78.03884)
					 A)
				      339.71559)
				   A)
				1228.9483)
			     A)
			  2779.373)
		       A)
		    4630.3895))
	     (EXP Y)
	     (EXPT (* 0.5 SQRP) A)))
	(COND ((> 1.0E-36 (ABS RZ)) (SETQ PHI (* A 1.57079632)))
	      (T (SETQ PHI (ATAN Y RZ) PHI (- (* PHI A) (* RZ)))))
	(SETQ RSUM (* (COS PHI) QLAM) CSUM (* (SIN PHI) QLAM))
	(SETQ RS (1+ RS) S (+ (* RS RS) (* CS CS)))
	(SETQ RR (// (+ (* RSUM RS) (* CSUM CS)) S))
	(SETQ CR (// (- (* RS CSUM) (* RSUM CS)) S))
	(ASET RR RJ-BESSEL-ARRAY 0)
	(ASET CR CJ-BESSEL-ARRAY 0)
	(COND ((> N 0.)
	       (DO ((K 1. (+ K 1.)))
		   ((> K N))
		   (SETQ RS (AREF RJ-BESSEL-ARRAY K) CS (AREF CJ-BESSEL-ARRAY K))
		   (SETQ S (- (* RS RR) (* CS CR)))
		   (SETQ CR (+ (* RS CR) (* RR CS)))
		   (SETQ RR S)
		   (ASET RR RJ-BESSEL-ARRAY K)
		   (ASET CR CJ-BESSEL-ARRAY K))))
	(COND ((> 0.0 CZ) 
	       (DO ((K 0. (1+ K)))
		   ((> K N))
		   (ASET (- (AREF CJ-BESSEL-ARRAY K)) CJ-BESSEL-ARRAY K))))
	(RETURN `((MLIST SIMP) ,(AREF RJ-BESSEL-ARRAY N) ,(AREF CJ-BESSEL-ARRAY N)))))

(DEFVAR $BESSELARRAY)

(DEFUN $BESSEL ($ARG $ORDER)
  (LET ((A 0.0) (RP ($REALPART $ARG)) (IP ($IMAGPART $ARG)))
	   (COND ((NOT (AND (NUMBERP $ORDER)
			    (NOT (< (SETQ A (FLOAT $ORDER)) 0.0))
			    (NUMBERP RP)
			    (NUMBERP IP)))
		  `(($BESSEL SIMP) ,(SIMPLIFY $ARG) ,(SIMPLIFY $ORDER)))
		 (T (BESSEL RP IP A)
		    (APPLY '$ARRAY `($BESSELARRAY $COMPLETE ,(FIX A)))
		    (SETQ $BESSELARRAY (MGET '$BESSELARRAY 'ARRAY))
		    (DO ((K 0. (1+ K)) (N (FIX A)))
			((> K N) (AREF $BESSELARRAY N))
			(ASET (SIMPLIFY
			       `((MPLUS)
				 ,(SIMPLIFY `((MTIMES) $%I ,(AREF CJ-BESSEL-ARRAY K)))
				 ,(AREF RJ-BESSEL-ARRAY K)))
			       $BESSELARRAY K))))))
;HERE IS AI'
;AIRY1(Z):=IF Z = 0. THEN -1/(GAMMA(1/3.)*3.^(1/3.))
;ELSE BLOCK([ZZ],Z:-Z,ZZ:2./3.*Z^(3./2.),BESSEL(ZZ,4./3.),
;J:REALPART(2/(3.*ZZ)*BESSELARRAY[0]-BESSELARRAY[1]),
;-1/3.*Z*(J-REALPART(BESSEL(ZZ,2./3.))));

(DEFUN AIRY (RZ)
   (LET ((Y 0.0) (RS 0.0) (CS 0.0) (THIRD (// 3.0)) (SIN60 (SQRT 0.75)))
	(SETQ Y (SQRT (ABS RZ)) RZ (* 2.0 THIRD Y RZ))
	(COND ((= RZ 0.0) 0.35502805) ;;;AVOIDS BRANCH POINT PROBS
	      ((> RZ 3.3333333)
	       (DO ((FI 1.0 (1+ FI))
		    (TERM 1.0)
		    (SUM 1.0))
		   ((> FI 7.0)
		    (SETQ SUM (- SUM (* 0.5 TERM)))
		    (// (* (EXP (- RZ)) SUM)
			(* 2.0 (SQRT (* Y (ATAN 0 -1))))))
		   (SETQ TERM (// (* TERM -0.5 (- FI 0.83333333) (- FI 0.166666666))
				  (* FI RZ))
			 SUM (+ SUM TERM))))
	      ((< RZ -7.5)
	       (SETQ RZ (- RZ))
	       (DO ((FI 1.0 (1+ FI))
		    (TERM 1.0)
		    (COSSUM 0.0)
		    (SINSUM 1.0)
		    (SIGN -1.0)
		    (EVEN NIL (NOT EVEN)))
		   ((> FI 6.0)
		    (SETQ RZ (+ RZ (ATAN 1. 1.)))
		    (// (+ (* (SIN RZ) SINSUM)
			   (* (COS RZ) COSSUM))
			(SQRT (* Y (ATAN 0 -1.)))))
		   (SETQ TERM (// (* TERM 0.5 (- FI 0.83333333) (- FI 0.166666666))
				  (* FI RZ)))
		   (COND (EVEN (SETQ SINSUM (+ SINSUM (* SIGN TERM))
				     SIGN (- SIGN)))
			 (T (SETQ COSSUM (+ COSSUM (* SIGN TERM)))))))
	      ((< RZ 0.0)
	       (SETQ RZ (- RZ))
	       (BESSEL RZ 0.0 (* 5.0 THIRD))
	       (SETQ RS (- (// (* 4.0 (AREF RJ-BESSEL-ARRAY 0.))
			       (* 3.0 RZ)) (AREF RJ-BESSEL-ARRAY 1.)))
	       (BESSEL RZ 0.0 THIRD)
	       (* Y THIRD (+ RS (AREF RJ-BESSEL-ARRAY 0.))))
	      (T (BESSEL 0.0 RZ (* 5.0 THIRD))
		 (SETQ RS (- (// (* 4.0 (AREF CJ-BESSEL-ARRAY 0.))
				 (* 3.0 RZ))
			     (AREF RJ-BESSEL-ARRAY 1.))
		       CS (- (// (* -4.0 (AREF RJ-BESSEL-ARRAY 0.))
				 (* 3.0 RZ))
			     (AREF CJ-BESSEL-ARRAY 1.))
		       RS (- (* SIN60 RS) (* 0.5 CS)))
		 (BESSEL 0.0 RZ THIRD)
		 (SETQ CS (+ (* SIN60 (AREF RJ-BESSEL-ARRAY 0.))
			     (* 0.5 (AREF CJ-BESSEL-ARRAY 0.))))
		 (* Y THIRD (- RS CS))))))

(DEFUN $AIRY ($ARG) (IF (NUMBERP $ARG) (AIRY $ARG) `(($AIRY SIMP) ,(SIMPLIFY $ARG))))

(DEFUN Z-FUNCTION (X Y) 
   (LET ((XS (COND ((> 0.0 X) -1.0) (T 1.0)))
	 (YS (COND ((> 0.0 X) -1.0) (T 1.0)))
	 (CAPN 0.) (NU 0.) (NP1 0.) (H 0.0) (H2 0.0) (LAMB 0.0)
	 (R1 0.0) (R2 0.0) (S 0.0) (S1 0.0) (S2 0.0) (T1 0.0) (T2 0.0)
	 (C 0.0) BOOL (RE 0.0) (IM 0.0)) 
	(SETQ XS (COND ((> 0.0 X) -1.0) (T 1.0)))
	(SETQ YS (COND ((> 0.0 Y) -1.0) (T 1.0)))
	(SETQ X (ABS X) Y (ABS Y))
	(COND ((AND (> 4.29 Y) (> 5.33 X))
	       (SETQ S (* (1+ (* -0.23310023 Y))
			  (SQRT (1+ (* -0.035198873 X X)))))
	       (SETQ H (* 1.6 S) H2 (* 2.0 H) CAPN (+ 6. (FIX (* 23.0 S))))
	       (SETQ NU (+ 9. (FIX (* 21.0 S)))))
	      (T (SETQ H 0.0) (SETQ CAPN 0.) (SETQ NU 8.)))
	(AND (> H 0.0) (SETQ LAMB (^$ H2 CAPN)))
	(SETQ BOOL (OR (= H 0.0) (= LAMB 0.0)))
	(DO ((N NU (1- N)))
	    ((> 0. N))
	    (SETQ NP1 (1+ N))
	    (SETQ T1 (+ H (* (FLOAT NP1) R1) Y))
	    (SETQ T2 (- X (* (FLOAT NP1) R2)))
	    (SETQ C (// 0.5 (+ (* T1 T1) (* T2 T2))))
	    (SETQ R1 (* C T1) R2 (* C T2))
	    (COND ((AND (> H 0.0) (NOT (< CAPN N)))
		   (SETQ T1 (+ S1 LAMB) S1 (- (* R1 T1) (* R2 S2)))
		   (SETQ S2 (+ (* R1 S2) (* R2 T1)) LAMB (// LAMB H2)))))
	(SETQ IM (COND ((= Y 0.0) (* 1.77245384 (EXP (- (* X X)))))
		       (T (* 2.0 (COND (BOOL R1) (T S1))))))
	(SETQ RE (* -2.0 (COND (BOOL R2) (T S2))))
	(COND ((> YS 0.0) (SETQ RE (* RE XS)))
	      (T (SETQ R1 (* 3.5449077 (EXP (- (* Y Y) (* X X)))))
		 (SETQ R2 (* 2.0 X Y))
		 (SETQ RE (* (- RE (* R1 (SIN R2))) XS))
		 (SETQ IM (- (* R1 (COS R2)) IM))))
	`((MLIST SIMP) ,RE ,IM))) 

(DEFUN $NZETA ($Z) 
  (LET (($X ($REALPART $Z)) ($Y ($IMAGPART $Z)))
     (IF (AND (NUMBERP $X) (NUMBERP $Y))
	 (LET (($W (Z-FUNCTION $X $Y)))
	       (SIMPLIFY `((MPLUS) ,(SECOND $W) ,(SIMPLIFY `((MTIMES) $%I ,(THIRD $W))))))
	 `(($NZETA SIMP) ,(SIMPLIFY $Z)))))

(DEFUN $NZETAR ($Z)
  (LET (($X ($REALPART $Z)) ($Y ($IMAGPART $Z)))
     (IF (AND (NUMBERP $X) (NUMBERP $Y)) (SECOND (Z-FUNCTION $X $Y))
	 `(($NZETAR SIMP) ,(SIMPLIFY $Z)))))

(DEFUN $NZETAI ($Z)
  (LET (($X ($REALPART $Z)) ($Y ($IMAGPART $Z)))
     (IF (AND (NUMBERP $X) (NUMBERP $Y)) (THIRD (Z-FUNCTION $X $Y))
	 `(($NZETAI SIMP) ,(SIMPLIFY $Z)))))

(DEFUN GAUSS ()
  (DO ((I 0. (1+ I))
       (TE 0.0 (+ TE (* (RANDOM) 1.45519152E-11))))
      ((= I 12.) TE)))

(DEFUN $GAUSS ($MEAN $SD)
  (IF (AND (NUMBERP $MEAN) (NUMBERP $SD)) (+ $MEAN (* $SD (GAUSS)))
	`(($GAUSS SIMP) ,(SIMPLIFY $MEAN) ,(SIMPLIFY $SD))))

(DEFUN EXPINT (X) 
   (COND ((< X 1.0)
	  (- (* (+ (* (+ (* (+ (* (+ (* 1.07857E-3 X)
				     -9.76004E-3)
				  X)
			       0.05519968)
			    X)
			 -0.24991055)
		      X)
		   0.99999193)
		X) 0.57721565 (LOG X)))
	 (T (LET ((W 0.0) (Y 0.0))
		 (SETQ Y (+ (* (+ (* (+ (* (+ X 8.57332873) X) 18.059017) X) 8.63476085)
			       X) 0.26777373)
		       W (+ (* (+ (* (+ (* (+ X 9.5733224) X) 25.6329562) X) 21.099653)
			       X) 3.95849696))
		 (* (// (EXP (- X)) X) (// Y W))))))

(DEFUN $EXPINT ($X) (IF (NUMBERP $X) (EXPINT $X) `(($EXPINT SIMP) ,(SIMPLIFY $X))))