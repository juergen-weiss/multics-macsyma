(defun op-equalp (e &rest op)
  (and (not (atom e)) 
       (not (atom (car e)))
       (do ((op op (cdr op)))
	 ((null op) nil)
	(cond ((equal (caar e) (car op))
	       (return t))))))

(defmfun $taylorp (x)
  (and (not (atom x))
       (eq (caar x) 'mrat)
       (memq 'trunc (cdar x))
       t))


(defun approx-alike (f g)
  (cond ((floatp f) (and (floatp g) (= f g)))
        (($bfloatp f) (and ($bfloatp g) (equal f g)))
        (($taylorp g)
         (approx-alike 0 (sub (ratdisrep f) (ratdisrep g))))
        ((stringp f)
         (and (stringp g) (equal f g)))
        ((arrayp f)
         (and (arrayp g) (approx-alike ($listarray f) ($listarray g))))
        ((atom f)
         (and (atom g) (equal f g)))
        ((op-equalp f 'lambda)
         (and (op-equalp g 'lambda)
              (approx-alike-list (mapcar #'(lambda (s) (simplifya s nil)) (margs f))
                                 (mapcar #'(lambda (s) (simplifya s nil)) (margs g)))))
        
        (($ratp f)
         (and ($ratp g) (approx-alike (ratdisrep f) (ratdisrep g))))
        ((and (not (atom f)) (not (atom (car f))) (not (atom g)) (not (atom (car g)))
              (or (approx-alike (mop f) (mop g)) 
                  (and (atom (mop f)) (atom (mop g))
                       (approx-alike ($nounify (mop f)) ($nounify (mop g)))))
              (eq ($subvarp f) ($subvarp g))
              (approx-alike-list (margs f) (margs g))))

        (t nil)))

(defun approx-alike-list (p q)
  (cond ((null p) (null q))
        ((null q) (null p))
        (t (and (approx-alike (car p) (car q)) (approx-alike-list (cdr p) (cdr q))))))


(defun simple-equal-p (f g)
  (approx-alike (simplifya f nil) (simplifya g nil)))

(defun batch-equal-check (expected result)
  (let ((answer (*catch 'macsyma-quit (simple-equal-p expected result))))
    (if (eq answer 'maxima-error) nil answer)))


(DEFUN TEST-CONTINUE (&OPTIONAL (STANDARD-INPUT STANDARD-INPUT) BATCH-OR-DEMO-FLAG)
  (DO ((R) ($__) ($%) (NEXT)
       (EOF (LIST NIL)))
      (NIL)
      (SETQ R (MREAD STANDARD-INPUT EOF))
      (IF (EQ R EOF) (RETURN '$DONE))
      (SETQ $__ (CADDR R))
      (SETQ $% (meval* $__))
      (DISPLA $%)
      (SETQ NEXT (MREAD STANDARD-INPUT EOF))
      (IF (EQ NEXT EOF) (RETURN '$DONE))
      (SETQ NEXT (CADDR NEXT))
      (DISPLA NEXT)
      (if (batch-equal-check next $%)
          (print "equal")
	(print "not equal #######################################################")
	)
 ))

(DEFUN TEST-BATCH (FILENAME &OPTIONAL DEMO-P &AUX FILE-OBJ (accumulated-time 0.0))
  (UNWIND-PROTECT
    (TEST-CONTINUE (SETQ FILE-OBJ (OPENI FILENAME))
              (IF DEMO-P ':DEMO ':BATCH))
    (IF FILE-OBJ (CLOSE FILE-OBJ))
    (CURSORPOS 'A)
    (if $showtime
        (MTELL "Batch spent ~A seconds in evaluation ~%"
               (FORMAT NIL "~2F" accumulated-time)))))


(DEFUN $TESTBATCH (&REST ARG-LIST)
  (TEST-BATCH (FILENAME-FROM-ARG-LIST ARG-LIST) NIL))
