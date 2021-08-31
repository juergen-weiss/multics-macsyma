(eval-when (compile load eval)
	   (setq ibase 10.)
	   (setq base 10.)
	   (setq *nopoint t))
;;;
;;; map upper case to lower case
(eval-when (compile load eval)
	   (do ((i 65 (1+ i)))
	       ((> i 90) t)
	       (sstatus chtran (+ i 0) (+ i 32)))
)

(eval-when
 (compile load eval)
 (defun macro-dir (x) (list '* x ))
 (defun executable-dir (x) '(* * *))
 (defvar executable-dir '(* * *))
 (defvar macsyma-dir ">user_dir_dir>SysAdmin>Weiss>macsyma")

 (defun pathname-util (x) x)
 (defun probef (x) (car (allfiles x)))

 (defmacro *break (breakp mess)
	 `(apply 'break `(,,mess ,',breakp)))

 (defun sfap (x) nil)
 (defvar tyi nil)
 (defvar standard-input t)
 (defvar standard-output t)
 (defvar terminal-io t)

 (DEFUN SPLITFILE FEXPR (IGNORE) NIL)
)

(DEFMACRO INCREMENT (COUNTER &OPTIONAL INCREMENT)
  (IF INCREMENT
      `(SETF ,COUNTER (+ ,COUNTER ,INCREMENT))
      `(SETF ,COUNTER (1+ ,COUNTER))))

(DEFMACRO DECREMENT (COUNTER &OPTIONAL DECREMENT)
  (IF DECREMENT
      `(SETF ,COUNTER (- ,COUNTER ,DECREMENT))
      `(SETF ,COUNTER (1- ,COUNTER))))

;;;
;;; arrays
(defmacro ARRAY-DIMENSION-N (i arr)
  `(nth ,i (arraydims ,arr)))

(defmacro ARRAY-TYPE (arr)
  `(car  (arraydims ,arr)))

(defmacro ARRAY-/#-DIMS (arr)
  `(cdr  (arraydims ,arr)))

(DEFUN MARRAY-TYPE (X)
  (OR (CDR (ASSQ (ARRAY-TYPE X)
                 '((FLONUM . $FLOAT)
                   (FIXNUM . $FIXNUM))))
      (ARRAY-TYPE X)))

(DEFUN DIMENSION-ARRAY-OBJECT (FORM RESULT)
  (dimension-string  (maknum form) result))

;;;
;;; strings
(defun string-append (a b) (catenate a b))
(defun string (x) (catenate x))
(defun string-length (x) (stringlength x))
(defun get-pname (x) (GET_PNAME x))
(defun character (x) (getcharn x 1))
(defun string-search (str1 str2 n) (cond ((> (index str2 str1) 0) t)
                                         (t nil)))
(defun STRING-LESSP (x y) (alphalessp x y))

;;;
;;; file type
(declare  (defpl1 hcs_$status_minf "" (char (*)) (char (*)) (fixed bin (1))
                               (return fixed bin (2)) (return fixed bin (24.))
                               (return fixed bin (21.))))

(defun file-type (file)
           (let* ((fname (namestring (probef file)))
                  (minf-result (hcs_$status_minf fname "" 1)))   ; check that file is OK
                (print minf-result)
                (cond ((not (= 0 (caddr minf-result)))  ; not found?
                       nil)
                      ((= 1  (car minf-result))
                       "file")
                      ((= 2 (car minf-result))
                       (cond ((= 0 (cadr minf-result))
                              "directory")
                             (t "msf")))
                      (t "unkown"))))
