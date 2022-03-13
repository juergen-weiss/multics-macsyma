(eval-when (compile)
    (print obarray)
    (setq obarray (get 'compiler-obarray 'array))
    (print obarray)
)

(eval-when (compile)
	   (*rset t)
             (nouuo t)
             (sstatus uuolinks)
)

(eval-when (compile)
(defun ckargs2 (name fl force)                     ;check out args prop
       (let ((n (nargs name)))
         (cond ((and (null n) (null (args name)))(putprop name (cons nil fl) 'args))
               ((null n))
                  ((= n fl))
                  (force (warn name "has been previously used with the wrong number of arguments")
                         (putprop name (cons nil fl) 'args))
                  (t (barf name "wrong number of args" data)))))
(plist 'ckargs)
)

(eval-when (compile)
    (setq obarray (get 'obarray 'array))
    (print obarray)
    (*rset nil)
    (nouuo nil)
)

