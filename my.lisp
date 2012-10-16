

(defun cl-trimn (str init)
  (if (string-equal str "")
      nil
      (if (or (char-equal (aref str 0) #\space)
	      (char-equal (aref str 0) #\tab))
	  (cl-trimn (subseq str 1) init)
	  (if (or (char-equal (aref str (- (length str) 1)) #\space)
		  (char-equal (aref str (- (length str) 1)) #\tab))
	      (cl-trimn (reverse str) 1)
	      (if (= init 1)
		  (reverse str)
		  str)))))
(defun cl-trim (str)
  (if (stringp str)
      (progn (princ (type-of str)) (cl-trimn str 0))
      (format t "~a not a string. ~a ~%"
	      str (type-of str))))

