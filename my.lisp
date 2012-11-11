

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







(defun is-char-space (x)
  (if (or (char-equal x #\tab)
	  (char-equal x #\ ))
      x
      nil))
(defun words (sent)
  "take a long string, out is a list,
every element of this list is a word"
  (labels ((words-by (sent beg end len)
	     (if (= end len)
		 (if (= beg end)
		     nil
		     (cons (subseq sent beg end) nil))
		 (if (and (is-char-space (aref sent end))
			  (/= beg end))
		     (cons (subseq sent beg end)
			   (words-by sent (1+ end) (1+ end) len))
		     (if (and (is-char-space (aref sent beg))
			      (is-char-space (aref sent end)))
			 (words-by sent (1+ end) (1+ end) len)
			 (words-by sent beg (1+ end) len))))))
    (words-by sent 0 0 (length sent))))


	       
(defun lines (sent)
  "take a long string, out is a list,
every element of this list a line"
  (labels ((lines-by (sent beg end len)
	     (if (= end len)
		 (cons (subseq sent beg) nil)
		 (if (char-equal (aref sent end)
				 #\newline)
		     (cons (subseq sent beg end)
			   (lines-by sent (1+ end) (1+ end) len))
		     (lines-by sent beg (1+ end) len)))))
    (lines-by sent 0 0 (length sent))))



