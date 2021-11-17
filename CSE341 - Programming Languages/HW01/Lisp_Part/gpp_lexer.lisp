(defvar KeyWord (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
(defvar KW (list "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(defvar Operator (list "+" "-" "/" "**" "*" "(" ")" "\"" "\"" ","))
(defvar OP (list "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_DBLMULT" "OP_MULT" "OP_OP" "OP_CP" "OP_OC" "OP_CC" "OP_COMMA"))

(defvar Space (list "\n" "\t" " "))
(defvar Comment ";")
(defvar Possible (list "(" ")" "\""))
(defvar opoc 0)

(defvar tokens (list))


(defun gppinterpreter (&optional (filename -1))
	(if (equal filename -1)
		(let ((line) (check))
			(loop 
			   (setq line (read-line))
			   (setq check (evalline line))
			   (terpri)
			   (when (= check -1) (return))
			)
		)
		(let ((in (open filename :if-does-not-exist nil)))
   			(when in
      			(loop for line = (read-line in nil)
      
      			while line do (evalline line))
      			(close in)
   			)
		)
	)
)

(defun evalline (line)
	(let ((words) (res 0) (tempword))
		(setq line (string-trim '(#\Space #\Tab #\Newline) line))
		(setq words (split-str line))
		(loop for word in words
			do
			(progn
				(setq tempword (string-trim '(#\Space #\Tab #\Newline) word))
				(setq res (evalword tempword))
				(if (or (= res 2) (= res -1)) (return res))
			)
		)
		res			
	)
)

(defun evalword (word)
	(let ((len (length word)) (subword) (j 0) (res) (temp) (check 0) (id 0))
		(loop for i from 1 to len
			do
			(progn
				(if (= check 1) (setq check 0) )
				(setq subword (string-downcase (subseq word j i)))

				;; Check wheter subword is operator or not.
				(if (= check 0)
					(progn
						(setq res (findinList subword Operator))
						(if (not (equal res nil))
							(progn
								(if (equal res 4)
									(if (and (< i len) (string= (subseq word i (+ i 1)) "*")) (setq res 3))
								)
								(if (equal res 7) (progn (setq res (+ res (mod opoc 2))) (setq opoc (+ opoc 1))))
							
								(if (or (equal res 5) (equal res 6) (equal res 7) (equal res 9))
									(progn (setq tokens (append tokens (list subword))) (write (nth res OP)) (terpri) (setq j i) (setq check 1))
									
									(if (>= i len)
										(progn (setq tokens (append tokens (list subword))) (write (nth res OP)) (terpri) (setq check 1))
										(progn
										 	(setq temp (subseq word i (+ i 1)))
										 	(if (equal (findinList temp Possible) nil)
										 		(progn (setq check -1) (format t "ERROR ~S can not be tokenized." (subseq word j len)) (terpri))
										 		(progn (setq tokens (append tokens (list subword))) (write (nth res OP)) (terpri) (setq j i) (setq check 1))
										 	)
										)
									)	
								)
							)	
						)
					)	
				)

				
				(if (= check 0)
					(progn
						(setq res (findinList subword KeyWord))
						(if (not (equal res nil))
							(if (>= i len)
								(progn (setq tokens (append tokens (list subword))) (write (nth res KW)) (terpri) (setq check 1))
								(progn
								 	(setq temp (subseq word i (+ i 1)))
								 	;; After these keywords, Only possible (defined above) tokens can come.
								 	(if (and (equal (findinList temp Possible) nil))
								 		(if (equal (isID (concatenate 'string subword temp)) nil) 
								 			(progn (setq check -1) (format t "ERROR ~S can not be tokenized." (subseq word j len)) (terpri))
								 		)
								 		(progn (setq tokens (append tokens (list subword))) (write (nth res KW)) (terpri) (setq j i) (setq check 1))
								 	)
								)
							)
						)
					)	
				)

				
				(if (= check 0)
					(progn
						(setq res (isVal subword))
						(if (not (equal res nil))
							(progn
								(loop
									(setq temp (string-downcase (subseq word j i)))
									(setq i (+ i 1))
									(when (or (equal (isVal temp) nil) (> i len)) (return))
								)
								(setq i (- i 1))
								(if (equal (isVal temp) nil) (setq i (- i 1)))								
								(if (>= i len)
									(progn (setq tokens (append tokens (list subword))) (write "VALUE") (terpri) (setq check 1))
									(progn
									 	(setq temp (subseq word i (+ i 1)))
									 	(if (equal (findinList temp Possible) nil)
									 		(progn (setq check -1) (format t "ERROR ~S can not be tokenized." (subseq word j len)) (terpri))
									 		(progn (setq tokens (append tokens (list subword))) (write "VALUE") (terpri) (setq j i) (setq check 1))
									 	)
									)
								)
							)	
						)
					)
				)

				
				(if (and (= check 0) (string= subword Comment))
						(if (and (< i len) (string= (subseq word i (+ i 1)) Comment))
							(progn (setq tokens (append tokens (list "COMMENT"))) (write "COMMENT") (terpri) (setq j i) (setq check 2))
						)
				)

				
				(if (= check 0)
					(progn
						(setq res (isID subword))
						(if (equal res t)
							(if (= i len)
								(progn (setq tokens (append tokens (list subword))) (write "IDENTIFIER") (terpri)  (setq check 1))
								(progn
									(setq temp (string-downcase (subseq word j (+ i 1))))
									(setq id (isID temp))
									(if (equal res id)
										()
										(progn
										 	(setq temp (subseq word i (+ i 1)))
										 	(if (equal (findinList temp Possible) nil)
										 		(progn (setq check -1) (format t "ERROR ~S can not be tokenized." (subseq word j len)) (terpri))
										 		(progn (setq tokens (append tokens (list subword))) (write "IDENTIFIER") (terpri) (setq j i) (setq check 1))
										 	)
										)
									)
								)
							)
							(progn (setq check -1) (format t "ERROR ~S can not be tokenized." (subseq word j len)) (terpri))
						)
					)	
				)
				(setq check (evaluate check))
				(if (or (= check -1) (= check 2)) (return check))

			)
		)
		check			
	)
)

(defun split-str (string &optional (separator " "))
  (split-1 string separator))

(defun split-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))

(defun findinList (word complist &optional (i 0))
	(if (null complist)
		nil
		(if (string= word (car complist))
			i
			(findinList word (cdr complist) (+ i 1))
		)
	)
)

(defun isID (word)
	(let ((len (- (length word) 1)) (chr) (res t))

		(loop for i from 0 to len
			do
			(progn
				(setq chr (char word i))
				(if (= i 0)
					(if (or (alpha-char-p chr) (char= chr #\_)) (setq res t) (setq res nil))
					(if (or (alpha-char-p chr) (digit-char-p chr) (char= chr #\_)) () (setq res nil))
				)
				(if (equal res nil) (return res))
			)
		)
		res
	)
)

;; is subword a value
(defun isVal (word)
	(let ((chr) (res t))
		(if (equal (every #'digit-char-p word) nil)
			(setq res nil)
			(progn
				(if (= (length word) 1)
					(setq res t)
					(progn
						(setq chr (char word 0))
						(if (equal (digit-char-p chr) 0) (setq res nil) (setq res t))
					)
				)		
			)
		)
		res	
	)
)

(defun evaluate (check)
	(let ((len (list-length tokens)) (res check))

		(if (> len 2)
			(if (and (string= (nth (- len 3) tokens) "(") (string= (nth (- len 2) tokens) "exit")  (string= (nth (- len 1) tokens) ")")) (setq res -1))
		)
		res
	)
)

(defvar filename)

(defun work ()
	(let ((rline) (res))
		(setq rline (read-line))
		(terpri)
		(setq rline (string-trim '(#\Space #\Tab #\Newline) rline))
		(if (string= rline "(gppinterpreter)")
			(setq res 1)
			(progn
				(setq rline (split-str rline "\"" ))
				(if (and (= (list-length rline) 3) (string= "(gppinterpreter " (nth 0 rline)) (string= ")" (nth 2 rline)))   
					(progn (setq filename (nth 1 rline)) (setq res 2))
					(setq res -1)
				)
			)
		)
		res
	)
)

(write "please read ReadMe")

(defvar run (work))
(if (= run 1) (gppinterpreter))
(if (= run 2) (gppinterpreter filename))
    (if (= run -1) (write "G++ Starting method is incorrect."))
