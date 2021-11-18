(setq KeyWord (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
(setq KW (list "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(setq Operator (list "+" "-" "/" "**" "*" "(" ")" "\"" "\"" ","))
(setq OP (list "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_DBLMULT" "OP_MULT" "OP_OP" "OP_CP" "OP_OC" "OP_CC" "OP_COMMA"))

(setq Space (list "\n" "\t" " "))
(setq Comment ";")
(setq Possible (list "(" ")" "\""))
(setq opoc 0)

(setq tokens (list))


(defun gppinterpreter ()
    "Call split-word func in a while loop"
    (loop 
        (evalline (read-line))

        ; (setq line (read-line))
        ; (setq check (evalline line))
        ; (when (= check -1) (return))
    )

)


(defun evalline (line)
	(let ((words (list)) (tempword ""))
        ; remove newline tab space from given line
		(setq line (string-trim '(#\Space #\Tab #\Newline) line))
		(setq words (split-str line))
		(loop for word in words
			do
                (format t "Token: |~a|" word)
                ; remove empty spaces from word
                (setq word (string-trim '(#\Space #\Tab #\Newline) word))
                (evalword word)
		)		
	)
)

(defun evalword (word)
	(let ((len (length word)) (subword) (j 0) (res) (temp) (check 0) (id 0))
		(loop for i from 1 to len
			do

            (if (= check 1) (setq check 0) )
            (setq subword (string-downcase (subseq word j i)))

            (format t "Subword: ~a ~%" subword)

            ; Check if subword is a operator
            (setq isOperatorResult (isOperator subword))
            (if (not (equal isOperatorResult nil))
                (progn 
                    (setq j i) 
                    (setq check 1)
                )
            )	

            (if (= check 0)
            (progn
            (setq res (findinList subword KeyWord))
            (if (not (equal res nil))
                (if (= i len)
                    (progn 
                        ; (setq tokens (append tokens (list subword))) 
                        (print (nth res KW)) 
                        (setq check 1)
                    )
                    (progn
                        (setq temp (subseq word i (+ i 1)))
                        (format t "temp: ~a ~%" temp)

                        (if (equal (findinList temp Possible) nil)
                            (if (equal (isID (concatenate 'string subword temp)) nil) 
                                (progn (setq check -1) (format t "HERE1 ~S can not be tokenized.~%" (subseq word j len)))
                            )
                            (progn 
                                (print (nth res KW)) 
                                (setq j i) 
                                (setq check 1)
                            )
                        )
                    )
                )
            )
            ))
            

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
                                (progn (setq check -1) (format t "HERE2 ~S can not be tokenized." (subseq word j len)) (terpri))
                                (progn (setq tokens (append tokens (list subword))) (write "VALUE") (terpri) (setq j i) (setq check 1))
                            )
                        )
                    )
                )	
            )

			(if (= check 0)
            (if (string= subword Comment)
                (if (and (< i len) (string= (subseq word i (+ i 1)) Comment))
                    (progn (setq tokens (append tokens (list "COMMENT"))) (write "COMMENT") (terpri) (setq j i) (setq check 2))
                )
            ))

				
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
                                        (progn (setq check -1) (format t "HERE3 ~S can not be tokenized." (subseq word j len)) (terpri))
                                        (progn (setq tokens (append tokens (list subword))) (write "IDENTIFIER") (terpri) (setq j i) (setq check 1))
                                    )
                                )
                            )
                        )
                    )
                    (progn (setq check -1) (format t "HERE4 ~S can not be tokenized." (subseq word j len)) (terpri))
                )
            ))	


            (setq check (evaluate check))
            (if (or (= check -1) (= check 2)) (return check))

		)

		check			
	)
)

(defun isOperator (word)
    (setq res (findinList word Operator))
    (if (not (equal res nil))
        (progn
            ; check subword is " . 
            ; If it is, then increment opoc value for close or open
            (if (equal res 7) 
                (progn (setq res (+ res (mod opoc 2))) (setq opoc (+ opoc 1)))
            )
            (print (nth res OP))
        )
    )
    res
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
    (format t "~S ~S ~S ~%" word (car complist) i)
	(if (null complist)
		nil
		(if (string= word (car complist))
			i
			(findinList word (cdr complist) (+ i 1))
		)
	)
)

(defun isID (word)
    (format t "ISID: ~a ~%" word)

	(let ((len (- (length word) 1)) (chr) (res t))

		(loop for i from 0 to len
			do
			(progn
				(setq chr (char word i))
				(if (= i 0)
					(if (or (alpha-char-p chr) (char= chr #\_)) 
                        (setq res t) 
                        (setq res nil)
                    )
					(if (or (alpha-char-p chr) (digit-char-p chr) (char= chr #\_)) 
                        (setq res t) 
                        (setq res nil)
                    )
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

(gppinterpreter)
