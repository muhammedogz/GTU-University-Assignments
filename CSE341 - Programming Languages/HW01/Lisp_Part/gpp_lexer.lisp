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
		(setq words (strSplit line))
		(loop for word in words
			do
                (format t "Token: |~a| ~%a" word)
                ; remove empty spaces from word
                (setq word (string-trim '(#\Space #\Tab #\Newline) word))
                
                ; when a comment line entered, do not check other inputs
                (if (equal (evalword word) 2)
                    (return)
                )
		)		
	)
)

(defvar check 0)

(defun evalword (word)
	(let ((len (length word)) (subword) (j 0) (res) (temp) (id 0))
        (setq check 0)
		(loop for i from 1 to len
			do

            (if (= check 1) (setq check 0) )
            (setq subword (string-downcase (subseq word j i)))

            (format t "~%Subword: ~a ~%" subword)
            
            ; Check if subword is a operator
            (if (and (equal check 0) (not (equal (isOperator subword) nil)) )
                (progn 
                    (print "--operator--")
                    (setq j i) 
                    (setq check 1)
                )
            )	

            ; Check if subword is a keyword
            (setq isKeywordValue (isKeyWord word subword i len))
            (if (and (equal check 0) (not (equal isKeywordValue nil)) )
                (progn 
                    (if (equal isKeywordValue 1)
                        (setq j i)
                    )
                    (setq check 1)
                )
            )

            ; Check if subword is a value
            (setq isValueNum (isValue word subword i j len))
            (if (and (equal check 0) (not (equal isValueNum nil)) )
                (progn 
                    (setq i isValueNum)
                    (setq j i)
                    (setq check 1)
                )
            )

            ; check if subword is a comment
            ; first and second letters are: ;
            (if (and (equal check 0) (>= len 2) (string= (subseq word 0 1) Comment)) 
                (if (string= (subseq word 1 2) Comment)
                    (progn  (print "COMMENT")  (setq check 2))
                )
            )

            ; check of if given token is identifier or not
            ; also checks given token is valid or not
            (if (equal check 0)
                (progn
                    (setq isIdentifierValue (isIdentifier word subword i j len))
                    (if (not (equal isIdentifierValue nil)) 
                        (progn 
                            (if (equal isIdentifierValue 1)
                                (setq j i)  
                            )
                            (setq check 1)
                        )
                    )
                )   
            )

            

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

(defun isKeyword (word subword i len)
    (setq returnValue nil)
    (setq res (findinList subword KeyWord))
    (if (not (equal res nil))
        (if (>= i len)
            (progn
                (print (nth res KW))
                (setq returnValue 0)
            )
            ; else
            (progn
                (setq temp (subseq word i (+ i 1)))
                (print temp)
                (if (equal (findinList temp Possible) nil)
                    (if (equal (isID (concatenate 'string subword temp)) nil) 
                        (progn
                            (format t "HERE1 ~S can not be tokenized.~%" (subseq subword j len))
                            (setq check -1)
                        )
                    )
                    (progn
                        (print (nth res KW))
                        (setq returnValue 1)
                     
                    )

                )
            )
        )
    )
    returnValue
)
    
(defun isValue (word subword i j len)
    (setq returnValue nil)
    (setq res (isValueHelper subword))
    (if (not (equal res nil))
        (progn
            (loop
                (setq i (+ i 1))
                (when (or (equal (isValueHelper (subseq word j (- i 1))) nil) (> i len)) 
                    (return))
            )
            (setq i (- i 1))
            (if (equal (isValueHelper (subseq word j i)) nil) 
                (progn
                    (setq i (- i 1))
                    (if (equal (findinList (subseq word i (+ i 1)) Possible) nil)
                        (progn
                            (format t "HERE2 ~S can not be tokenized.~%" (subseq word j len))
                            (setq check -1)
                        )
                        (print "VALUE")
                    )
                )
                (print "VALUE") 
            )	
            (setq returnValue i)							     
        )	
    )
    returnValue
)

; checks given string contains digit characters or not
(defun isValueHelper (word)
	(let ((letter "") (res t))
		(if (equal (every #'digit-char-p word) nil)
			(setq res nil) ; if there is a word that is not a digit
			(setq res t) ; if every word is a digit
		)
		res	
	)
)

(defun isIdentifier (word subword i j len)
    (setq returnValue nil)
    (setq res (isIdentifierHelper subword))
    (if (and (equal check 0) (equal res t) )
        (if (= i len)
            (progn 
                (print "IDENTIFIER")  
                (setq returnValue 0)
            )
            (progn
                (setq temp(subseq word j (+ i 1)))
                (setq id (isIdentifierHelper temp))
                (if (not (equal res id))
                    (progn
                        (setq temp (subseq word i (+ i 1)))
                        (if (equal (findinList temp Possible) nil)
                            (progn 
                                (setq check -1) 
                                (format t "HERE3 ~S can not be tokenized. ~%" (subseq word j len))
                            )
                            (progn 
                                (print "IDENTIFIER") 
                                (setq returnValue 1) 
                            )
                        )
                    )
                )
            )
        )
        (progn
            (format t "HERE4 ~S can not be tokenized.~%" (subseq word j len))
            (setq check -1)
        )
    )
    returnValue
)

(defun isIdentifierHelper (word)
	(let ((len (- (length word) 1)) (letter "") (res t))

		(loop for i from 0 to len
			do
			(progn
				(setq letter (char word i))
				(if (= i 0)
					(if (or (alpha-char-p letter) (char= letter #\_)) 
                        (setq res t) 
                        (setq res nil)
                    )
					(if (or (alpha-char-p letter) (digit-char-p letter) (char= letter #\_)) 
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

(defun strSplit (str)
  (strSplitHelper str " "))

(defun strSplitHelper (string (separator " ") &optional  (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(strSplitHelper (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))

(defun findinList (word complist &optional (i 0))
    ; (format t "~S ~S ~S ~%" word (car complist) i)
	(if (null complist)
		nil
		(if (string= word (car complist))
			i
			(findinList word (cdr complist) (+ i 1))
		)
	)
)



(gppinterpreter)
