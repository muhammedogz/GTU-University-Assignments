(setq KeyWord (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
(setq KW (list "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(setq Operator (list "+" "-" "/" "**" "*" "(" ")" "\"" "\"" ","))
(setq OP (list "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_DBLMULT" "OP_MULT" "OP_OP" "OP_CP" "OP_OC" "OP_CC" "OP_COMMA"))

(setq Space (list "\n" "\t" " "))
(setq Comment ";")
(setq Possible (list "(" ")" "\""))
(setq opoc 0)

(defvar exitValue 0)

(defun gppinterpreter ()
    "Call splitLine func in a while loop"
    (format t "Welcome to perfect lisp parser. Type (exit) to exit from program ~%")
    (loop 
        (splitLine (read-line))
        (if (equal exitValue 1)
            (progn
                (setq exitValue 0)
                (format t "Exiting from parser! Have a good day. ~%")
                (return)
            )
        )
    )

)


(defun splitLine (line)
    "Take line input and divide it into words list"
	(let ((words (list)))
        ; remove newline tab space from given line
		(setq line (string-trim '(#\Space #\Tab #\Newline) line))
		(setq words (strSplit line))
		(loop for word in words
			do
                ; remove empty spaces from word
                (setq word (string-trim '(#\Space #\Tab #\Newline) word))

                (if (string= word "(exit)")
                    (progn
                        (setq exitValue 1)
                        (return)
                    )
                )

                ; when a comment line entered, do not check other inputs
                (if (equal (splitWord word) 2)
                    (return)
                )
		)		
	)
)

(defvar check 0)

(defun splitWord (word)
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
    (setq res (searchList word Operator))
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
    (setq res (searchList subword KeyWord))
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
                (if (equal (searchList temp Possible) nil)
                    (if (equal (isID (concatenate 'string subword temp)) nil) 
                        (progn
                            (format t "ERROR ~S can not be tokenized.~%" (subseq subword j len))
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
                    (if (equal (searchList (subseq word i (+ i 1)) Possible) nil)
                        (progn
                            (format t "ERROR2 ~S can not be tokenized.~%" (subseq word j len))
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
                        (if (equal (searchList temp Possible) nil)
                            (progn 
                                (setq check -1) 
                                (format t "ERROR ~S can not be tokenized. ~%" (subseq word j len))
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
            (format t "ERROR ~S can not be tokenized.~%" (subseq word j len))
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

; splits given stirng to words that represented as a list
(defun strSplit (str)
  (strSplitHelper str))

; this is predefined function
; I took some help from stackoverflow.
(defun strSplitHelper (str &optional (r nil))
  (let ((n (position " " str
		     :from-end t
		     :test #'
             (lambda (x y) (find y x :test #'string=)) )))
    (if n
	(strSplitHelper (subseq str 0 n) (cons (subseq str (1+ n)) r))
      (cons str r)))
)

; search in the given list recursively
(defun searchList (word listCheck &optional (i 0))
    ; (format t "~S ~S ~S ~%" word (car complist) i)
	(if (null listCheck)
		nil
		(if (string= word (car listCheck))
			i
			(searchList word (cdr listCheck) (+ i 1))
		)
	)
)

; call the desired function
(gppinterpreter)
