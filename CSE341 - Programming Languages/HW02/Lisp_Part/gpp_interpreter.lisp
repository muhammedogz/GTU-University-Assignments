(load "gpp_lexer.lisp")

(defun gppinterpreter ()
    "Call splitLine func in a while loop"
    (format t "Welcome to perfect lisp interpreter. Type (exit) or enter two newline to exit from program ~%")
    (loop 

        ; if you are using terminal to enter inputs. You can uncommnet this line
        ; (format t "> ") ; prints this before taking input

        (determineTokenType)

        ; if newlineCount == 2 break the loop condition
        (if (= newLineCount 2)
            (progn
                (setq exitValue 1)
            )
        )

        (start)


        (if (equal exitValue 1) ; if exitValue is 1, terminate the program
            (progn
                (setq exitValue 0)
                (format t "~a ~%" tokenType)
                (format t "Exiting from interpreter! Have a good day. ~%")
                (return)
            )
        )

        (format t "~a ~%" tokenType)
        (format t "~a ~%" valueList)

        (setq tokenType (list))
        (setq valueList (list))
    )
)

; Use top down parser to parse the input
(defun start () ;(isEXPLISTI EXPILISTI) (isCOMMENT COMMENT)
    (setf isExpi (EXPI))

    (if (equal isExpi nil)
        (progn
            (printLn "Syntax Error!")
            (setq exitValue 1)
        )
        (progn
            (printLn "Syntax OK!")
            (format t "Result: ~a ~%" isExpi)
        )
    )
)

(defun EXPI ()
    (setf expiValue nil)
    (if (equal (length tokenType) 5)
        (progn 
            (if (and (string= (nth 0 tokenType) "OP_OP") (string= (nth 4 tokenType) "OP_CP"))
                (progn
                    ; look for + operation
                    (if (string= (nth 1 tokenType) "OP_PLUS")
                        (setf expiValue (opPLUS))
                    )

                    ; look for - operation	
                    (if (string= (nth 1 tokenType) "OP_MINUS")
                        (setf expiValue (opMINUS))
                    )

                    ; look for * operation
                    (if (string= (nth 1 tokenType) "OP_MULT")
                        (setf expiValue (opMULT))
                    )

                    ; look for / operation
                    (if (string= (nth 1 tokenType) "OP_DIV")
                        (setf expiValue (opDIV))
                    )

                    ; OP_OP KW_SET IDENTIFIER EXPI OP_CP
                    (if (string= (nth 1 tokenType) "KW_SET")
                        (setf expiValue (opSET))
                    )
                    
                    
                )
                    
            )
        )
    )
    (if (equal (length tokenType) 6)
        (progn
            (if (and (string= (nth 0 tokenType) "OP_OP") (string= (nth 5 tokenType) "OP_CP"))
                (progn

                    ; look for ** operation
                    (if (and (string= (nth 1 tokenType) "OP_MULT") (string= (nth 2 tokenType) "OP_MULT"))
                        (setf expiValue (opDBLMULT))
                    )

                )
            )
        )
    )
    ; return
    expiValue
)


(defun opPLUS ()
    (if (and (string= (nth 2 tokenType) "VALUE") (string= (nth 3 tokenType) "VALUE"))
        (+ (nth 0 valueList) (nth 1 valueList))
    )
)

(defun opMINUS ()
    (if (and (string= (nth 2 tokenType) "VALUE") (string= (nth 3 tokenType) "VALUE"))
        (- (nth 0 valueList) (nth 1 valueList))
    )
)

(defun opMULT ()
    (if (and (string= (nth 2 tokenType) "VALUE") (string= (nth 3 tokenType) "VALUE"))
        (* (nth 0 valueList) (nth 1 valueList))
    )
)

(defun opDIV ()
    (if (and (string= (nth 2 tokenType) "VALUE") (string= (nth 3 tokenType) "VALUE"))
        (progn
            (if (equal (nth 1 valueList) 0)
                (progn
                    (format t "Division by zero error! ~%")
                    nil
                )
                (/ (nth 0 valueList) (nth 1 valueList))
            )
        )
    )
)

(defun opDBLMULT ()
    (if (and (string= (nth 3 tokenType) "VALUE") (string= (nth 4 tokenType) "VALUE"))
        (expt (nth 0 valueList) (nth 1 valueList))
    )
)

(defun opSET ()
    (if (and (string= (nth 2 tokenType) "IDENTIFIER") (string= (nth 3 tokenType) "VALUE"))
        (progn
            (addToListTail (nth 0 identifierListTemp) identifierList)
            (addToListTail (nth 0 valueList) identifierValueList)
            (nth 0 valueList)
        )
    )
)


(gppinterpreter)


