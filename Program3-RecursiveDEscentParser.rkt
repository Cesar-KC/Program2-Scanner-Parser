#lang racket
;;------------PHASE1: TOKENS-----------------

;NOTES: We need to define tokens that scanner can correctly proudce list of
     ;Parser will take tokens and parser then based on grammar rules to get expected output


;struct to define token and type/value
(struct token (type value) #:transparent)               ;LLM guided token struct design (A)

;; --- ALL VALID TOKENS ---
;for documentaion, dont intend to use token-type, but these are the tokens i want to create from valid source input
(define token-types
  
  '(IF-TOK THEN-TOK ELSE-TOK WHILE-TOK DO-TOK END-TOK PRINT-TOK ; keywords

    ID-TOK INT-TOK FP-TOK ; ID/Num
     
    PLUS-TOK MINUS-TOK MULT-TOK DIV-TOK  ; arithmetic
  
    ASSIGN-TOK  ; assigning
    
    EQ-TOK NEQ-TOK GT-TOK GTE-TOK LT-TOK LTE-TOK ; relational
   
    SEMI-TOK LPAREN-TOK RPAREN-TOK  ; delims
    
    EOF-TOK))  ;end-of                                                



;;-----------PHASE2: SCANNER-------------

;NOTES: We need 'strip_comments' and 'tokenize', then we can define 'scan'



;;--------STRIP-COMMENTS IMPLEMENTATION---------

(define (strip-comments source)
  
  ; check if source is empty before anything, throw error is so
  (when (string=? source "")
    (error "Lexical Error: source input is empty"))
  
  (define (helper chars in-comment?)
    (cond
      ; empty list while in comment , throw error, unclosed comment occured
      [(and (null? chars) in-comment?)
       (error "Lexical Error: unclosed comment, missing */")]
      
      ; Base case: empty list and not in comment, signals we are done
      [(null? chars) '()]
      
      ; in comment, found end '*/'  , exit comment, add space
      [(and in-comment?
            (char=? (car chars) #\*)
            (not (null? (cdr chars)))
            (char=? (cadr chars) #\/))
       (cons #\space (helper (cddr chars) #f))]
      
      ; in comment, all else we ignored, keep going until *
      [in-comment?
       (helper (cdr chars) #t)]
      
      ; found /*, start of comment
      [(and (char=? (car chars) #\/)
            (not (null? (cdr chars)))
            (char=? (cadr chars) #\*))
       (helper (cddr chars) #t)]
      
      ;normal char that is outside comment block, keep it
      [else
       (cons (car chars) (helper (cdr chars) #f))]))
  
  
  (list->string (helper (string->list source) #f)))


;NOTE: add nesting functioanlity to 'strip-comments'



;;--------------------TOKENIZE IMPLEMENTATION----------------------

;----FUNCTION FOR VALID NUMBER-------
;Used by tokeinze below

(define (make-number-token num-str)                   ;Code from LLM, with some alternations (B)

  (cond
    ; FP: optional sign, digits, decimal, digits
    [(regexp-match? #px"^[+-]?[0-9]+\\.[0-9]+$" num-str)
     (token 'FP-TOK (string->number num-str))]

    ; FP: trailing decimal allowed
    [(regexp-match? #px"^[+-]?[0-9]+\\.$" num-str)
     (token 'FP-TOK (string->number (string-append num-str "0")))]
    
    ; INT: bare 0 exception
    [(regexp-match? #px"^[+-]?0$" num-str)
     (token 'INT-TOK 0)]
    
    ; INT: optional sign, non-zero leading digit
    [(regexp-match? #px"^[+-]?[1-9][0-9]*$" num-str)
     (token 'INT-TOK (string->number num-str))]
    
    ; anything else is a lexical error
    [else
     (error "Lexical Error: invalid number literal")]))



;;-------TOKEINZE FUNCTION---------------------

;;------------ TOKENIZE ---------------
(define (tokenize source)
  ;letrec allows for three local functions
  (letrec
    (; function to collect valid word characters
     [collect-word
      (lambda (chars)
        (cond
          [(null? chars) '()]
          [(or (char-alphabetic? (car chars))
               (char-numeric? (car chars))
               (char=? (car chars) #\_)
               (char=? (car chars) #\-))
           (cons (car chars) (collect-word (cdr chars)))]
          [else '()]))]

     ; function to collect valid number characters, digits and decimals
     [collect-number
      (lambda (chars)
        (cond
          [(null? chars) '()]
          [(or (char-numeric? (car chars))
               (char=? (car chars) #\.))
           (cons (car chars) (collect-number (cdr chars)))]
          [else '()]))]

     ; main function, recursive, walks character list and produces token list, has 'prev-token' to clear amiguity of unary vs binary
     [helper
      (lambda (chars prev-token)
        (cond

          ; Base case: nothing left, return empty list
          [(null? chars) '()]

          ; whitespace , skip and keep going
          [(char-whitespace? (car chars))
           (helper (cdr chars) prev-token)]

          ; assign :=, : by itself, invalid, throw error
          [(char=? (car chars) #\:)
           (if (and (not (null? (cdr chars)))
                    (char=? (cadr chars) #\=))
               (cons (token 'ASSIGN-TOK #f)
                     (helper (cddr chars) (token 'ASSIGN-TOK #f)))
               (error "Lexical Error: expected ':='"))]

          ; assign !=, not equal, ! itself , throw error
          [(char=? (car chars) #\!)
           (if (and (not (null? (cdr chars)))
                    (char=? (cadr chars) #\=))
               (cons (token 'NEQ-TOK #f)
                     (helper (cddr chars) (token 'NEQ-TOK #f)))
               (error "Lexical Error: expected '!='"))]

          ; >= or >
          [(char=? (car chars) #\>)
           (if (and (not (null? (cdr chars)))
                    (char=? (cadr chars) #\=))
               (cons (token 'GTE-TOK #f)
                     (helper (cddr chars) (token 'GTE-TOK #f)))
               (cons (token 'GT-TOK #f)
                     (helper (cdr chars) (token 'GT-TOK #f))))]

          ; <= or 
          [(char=? (car chars) #\<)
           (if (and (not (null? (cdr chars)))
                    (char=? (cadr chars) #\=))
               (cons (token 'LTE-TOK #f)
                     (helper (cddr chars) (token 'LTE-TOK #f)))
               (cons (token 'LT-TOK #f)
                     (helper (cdr chars) (token 'LT-TOK #f))))]

          ;---More basic token assignment below---
          ; =
          [(char=? (car chars) #\=)
           (cons (token 'EQ-TOK #f)
                 (helper (cdr chars) (token 'EQ-TOK #f)))]

          ; ;
          [(char=? (car chars) #\;)
           (cons (token 'SEMI-TOK #f)
                 (helper (cdr chars) (token 'SEMI-TOK #f)))]

          ; (
          [(char=? (car chars) #\()
           (cons (token 'LPAREN-TOK #f)
                 (helper (cdr chars) (token 'LPAREN-TOK #f)))]

          ; )
          [(char=? (car chars) #\))
           (cons (token 'RPAREN-TOK #f)
                 (helper (cdr chars) (token 'RPAREN-TOK #f)))]

          ; *
          [(char=? (car chars) #\*)
           (cons (token 'MULT-TOK #f)
                 (helper (cdr chars) (token 'MULT-TOK #f)))]

          ; /
          [(char=? (car chars) #\/)
           (cons (token 'DIV-TOK #f)
                 (helper (cdr chars) (token 'DIV-TOK #f)))]

          ;----here we check if prev-token is valid, then operation, else assignment(unary)
          
          ; + unary or binary
          [(char=? (car chars) #\+)
           (if (and prev-token
                    (member (token-type prev-token)
                            '(ID-TOK INT-TOK FP-TOK RPAREN-TOK)))
               (cons (token 'PLUS-TOK #f)
                     (helper (cdr chars) (token 'PLUS-TOK #f)))
               (let* ([num-chars (cons (car chars) (collect-number (cdr chars)))]
                      [num-str   (list->string num-chars)]
                      [remaining (list-tail chars (length num-chars))]
                      [num-tok   (make-number-token num-str)])
                 (cons num-tok (helper remaining num-tok))))]

          ; - unary or binary
          [(char=? (car chars) #\-)
           (if (and prev-token
                    (member (token-type prev-token)
                            '(ID-TOK INT-TOK FP-TOK RPAREN-TOK)))
               (cons (token 'MINUS-TOK #f)
                     (helper (cdr chars) (token 'MINUS-TOK #f)))
               (let* ([num-chars (cons (car chars) (collect-number (cdr chars)))]
                      [num-str   (list->string num-chars)]
                      [remaining (list-tail chars (length num-chars))]
                      [num-tok   (make-number-token num-str)])
                 (cons num-tok (helper remaining num-tok))))]


          
          ; numbers, we used defined 'make-number-token' to classify float or int
          [(char-numeric? (car chars))
           (let* ([num-chars (collect-number chars)]
                  [num-str   (list->string num-chars)]
                  [remaining (list-tail chars (length num-chars))]
                  [num-tok   (make-number-token num-str)])
             (if (and (not (null? remaining))
                      (char-alphabetic? (car remaining)))
                 (error "Lexical Error: invalid token, number followed by letter")
                 (cons num-tok (helper remaining num-tok))))]

          ; Keywords, else its an identifier
          [(or (char-alphabetic? (car chars))
               (char=? (car chars) #\_))
           (let* ([word-chars (collect-word chars)]
                  [word       (list->string word-chars)]
                  [remaining  (list-tail chars (length word-chars))]
                  [word-tok   (cond
                                [(string=? word "IF")    (token 'IF-TOK #f)]
                                [(string=? word "THEN")  (token 'THEN-TOK #f)]
                                [(string=? word "ELSE")  (token 'ELSE-TOK #f)]
                                [(string=? word "WHILE") (token 'WHILE-TOK #f)]
                                [(string=? word "DO")    (token 'DO-TOK #f)]
                                [(string=? word "END")   (token 'END-TOK #f)]
                                [(string=? word "PRINT") (token 'PRINT-TOK #f)]
                                [else                    (token 'ID-TOK word)])])
             (cons word-tok (helper remaining word-tok)))]

          ; invalid character, throw error
          [else
           (error "Lexical Error: invalid character")]))])

    (helper (string->list source) #f)))



;;---------SCANNER---------

; Simply create a scanner by creating function that takes in source input, strip comments, and creates tokens based on source with no comments
; We can do so by calling our defined functions above!

(define (scan source)
 (tokenize (strip-comments source)))




;------------PHASE3: PARSER-----------------

;NOTES: Two helper functions(consume, peek), then write parser

(define (peek tokens)               ;LLM provided 'peek' function (C), but changed #f to END-TOK, alter to return token struct
  (if (null? tokens)
      (token 'EOF-TOK #f)        
      (car tokens)))

(define (consume tokens expected-type)    ; here we use 'token-type' from our token struct to , if equal return, else error
  (if (null? tokens)
      (error "Syntax Error: unexpected end of input")
      (if (equal? (token-type (car tokens)) expected-type)
          (cdr tokens)
          (error "Syntax Error: unexpected token"))))

; optionally consume a semicolon after END if present
(define (consume-optional-semi tokens)
  (if (and (not (null? tokens))
           (equal? (token-type (car tokens)) 'SEMI-TOK))
      (cdr tokens)
      tokens))


;---------PARSER---------------

;We want to build our parser bottom up

;----Parse_factor-------

(define (parse-factor tokens)
  (let ([tok (peek tokens)])
    (cond

      ; if numeric
      [(or (equal? (token-type tok) 'INT-TOK)
           (equal? (token-type tok) 'FP-TOK))
       (cons (token-value tok) (cdr tokens))]

      ; if Id
      [(equal? (token-type tok) 'ID-TOK)
       (cons (string->symbol (token-value tok)) (cdr tokens))]

      ; if PArenthse
      [(equal? (token-type tok) 'LPAREN-TOK)
       (let* ([tokens1 (consume tokens 'LPAREN-TOK)]
              [result  (parse-expression tokens1)]     ; define parse expression
              [tokens2 (consume (cdr result) 'RPAREN-TOK)])
         (cons (car result) tokens2))]

      ; else, throw error
      [else
       (error "Syntax Error: expected number, identifier, or '('")])))

;------Parse_Term---------

(define (parse-term tokens)
  (let* ([r (parse-factor tokens)]) ; r stores parse_facotr
    (let loop ([left-ast (car r)]   ; left-AST is first of r, rest is rest
               [rest     (cdr r)])
      (let ([tok (peek rest)])
        (cond
          ; multiply term
          [(equal? (token-type tok) 'MULT-TOK)
           (let* ([t  (consume rest 'MULT-TOK)]
                  [r2 (parse-factor t)])
             (loop (list 'mult left-ast (car r2))
                   (cdr r2)))]

          ; Divide term
          [(equal? (token-type tok) 'DIV-TOK)
           (let* ([t  (consume rest 'DIV-TOK)]
                  [r2 (parse-factor t)])
             (loop (list 'div left-ast (car r2))
                   (cdr r2)))]

          ; anythign else, done, return
          [else
           (cons left-ast rest)])))))

;-----Parse_expression------

(define (parse-expression tokens)
  (let* ([r (parse-term tokens)])   ;r stores parse-expressions return
    (let loop ([left-ast (car r)]   ; left-ast first result in r
               [rest     (cdr r)])  ;rest is rest of pair(list)
      
      ;similar to parse_term, check for add/sub after checking for mult/div
      (let ([tok (peek rest)])
        (cond
          ; addition
          [(equal? (token-type tok) 'PLUS-TOK)
           (let* ([t  (consume rest 'PLUS-TOK)]
                  [r2 (parse-term t)])
             (loop (list 'plus left-ast (car r2))
                   (cdr r2)))]
          ; subtraction
          [(equal? (token-type tok) 'MINUS-TOK)
           (let* ([t  (consume rest 'MINUS-TOK)]
                  [r2 (parse-term t)])
             (loop (list 'minus left-ast (car r2))
                   (cdr r2)))]
          ; anything else, done, return 
          [else
           (cons left-ast rest)])))))

;------Parse_comparison-------
;needs to parse left , compare, parse right, return node

(define (parse-comparison tokens)
  (let* ([left  (parse-expression tokens)] ;call parse-expressions on tokens 
         [tok   (peek (cdr left))]  ;realtion op

         ;conditioanls for each token, convert to symbol
         [op    (cond
                  [(equal? (token-type tok) 'EQ-TOK)  'eq]
                  [(equal? (token-type tok) 'NEQ-TOK) 'neq]
                  [(equal? (token-type tok) 'GT-TOK)  'gt]
                  [(equal? (token-type tok) 'GTE-TOK) 'gte]
                  [(equal? (token-type tok) 'LT-TOK)  'lt]
                  [(equal? (token-type tok) 'LTE-TOK) 'lte]
                  [else (error "Syntax Error: expected relational operator")])]
         [t     (cdr (cdr left))] ;remaining tokesn
         [right (parse-expression t)])  ;parse right
    (cons (list op (car left) (car right))
          (cdr right))))


;------Parse_statemtn---------

;parse statement takes a 'peek' , then chooses correct parse statement version based on that

(define (parse-statement tokens)
  (let ([tok (peek tokens)])
    (cond
      ;  call if stamement
      [(equal? (token-type tok) 'IF-TOK)
       (parse-if-stmt tokens)]

      ; call while statemtn
      [(equal? (token-type tok) 'WHILE-TOK)
       (parse-while-stmt tokens)]

      ; id, assignemnt statemtn
      [(equal? (token-type tok) 'ID-TOK)
       (parse-assign-stmt tokens)]

      ; print statemtn
      [(equal? (token-type tok) 'PRINT-TOK)
       (parse-print-stmt tokens)]

      ; anything else, we throw error!
      [else
       (error "Syntax Error: unexpected token in statement")])))

;-------Parse-If--------

(define (parse-if-stmt tokens)
  ;several  checks for each token in an 'if-else'
  (let* ([t1       (consume tokens 'IF-TOK)]
         [cmp      (parse-comparison t1)]
         [t2       (consume (cdr cmp) 'THEN-TOK)]
         [then-body (parse-stmt-list t2)]
         [t3       (consume (cdr then-body) 'ELSE-TOK)]
         [else-body (parse-stmt-list t3)]
         [t4       (consume (cdr else-body) 'END-TOK)]
         [t5        (consume-optional-semi t4)]) 
    (cons (list 'if (car cmp)
                (list 'then (car then-body))
                (list 'else (car else-body)))
          t4)))


;-------Parse_while-----

;similar to parse-if, but for while loop
(define (parse-while-stmt tokens)
  (let* ([t1   (consume tokens 'WHILE-TOK)]
         [cmp  (parse-comparison t1)]
         [t2   (consume (cdr cmp) 'DO-TOK)]
         [body (parse-stmt-list t2)]
         [t3   (consume (cdr body) 'END-TOK)]
         [t4   (consume-optional-semi t3)])
    (cons (list 'while (car cmp) (car body))
          t3)))

;--------Parse_assign-------

;assign value to ID after assign token until semi-token

(define (parse-assign-stmt tokens)
  (let* ([id-tok (peek tokens)]
         [t1     (consume tokens 'ID-TOK)]
         [t2     (consume t1 'ASSIGN-TOK)]
         [expr   (parse-expression t2)]
         [t3     (consume (cdr expr) 'SEMI-TOK)])
    (cons (list 'assign (string->symbol (token-value id-tok)) (car expr))
          t3)))


;-------Parse_print---------

;Print tokens after print-token and until sem-token

(define (parse-print-stmt tokens)
  (let* ([t1   (consume tokens 'PRINT-TOK)]
         [expr (parse-expression t1)]
         [t2   (consume (cdr expr) 'SEMI-TOK)])
    (cons (list 'print (car expr))
          t2)))


;---------Parse-statement-List---------

;statemtns empy, tokens is not, check next token, if statetment,add to statements, else done

(define (parse-stmt-list tokens)
  (let loop ([stmts '()]
             [rest  tokens])
    (let ([tok (peek rest)])
      (cond
        ; skip stray semicolons between statements
        [(equal? (token-type tok) 'SEMI-TOK)
         (loop stmts (cdr rest))]
        
        ; valid statement starter, parse it
        [(member (token-type tok)
                 '(IF-TOK WHILE-TOK ID-TOK PRINT-TOK))
         (let ([result (parse-statement rest)])
           (loop (append stmts (list (car result)))
                 (cdr result)))]
        
        ; not a statement starter, done
        [else
         (cons stmts rest)]))))


;--------PARSE-PROGRAM------------

;this is the entry point, what weve all been waiting, everything coming together recursviely!
(define (parse-program tokens)
  (let* ([result (parse-stmt-list tokens)]) ;call parse-stmtn-list on tokens
    
    (if (equal? (token-type (peek (cdr result))) 'EOF-TOK) ; if token is EOf-tok, then all parsed, eturn
        
        (cons 'program (car result))  ; creates AST(pair of program(root) and others nodes)
        
        (error "Syntax Error: unexpected token at end"))))




;-----------------RDP-PROGRAM---------------------------------

(define (RDP source)
  (parse-program (scan source)))



;---------TESTS------------

(define test1
  "/* Simple code, should parse. */ 

X := 1; 

Y := 2; 

PRINT (X + Y)/2;")

(define test2
  "/* Nested control structures. Should compile. */ 



X := 10; 

Y := 2;

IF X > 10 THEN  

   WHILE Y > 0 DO   

      Y := Y - 1;

      IF Y = 0 THEN 

		PRINT 0;

      ELSE 

		Z := 0; /* SYNTAX REQUIRES ELSE CLAUSE */ 

	  END;  /* END IF CLAUSE */ 

   END;  /* END WHILE */ 

ELSE

	X := 10;

END;  /* END OUTER IF */")

(define test3
  "/* Lexical error on line 5 */ 



X := 10; 

Y := 2;

IF X > 10 THEN  // The '//' is not a comment symbol. 

   WHILE Y > 0 DO

      Y := Y - 1;

      IF Y = 0 THEN 

		PRINT 0;

      ELSE 

		Z := 0; /* SYNTAX REQUIRES ELSE CLAUSE */ 

	  END;  /* END IF CLAUSE */ 

   END;  /* END WHILE */ 

ELSE

	X := 10;

END;  /* END OUTER IF */")

(define test4
  "/* Lexical error on line 4 */ 



_X-2345ASDadlkj := 10; 

3Y+43 := 2;

IF _X-2345ASDadlkj > 10 THEN   

   WHILE Y > 0 DO 

      Y := Y - 1;

      IF Y = 0 THEN 

		PRINT 0;

	  END;  /* END IF CLAUSE */ 

   END;  /* END WHILE */ 

ELSE

	X := 10;

	PRINT X; 

END;  /* END OUTER IF */")

(define test5
  "/* Invalid number format on line 11. Also on 12. */ 



_X-2345ASDadlkj := 10; 



/* Math check */ 

Y-z := _X-2345ASDadlkj + 2 - 3 * 6 * -1 + 13.0 - 0 * 0.0 + 3 * 6 / 3 * 12 / 27;

X := _X-2345ASDadlkj;

Y := Y-z;

IF X > 10 THEN   

   WHILE Y > 0  DO 

      Y := Y - 01;

      IF Y = 0. THEN 

		PRINT 0;

	  END;  /* END IF CLAUSE */ 

   END;  /* END WHILE */ 

ELSE

	X := 10;

	PRINT X; 

END;  /* END OUTER IF */")

(define test6
  "/* Give the nested-control logic a workout. This should parse. */ 



_X-2345ASDadlkj := 10; 



/* Math check */ 

Y-z := _X-2345ASDadlkj + 2 - 3 * ((6 * -1) + 13.0) - 0 * 0.0 + 3 * 6 / 3 * (12 / 27);

X := _X-2345ASDadlkj;

Y := Y-z;

IF X > 10 THEN  

   IF Y < 5 THEN  

	  WHILE Y > 0 DO 

		Y := Y - 01;

		IF Y = 0. THEN 

			PRINT 0;

		END;  /* END IF CLAUSE */ 

	  END;  /* END WHILE */ 

	END;  /* END OUTER IF */ 

ELSE

	X := -10;

	PRINT X;

END;  /* END OUTERMOST IF */")

(define test7
  "/* Syntax error at EOF -- missing END for outermost IF */ 



_X-2345ASDadlkj := 10; 



/* Math check */ 

Y-z := _X-2345ASDadlkj + 2 - 3 * 6 * -1 + 13.0 - 0 * 0.0 + 3 * 6/3 * 12/27;

X := _X-2345ASDadlkj;

Y := Y-z;

IF X > 10 THEN  

   IF Y < 5 THEN 

	  WHILE Y > 0 DO 

		Y := Y - 01;

		IF Y = 0. THEN 

			PRINT 0;

		END;  /* END IF CLAUSE */ 

	  END;  /* END WHILE */ 

	END;  /* END OUTER IF */ 

ELSE

	X := -10;

	PRINT X;")


(define test8
  "x := 1 + 2 * 3 - 4 / 2;")

(RDP test8)

(RDP test1)
(RDP test2)

;(RDP test3)      ;;shows lixacal error, '//' not valid symbol
;(RDP test4)       ;;shows lexical error
;(RDP test5)        ;; shows invalid number error
(RDP test6)         ;; should pass but I get error here, invalid number literal, since '01' is present but that is not a valid INT, so throws error for me, is this not expected??
(RDP test7)




;-----------TESTS-------------

;; Assume RDP is already defined:

;LLM provdied tests for me below (D)
