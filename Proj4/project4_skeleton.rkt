#lang racket
(provide run-parser)
(provide run)

; --- syntax

; exp ::= id
;      |  num
;      |  "lambda" id exp0
;      |  "apply" exp1 exp2
;      |  "let" id exp1 exp0
;      |  "let" "_" exp1 exp0
;      |  "cond" exp0 exp1 exp2
;      |  op exp1 exp2
;      |  "letclass" id fields ":" exp0 
;      |  "new" id
;      |  "get" id exp1
;      |  "set" id exp1 exp2
;
; fields ::= id
;         |  id fields
;
;  op ::= "+"
;      |  "-"
;      |  "*"
;
; === lexical analysis

; --- tokens

(struct IdentT (string))
(struct NumT (num))
(struct LambdaT ())
(struct ApplyT ())
(struct LetT ())
(struct CondT ())
(struct LetClassT ())
(struct NewT ())
(struct GetT ())
(struct SetT ())
(struct PlusT ())
(struct MinusT ())
(struct TimesT ())
(struct ColonT ())
(struct UnderScoreT ())

(define (char->digit ch)
  (- (char->integer ch) (char->integer #\0)))

(define (lexer chars)
   (if (null? chars)
      null
      (let ([current (first chars)] [remain (rest chars)])
         (cond
           [(eq? current #\+) (cons (PlusT) (lexer remain))]
           [(eq? current #\-) (cons (MinusT) (lexer remain))]
           [(eq? current #\*) (cons (TimesT) (lexer remain))]
           [(eq? current #\:) (cons (ColonT) (lexer remain))]
           [(eq? current #\_) (cons (UnderScoreT) (lexer remain))]
           [(eq? current #\space) (lexer remain)]
           [(eq? current #\newline) (lexer remain)]	   	   
           [(char-numeric? current) (num-state (char->digit current) remain)]
           [(char-alphabetic? current) (ident-state (list current) remain)]
           [else (display (string-append "unknown symbol "
                              (list->string (list current)) "\n"))]
     ))))

(define (num-state seen chars)
   (if (and (pair? chars) (char-numeric? (first chars)))
      (num-state (+ (* 10 seen) (char->digit (first chars))) (rest chars))
      (cons (NumT seen) (lexer chars))
  ))

(define (ident-state seen chars)
   (if (and (pair? chars) 
          (or (char-alphabetic? (first chars))
              (char-numeric? (first chars))))
      (ident-state (append seen (list (first chars))) (rest chars))
      (cons (mk-alpha-token (list->string seen)) (lexer chars))
  ))

(define (mk-alpha-token seen)
   (cond
      [(equal? seen "lambda") (LambdaT)]
      [(equal? seen "apply") (ApplyT)]
      [(equal? seen "let") (LetT)]      
      [(equal? seen "cond") (CondT)]    
      [(equal? seen "letclass") (LetClassT)]        
      [(equal? seen "new") (NewT)]      
      [(equal? seen "get") (GetT)]      
      [(equal? seen "set") (SetT)]      
      [else (IdentT seen)]
  ))

(define (run-lexer x) (lexer (string->list x)))

; === parsing

; --- syntax trees

(struct IdentExp (id))
(struct NumExp (num))
(struct LambdaExp (formal body))
(struct ApplyExp (fun arg))
(struct LetExp (id exp1 body))
(struct SeqExp (exp1 exp2))
(struct CondExp (test exp1 exp2))
(struct PlusExp (exp1 exp2))
(struct MinusExp (exp1 exp2))
(struct TimesExp (exp1 exp2))
(struct LetClassExp (id fields body))
(struct NewExp (id))
(struct ReadExp (obj field))
(struct WriteExp (obj field new))

(define (parseExpectIdent error-msg tks)
   (if (and (pair? tks) (IdentT? (first tks)))
      (values (IdentT-string (first tks)) (rest tks))
      (display error-msg)
   ))

(define (parseExpect what? error-msg tks)
   (if (and (pair? tks) (what? (first tks)))
      (values "dummy" (rest tks))
      (display error-msg)
   ))

(define (parseFields tks)
   (let*-values (
      [(field1 tks1) (parseExpectIdent 
                        "until ':' there must be one or more field names"
                        tks)])
        (if (and (pair? tks1) (ColonT? (first tks1)))
           (values (list field1) (rest tks1))
           (let*-values (
              [(fields tks2) (parseFields tks1)])
             (values (cons field1 fields) tks2)))
 ))

(define (parseExp tks)
   (if (pair? tks)
      (let ([tk (first tks)] [tks0 (rest tks)])
         (cond
            [(IdentT? tk)
               (values (IdentExp (IdentT-string tk)) tks0)]
            [(NumT? tk)
               (values (NumExp (NumT-num tk)) tks0)]
            [(LambdaT? tk)
               (let*-values (
                  [(id tks1) (parseExpectIdent
                                "identifier expected after 'lambda'\n" tks0)]
                  [(e tks2) (parseExp tks1)])
                 (values (LambdaExp id e) tks2))]
            [(ApplyT? tk)
               (let*-values (
                  [(e1 tks1) (parseExp tks0)]
                  [(e2 tks2) (parseExp tks1)])
                 (values (ApplyExp e1 e2) tks2))]
            [(LetT? tk)
               (if (and (pair? tks0) (UnderScoreT? (first tks0)))
                   (let*-values (
                      [(e1 tks1) (parseExp (rest tks0))]
                      [(e2 tks2) (parseExp tks1)])
                     (values (SeqExp e1 e2) tks2))
                   (let*-values (
                      [(id tks1) (parseExpectIdent
                                    "identifier expected after 'let'\n" tks0)]
                      [(e1 tks2) (parseExp tks1)]
                      [(e0 tks3) (parseExp tks2)])
                     (values (LetExp id e1 e0) tks3)))]
            [(CondT? tk)
               (let*-values (
                  [(e0 tks1) (parseExp tks0)]
                  [(e1 tks2) (parseExp tks1)]
                  [(e2 tks3) (parseExp tks2)])
                 (values (CondExp e0 e1 e2) tks3))]
            [(PlusT? tk)
               (let*-values (
                  [(e1 tks1) (parseExp tks0)]
                  [(e2 tks2) (parseExp tks1)])
                 (values (PlusExp e1 e2) tks2))]
            [(MinusT? tk)
               (let*-values (
                  [(e1 tks1) (parseExp tks0)]
                  [(e2 tks2) (parseExp tks1)])
                 (values (MinusExp e1 e2) tks2))]
            [(TimesT? tk)
               (let*-values (
                  [(e1 tks1) (parseExp tks0)]
                  [(e2 tks2) (parseExp tks1)])
                 (values (TimesExp e1 e2) tks2))]
            [(LetClassT? tk)
               (let*-values (
                  [(id tks1) (parseExpectIdent
                                "identifier expected after 'letclass'\n" tks0)]
                  [(fields tks2) (parseFields tks1)]
                  [(e tks3) (parseExp tks2)])
                 (values (LetClassExp id fields e) tks3))]
            [(NewT? tk)
               (let*-values (
                  [(id tks1) (parseExpectIdent
                                "class name expected after 'new'\n" tks0)])
                 (values (NewExp id) tks1))]
            [(GetT? tk)
               (let*-values (
                  [(field tks1) (parseExpectIdent
                                "field name expected after 'get'\n" tks0)]
                  [(e1 tks2) (parseExp tks1)])
                 (values (ReadExp e1 field) tks2))]
            [(SetT? tk)
               (let*-values (
                  [(field tks1) (parseExpectIdent
                                "field name expected after 'set'\n" tks0)]
                  [(e1 tks2) (parseExp tks1)]
                  [(e2 tks3) (parseExp tks2)])
                 (values (WriteExp e1 field e2) tks3))]
            [else (display "not proper start of expression\n")]
        ))
      (display "expression expected\n")
   ))
   
(define (parse tks)
   (let-values ([(main tks1) (parseExp tks)])
      (if (null? tks1)
         main
         (display "program read but more input given\n"))
    ))

(define (run-parser x) (parse (run-lexer x)))

; === auxiliary concepts

; --- dictionaries
;  are represented as a list of pairs

(define empty-dict '())

(define (extend-dict id new dict)
  (cons (cons id new) dict)
 )

(define (lookup-dict error-msg id dict)
  (cond
     [(null? dict)
        (display (string-append error-msg "\n"))]
     [(equal? id (car (first dict)) )
        (cdr (first dict))]
     [else (lookup-dict error-msg id (rest dict))]
  ))

; an environment 'env' associates identifiers with locations

; a class environment 'cenv' associates identifiers with lists of field names

; a store 'sto' associates locations with values

; --- Locations
;   represented as integers

(define locs (box 0))

(define (new-loc!)
   (let ([_ (set-box! locs (+ 1 (unbox locs)))])
       (unbox locs)))

; --- values

(struct NumV (num))
(struct ClosureV (formal body env cenv))
(struct ObjectV (env))  

; === evaluating (abstract) syntax

(define (interp exp env cenv sto)
   (cond
      [(IdentExp? exp)
          (let*
             ([id (IdentExp-id exp)]
	      [loc (lookup-dict 
                      (string-append "undeclared identifier " id) 
                      id env)]
              [val (lookup-dict
                      (string-append "uninitialized location for " id)
                       loc sto)])
            (values val sto))]
      [(NumExp? exp) (values (NumV (NumExp-num exp)) sto)]
      [(LambdaExp? exp)
           (values (NumV 101)   ;;; CHANGE #1
                    sto)]
      [(ApplyExp? exp)
          (let*-values 
               ([(v1 sto1) (interp (ApplyExp-fun exp) env cenv sto)]
                [(v2 sto2) (interp (ApplyExp-arg exp) env cenv sto1)])
            (if (ClosureV? v1)
               (let ([loc (new-loc!)])
                   (interp 
                      (NumExp 102) env cenv sto  ;;; CHANGE #2
                    ))
               (display "non-function applied\n")))]
      [(LetExp? exp)
          (interp 
             (ApplyExp 
                (LambdaExp (LetExp-id exp) (LetExp-body exp)) 
                (LetExp-exp1 exp))
             env cenv sto)]
      [(SeqExp? exp)
           (interp (SeqExp-exp2 exp) env cenv sto)] ;;; CHANGE #3
      [(CondExp? exp)
          (let-values 
               ([(v0 sto0) (interp (CondExp-test exp) env cenv sto)])
           (if (and (NumV? v0) (> (NumV-num v0) 0))
             (interp (CondExp-exp1 exp) env cenv sto0)
             (interp (CondExp-exp2 exp) env cenv sto0)))]
      [(PlusExp? exp)
          (let*-values 
               ([(v1 sto1) (interp (PlusExp-exp1 exp) env cenv sto)]
                [(v2 sto2) (interp (PlusExp-exp2 exp) env cenv sto1)])
           (if (and (NumV? v1) (NumV? v2))
              (values (NumV (+ (NumV-num v1) (NumV-num v2))) sto2)
              (display "operands to '+' must be numbers\n")))]
      [(MinusExp? exp)
          (let*-values 
               ([(v1 sto1) (interp (MinusExp-exp1 exp) env cenv sto)]
                [(v2 sto2) (interp (MinusExp-exp2 exp) env cenv sto1)])
           (if (and (NumV? v1) (NumV? v2))
              (values (NumV (- (NumV-num v1) (NumV-num v2))) sto2)
              (display "operands to '-' must be numbers\n")))]
      [(TimesExp? exp)
          (let*-values 
               ([(v1 sto1) (interp (TimesExp-exp1 exp) env cenv sto)]
                [(v2 sto2) (interp (TimesExp-exp2 exp) env cenv sto1)])
           (if (and (NumV? v1) (NumV? v2))
              (values (NumV (* (NumV-num v1) (NumV-num v2))) sto2)
              (display "operands to '*' must be numbers\n")))]
      [(LetClassExp? exp)
          (interp
             (LetClassExp-body exp)
             env
             cenv ;;; CHANGE #4
             sto)]
      [(NewExp? exp) 
          (let* ([id (NewExp-id exp)]
                 [fields (lookup-dict
                            (string-append "undeclared class " id)
                            id cenv)]
                 [env0 empty-dict]  ;;; CHANGE #5
             )
          (values (ObjectV env0) sto))]
      [(ReadExp? exp)
          (let-values 
               ([(v0 sto0) (interp (ReadExp-obj exp) env cenv sto)])
             (if (ObjectV? v0)
                (let* ([field (ReadExp-field exp)]
                       [loc (lookup-dict 
                               (string-append "undeclared field " field) 
                               field (ObjectV-env v0))])
                      (values (NumV 106) ;;; CHANGE #6 
                               sto0))  
                (display "field read from non-object\n")
             ))]
      [(WriteExp? exp)
          (let*-values 
               ([(v0 sto0) (interp (WriteExp-obj exp) env cenv sto)]
                [(v2 sto1) (interp (WriteExp-new exp) env cenv sto0)])
             (if (ObjectV? v0)
                (let* ([field (WriteExp-field exp)])
                    (values v2 
                        sto1  ;;; CHANGE #7
                    ))
                (display "field written to non-object\n")
             ))]
  ))


(define (run x)
   (let ([main (run-parser x)])
     (let-values ([(v sto) (interp main empty-dict empty-dict empty-dict)])
       (cond
         [(NumV? v) (NumV-num v)]
         [else (display "program must return a number\n")]
    ))))

