        ; Anil Kumar Konasale krishna
        ;   akumarkk@cs.utah.edu 
        ;       UID - u0939372

        ; Sahana Sandeep
        ;   sahana@cs.utah.edu
        ;       UID - u0932424


#lang racket

(require parser-tools/cfg-parser)

(require parser-tools/lex)


;; Helpers:
(define (unzip/callback lst k)
  (match lst
    ['() (k '() '())]
    [(cons (list a b) tl)
     (unzip/callback tl (λ (as bs)
       (k (cons a as) (cons b bs))))]))



;; Lexer definitions:
(define-tokens ID (NAME))

(define-tokens LIT (STRING NUMBER))

(define-empty-tokens PUNCT (+ - * ** / // % << >> & \| ^ ~ < > <= >= == !=
                      <> ... ; had to add these due to bug/omission in lexer spec
                      |(| |)| |[| |]| |{| |}| |,| : |.| |;| @ = -> += -= *= /= //= %= &= \|= ^= >>= <<= **=))


(define-empty-tokens KEYWORD (False class finally is return None continue for lambda try
                        True def from nonlocal while and del global not with as elif if 
                        or yield assert else import pass break except in raise))

(define-empty-tokens SPECIAL (ENDMARKER NEWLINE INDENT DEDENT LIT EOF))

(define (process-if-ladder elif-ladder else-block)
  ;(display elif-ladder)
  ;(newline)
  (match elif-ladder
    [(cons (list elif test-cond colon if-body)  rest)
     `(If , `(test , test-cond), `(body ,@if-body), `(orelse ,@(process-if-ladder rest else-block)))]

    [else
      (if else-block
        else-block
        `())]))

;; Auxiliary definitions:


;; Supply auxiliary helpers here, like process-trailers:

(define (process-trailers base trailers)
 (match trailers
  ['()      base]
  [(cons (cons "[" a) rest) `(Subscript, base,a)]
  [_ (cons base trailers)]))

;; You may want to put definitions here rather than defining
;; them in the grammar itself.

(define (process-binops base ops)
(match ops
['()
base]
[(cons (list op exp) rest)
(process-binops `(BinOp, base ,
		 (cond [(equal? op "+") `Add] 
		       [(equal? op "-") `Sub]
		       [(equal? op "*") `Mult]
		       [(equal? op "/") `Div]
		       [(equal? op "%") `Mod]
		       [(equal? op "//") `FloorDiv]
		       [(equal? op "<<") `LShift]
		       [(equal? op ">>") `RShift]
		       [(equal? op "&") `BitAnd]
		       [(equal? op "^") `BitXor]
		       [(equal? op "|") `BitOr]), 
		  exp) rest)]))

(define (process-augassign base ops)
(begin
(match ops
['()
base]
[(cons op exp)
 (begin
      (if (equal? (car op) "=") (process-assign (list base) ops)
                 `(AugAssign, base ,
                 (cond [(equal? op "+=") `Add]
                       [(equal? op "-=") `Sub]
                       [(equal? op "*=") `Mult]
                       [(equal? op "/=") `Div]
                       [(equal? op "%=") `Mod]
                       [(equal? op "//=") `FloorDiv]
                       [(equal? op "<<=") `LShift]
                       [(equal? op ">>=") `RShift]
                       [(equal? op "&=") `BitAnd]
                       [(equal? op "^=") `BitXor]
                       [(equal? op "|=") `BitOr]),
                  (car exp)))) ]
)))

(define (process-unops base ops)
(match ops
['()
  base] 
[_ (process-unops `(UnaryOp,
(match base
["+" `UAdd]
["-" `USub]
["~" `Invert]
["not" `Not]), ops) '())]))


(define (process-level base)

  (if (equal? base null)
    0
  (match (car base)
            ["." (+ (process-level (cdr base)) 1)]

            ["..." (+ (process-level (cdr base)) 3)])))



(define (process-dotted-names base variables)
  ;(display "Base-")
  ;(display base)
  ;(newline)
  ;(display "Varible-")
  ;(display variables)
  ;(newline)

  (match variables
   ['()
    base]

   [(cons (list comma var) rest)
    (begin
      (set! base (append base (list  var)))
      (process-dotted-names base  rest))]))

(define main_list '())

(define (process-arguments-only argument_list)
  (newline)
  (display "Argument_only_list is :")
  (display argument_list)
  (newline)
  (display "Initial MAIN_LIST - ")
  (display main_list)
  (newline)

  (match argument_list
         ['()
          (begin
            (display "Base case - ")
            (display (cons 'args main_list))
            ;(display `(args ,@main_list))
            (newline)
            (cons 'args main_list))]
           ;`(args ,main_list))]

         ;[ (list (list 'args var) rest)
         [(cons (list args var) rest)
         ;[ (list (cons (list 'args var) rest))
          (begin
            (display "This time matched cons of list")
            (newline)
            (display args)
            (newline)
            (display var)
            (newline)
            (display "REST-")
            (display rest)
            (newline)
            (set! main_list (append main_list (list var)))
            ;(set! main_list (cons main_list var))
            (display "MAIN_LIST NOW - ")
            (display main_list)
            (newline)
            (process-arguments-only rest))]
         
         [else
           (begin
             (display "Not matched any cases...")
             (newline)
             (if (empty? main_list)
                argument_list
              main_list))
           ]))



(define type_arg_base '())
(define type_arg-type_base '())
(define type_defaults_base '())

(define type_kwonlyargs_base '())
(define type_kwonlyargs-type_base '())
(define type_kwdefaults_base '())

(define type_varargs_base '())
(define type_kwargs_base '())
(define type_keywords_base '())
(define type_final_list '())

(define (reset-all-typed-variables flag)
  (if (= flag 1)
    (begin
        (set! type_arg_base '())
        (set! type_arg-type_base '())
        (set! type_defaults_base '())
        (set! type_kwonlyargs_base '())
        (set! type_kwonlyargs-type_base '())
        (set! type_kwdefaults_base '())
        (set! type_varargs_base '())
        (set! type_kwargs_base '())
        (set! type_keywords_base '()))
    (void)))

(define (process-typedargs typedargs)
  ;(display "----------------------- START TYPEARGS ------------------------")
  (newline)
  (display "TYPEDARG_LIST - ")
  (display typedargs)
  (newline)

  (match typedargs
         ['()
          (begin
            (display "TYPEDARGS Base case")
            (newline)
            (set! type_final_list
              (list
                (append (list 'args) type_arg_base)
                (cons  'arg-types type_arg-type_base)

                (append (list 'vararg) (if (empty? type_varargs_base)
                                            (list #f)
                                            type_varargs_base))

                (append (list 'kwonlyargs) type_kwonlyargs_base)
                (append (list 'kwonlyarg-types) type_kwonlyargs-type_base)
                (append (list 'kw_defaults) type_kwdefaults_base)

                ;We want (vararg t1 (Name int)) NOT (vararg (t1 (Name int)))
                ;so list ---> append
                ;(append (list 'kwarg) type_kwargs_base)
                (append (list 'kwarg) (if (empty? type_kwargs_base)
                                          (list #f)
                                       type_kwargs_base))

                (append (list 'defaults) type_defaults_base)))
            (display "TYPEDARGS FINAL LIST - ")
            (display type_final_list)
            (newline)
            (reset-all-typed-variables 1)
            `(Arguments ,@type_final_list))]
            ;type_final_list)]

         [(cons (list 'args var) rest)
          (begin

            (display "********* Matched type-arg base *************")
            (newline)
            (match var
                   [(cons (cons variable var-type) def-value)
                    (begin
                      (display "Complete argument")
                      (newline)
                      (display "variable = ")   (display variable) (newline)
                      (display "var-type = ")   (display var-type) (newline)
                      (display "default = ")    (display def-value) (newline)
                      (set! type_arg_base (append type_arg_base (list variable)))
                      (set! type_defaults_base (append type_defaults_base (list def-value)))
                      (set! type_arg-type_base (append type_arg-type_base var-type)))]

                   ; This case is matched when there is no DEFAULT VALUE for arguments
                   [(cons variable var-type)
                    (begin
                      (display "Variable - ")
                      (display variable)
                      (newline)
                      (display "var-type - ")
                      (display var-type)
                      (newline)
                      (set! type_arg_base (append type_arg_base (list variable)))
                      (set! type_arg-type_base (append type_arg-type_base var-type))
                      (set! type_defaults_base (append type_defaults_base (list #f))))]

                   [else 
                     (begin
                       (display "No default-value-")
                       (display var)
                       (newline)
                       (set! type_arg_base (append type_arg_base (list var))))])

            (display "REST : ") (display rest) (newline)
            ;(if (empty? rest)
              ;(process-typedargs (car rest))
            (process-typedargs rest))]
         
        [(cons (list 'kwonlyargs var) rest)
          (begin

            (display "********* Matched KWONLY-args base *************")
            (newline)
            (match var
                   [(cons (cons variable var-type) def-value)
                    (begin
                      (display "Complete argument")
                      (newline)
                      (display "variable = ")   (display variable) (newline)
                      (display "var-type = ")   (display var-type) (newline)
                      (display "default = ")    (display def-value) (newline)
                      (set! type_kwonlyargs_base (append type_kwonlyargs_base (list variable)))
                      (set! type_kwdefaults_base (append type_kwdefaults_base  (list def-value)))
                      (set! type_kwonlyargs-type_base (append type_kwonlyargs-type_base var-type)))]

                   ; This case is matched when there is no DEFAULT VALUE for arguments
                   [(cons variable var-type)
                    (begin
                      (display "Variable - ")
                      (display variable)
                      (newline)
                      (display "var-type - ")
                      (display var-type)
                      (newline)
                      (set! type_kwonlyargs_base (append type_kwonlyargs_base (list variable)))
                      (set! type_kwonlyargs-type_base (append type_kwonlyargs-type_base var-type))
                      (set! type_kwdefaults_base (append type_kwdefaults_base (list #f))))]

                   [else
                     (begin
                       (display "No default-value-")
                       (display var)
                       (newline)
                       (set! type_kwonlyargs_base (append type_kwonlyargs_base (list var))))])

            (display "REST : ") (display rest) (newline)
            ;(if (empty? rest)
              ;(process-typedargs (car rest))
            (process-typedargs rest))]


        [(cons (list 'vararg  var) rest)
          (begin
            (display "-----------------  Matched VAR-args base ---------------------")
            (newline) 
            (display "VAR : ")
            (display var)
            (newline)
            (set! type_varargs_base (append type_varargs_base (car var)))
            ;(display "DONE - ")
            ;(display type_varargs_base)
            ;(newline)
            ;(display "CDR - ")
            ;(display (cdr var))
            ;(newline)

            (if (not (equal? (cdr var) (list #f)))
              (set! type_varargs_base (append (list type_varargs_base) (cdr var)))
              (set! type_varargs_base (list type_varargs_base)))

            (process-typedargs rest))]

        [(cons (list 'kwarg  var) rest)
          (begin
            (display "-----------------  Matched KW-args base ---------------------")
            (newline)
            (set! type_kwargs_base (append type_kwargs_base (car var)))
            (display "VALUE - ")
            (display type_kwargs_base)
            (newline)
            (display "VAR - ")
            (display var)
            (newline)
            (display "RES - ")
            (display rest)
            (newline)

            (if (not (equal? (cdr var) (list #f)))
              (begin
                (set! type_kwargs_base (append (list type_kwargs_base) (cdr var)))
                ;(display "TYPE present - ")
                ;(display type_kwargs_base)
                ;(newline)
                )
              (begin
                ;(display "TYPE NOT Present - ")
                ;(display type_kwargs_base)
                ;(newline)
                (set! type_kwargs_base (list type_kwargs_base))
                (void)))


            (process-typedargs rest))]

         ))

(define (get_empty_arglist)
    (process-typedargs '()))
              

(define arg_base '())
(define starargs_base '())
(define kwargs_base '())
(define keywords_base '())
(define final_list '())

(define do-reset 1)

; TODO - May be there could be more elegant way to do this.
; But at this point, this seems to be better to my tired mind :)
(define (reset-all-variables flag)
  (if (= flag 1)
    (begin
      (set! arg_base '())
      (set! starargs_base '())
      (set! kwargs_base '())
      (set! keywords_base '()))
    (void)))

; As all the variables used here are global, expression that matches last 
; case is expected have only one expression not many subexpression.
;
; Example :
;   For this case
;   ((((starargs (Name arg_star)) (args (Name var_1))) (kwargs (Name kw_args))))
;   
;   NAME = ((starargs (Name arg_star)) (args (Name var_1)))
;   VAR  = (kwargs (Name kw_args))
;   REST = ()
;
;   This again invokes the function recursively with :
;   (((starargs (Name arg_star)) (args (Name var_1))))
;
;   NAME = (starargs (Name arg_star))
;   VAR  = (args (Name var_1))
;   REST =  ()
;
;   *** This results in resetting all the global variables ***

(define (process-all-args arglist)
  (display "===========   START process-all-arglist =============")
  (newline)
  (display "ARGLIST - ")
  (display arglist)
  (newline)
  (display "args_base - ")
  (display arg_base)
  (newline)

  (display "starargs_base - ")
  (display starargs_base)
  (newline)

  (display "kwargs_base - ")
  (display kwargs_base)
  (newline)

  (display "keywords_base - ")
  (display keywords_base)
  (newline)
  (newline)

  (match arglist
         ['()
          (begin
            (set! final_list 
                (list
                    (append (list 'args) arg_base)
                    ;(cons (list 'args) arg_base)
                    (append (list 'keywords) keywords_base)

                    ;(append (list 'kwargs kwargs_base))))

                    (list 'starargs (if (empty? starargs_base)
                                      #f
                                      starargs_base))
                    (list 'kwargs   (if (empty? kwargs_base)
                                      #f
                                      kwargs_base))))
            ; Reset everything
            (if (= do-reset 1)
              (begin
                (set! arg_base '())
                (set! starargs_base '())
                (set! kwargs_base '())
                (set! keywords_base '()))
              (void))

            (display "FINAL_LIST - ")
            (display final_list)
            (newline)
            (display "__________________  END process-all-arglist ________________")
            (newline)

            final_list)]

         [(cons (list 'args var) rest)
          (begin

            (display "********* Matched arg base *************")
            (newline)
            (set! arg_base (append arg_base (list var)))
            (process-all-args rest))]

         [(cons (list 'starargs var) rest)
          (begin
            (display "_________ STARARGS ________")
            (display var)
            (newline)
            ;(set! starargs_base (append starargs_base (list var)))
            (set! starargs_base  var)
            (process-all-args rest))]

         [(cons (list 'kwargs var) rest)
          (begin
            (display "//////// KW ARGS ////////")
            (display var)
            (newline)
            ;(set! kwargs_base (append kwargs_base (list var)))
            (set! kwargs_base var)
            (process-all-args rest))]

         [(cons (list 'keywords var) rest)
          (begin
            (display "........  KEYWORDS  .......")
            (display var)
            (newline)
            (set! keywords_base (append keywords_base (list var)))
            (process-all-args rest))]
       
         ; This rule should be matched only once per list.
         ; Later it should resolve the call to other lists
         ;
         [(cons (list name var) rest)
          (begin
            (display "^^^^^^^ Matched cons of list ^^^^^^^^^^^")
            (newline)
            (display "NAME ^")
            (display name)
            (newline)
            (display "VAR ^ ")
            (display var)
            (newline)
            (display "REST ^ ")
            (display rest)
            (newline)
            (set! do-reset 0)

            (match (list name)
                   [(cons (list sub-name sub-var) sub-rest)
                    (begin
                      (display "Matched sub-expression")
                      (newline)
                      (if (or (eq? sub-name 'args)
                              (eq? sub-name 'starargs)
                              (eq? sub-name 'keywords)
                              (eq? sub-name 'kwargs))
                        (process-all-args (list name))
                        (begin
                          (process-all-args (list sub-name))
                          (process-all-args (list sub-var)))))]


                   [else 
                     (process-all-args (list name))])


            ;(process-all-args (list name))
            (process-all-args (list var))
            (process-all-args rest)
            (set! do-reset 1)
            (reset-all-variables 1)
            final_list
          )]))



(define (process-globals base variables)
 ; (display "Base-")
  ;(display base)
 ; (newline)
 ; (display "Varible-")
 ; (display variables)
 ; (newline)

  (match variables
   ['()
    base]

   [(cons (list comma var) rest)
    (begin
      (set! base (append base (list (string->symbol var))))
      (process-globals base  rest))]))



(define (process-dots base variable)
  (match variable
  ['()
   (begin
     (set! base (string->symbol base))
      base)]

  [(cons (list dot var) rest)
   (begin
    (set! base (string-append base dot))
    (set! base (string-append base var))
    (process-dots base rest))]))
    ;(process-dots `,base,dot,var rest))]))



(define (process-as arg variable)
  ;(display "Debugging- ")
  ;(display arg)
  ;(newline)
  ;(display variable)
  ;(newline)
  (match variable
         ['()
          (arg)]

         [(cons as var)
            `(, arg ,var)]))



(define attr (list 'Attribute))
(define first 1)

(define (process-dotted base variables)
  ;(display "Base-")
  ;(display base)
  ;(newline)
  ;(display "Varible-")
  ;(display variables)
  ;(newline)

  (match variables
   ['()
    (begin
      (set! first 1)
        base)]

   [(cons (list comma var) rest)
    (begin
      (if (equal? first 1)
        (begin
            (set! base (append (list 'Name) base))
            (set! first 0)
   ;         (display "Base is now - ");
    ;        (display base)
     ;       (newline)
            )
        (void))

      (set! base (reverse (cons base attr)))
     ; (display "debug base")
     ; (display base)
     ; (newline)
      (set! base (append base (list (string->symbol var))))
     ; (display "debug base1")
     ; (display base)
     ; (newline) 
      (process-dotted base  rest))]))

(define (process-testlist arg1 arg2)
(match arg2
['()
arg1]
[_
        (begin
        (set! arg1 (append  arg1 (list (car arg2))))
        (process-testlist arg1 (rest arg2)))]))

(define tar '())
(define val '())

(define (process-assign base ops)
(begin

(match ops
['()
(begin
    (set! base `(Assign, `(targets ,@base), `(value, val)))
 base)]
[(cons (list op exp) rest)
                   (begin
                   (cond [(not (empty? rest)) (set! base (append base (list exp)))] 		   	 [(set! val exp)])
                   ;(set! tar (append tar val))
                   (process-assign base rest))])))

(define opers '(ops))
(define comps '(comparators))
;(define first 0)
(define (process-compops base ops)
;(begin
 ; (cond [(equal? first 0) ((set! base `(Compare, `(left, base), (process-compops '() ops)))
  ;(set! first 1))
  ;])
(match ops
['()
(begin
    (set! base (append `(,opers) `(,comps)))
    base)]

[(cons (list op exp) rest)
                   (begin
		   ;(display rest)
		   (set! opers (append opers (list op)))
		   (set! comps (append comps `(,exp)))
		   (process-compops '() rest))]))

;; The parser:
(define pyparse
  (cfg-parser
   
   (tokens ID LIT PUNCT KEYWORD SPECIAL)
   
   (end EOF)
  
   ; ATTENTION: To support working "bottom-up" through the grammar,
   ; the start symbol is set to `power` instead of `file_input`.
   ; You should change the start symbol as you move up the kinds
   ; of expressions.
   (start file_input)
   ;(start decorators)
   ;(start compound_stmt)

   (error (λ (tok-ok? tok-name tok-value)
            (if tok-ok?
                (error (format "Unexpected token: ~a ~a" tok-name tok-value))
                (error (format "Invalid token: " ~a)))))
   
   (grammar


