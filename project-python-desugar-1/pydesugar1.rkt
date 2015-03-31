        ; Anil Kumar Konasale krishna
        ;   akumarkk@cs.utah.edu 
        ;       UID - u0939372

        ; Sahana Sandeep
        ;   sahana@cs.utah.edu
        ;       UID - u0932424


#lang racket


(require pywalk)




;;; Normalize return
(define (canonicalize-return stmt)
  (match stmt
    [`(Return)  (list '(Return (NameConstant None)))]
    [else       (list stmt)]))


  

;;; Lift defaults
(define (lift-defaults stmt)

  ; <local definitions go here>
  (define def-list '())
  (define kwdef-list '())
  (define kwonlyargs-list '())
  ; A helper:
  (define (strip-defaults! arguments)
    (match arguments
      [`(Arguments
         (args . ,ids)
         (arg-types . ,arg-types)
         (vararg ,vararg . ,vararg-type)
         (kwonlyargs . ,kwonlyargs)
         (kwonlyarg-types . ,kwonlyarg-types)
         (kw_defaults . ,kw_defaults)
         (kwarg ,kwarg . ,kwarg-type)
         (defaults . ,defaults))
       ;=>
        (set! def-list defaults)
        (set! kwdef-list kw_defaults)
        (set! kwonlyargs-list kwonlyargs)
        (cond
        [(equal? kw_defaults '())
        `(Arguments
         (args . ,ids)
         (arg-types . ,arg-types)
         (vararg ,vararg . ,vararg-type)
         (kwonlyargs . ,kwonlyargs)
         (kwonlyarg-types . ,kwonlyarg-types)
         (kw_defaults . ,kw_defaults)
         (kwarg ,kwarg . ,kwarg-type)
         (defaults ,@(map (lambda (x) #f) defaults)))]
        [else `(Arguments
         (args . ,ids)
         (arg-types . ,arg-types)
         (vararg ,vararg . ,vararg-type)
         (kwonlyargs . ,kwonlyargs)
         (kwonlyarg-types . ,kwonlyarg-types)
         (kw_defaults #f)
         (kwarg ,kwarg . ,kwarg-type)
         (defaults ,@(map (lambda (x) #f) arg-types)))])]))


  (match stmt

    [`(FunctionDef
       (name ,id)
       (args ,args)
       (body . ,body)
       (decorator_list . ,decorators)
       (returns ,returns))

     ;(error "reconstitute the function def")]
     (list `(FunctionDef
       (name ,id)
       (args ,(strip-defaults! args))
       (body . ,body)
       (decorator_list . ,decorators)
       (returns ,returns))
            (cond
                [(equal? def-list (list #f))
                                `(Assign (targets (Attribute (Name ,id) __defaults__)) (value (NameConstant None)))]
                [`(Assign
                (targets (Attribute (Name ,id) __defaults__))
                (value ,`(Tuple ,@def-list)))])
            (cond
                [(equal? kwonlyargs-list '())
                                `(Assign (targets (Attribute (Name ,id) __kwdefaults__)) (value (NameConstant None)))]
                [`(Assign
                (targets (Attribute (Name ,id) __kwdefaults__))
                (value (Dict (keys (Str ,@(map symbol->string kwonlyargs-list))) (values ,@kwdef-list))))]))]
    [else (list stmt)]))


;;; Lift annotations
(define (lift-annotations stmt)

  ; <similar local definitions as lift-defaults>
  (define args-list '())
  (define argtype-list '())
  (define returns-list '())
  ; A helper:
  (define (strip-annot! arguments)
    (match arguments
      [`(Arguments
         (args . ,ids)
         (arg-types . ,arg-types)
         (vararg ,vararg . ,vararg-type)
         (kwonlyargs . ,kwonlyargs)
         (kwonlyarg-types . ,kwonlyarg-types)
         (kw_defaults . ,kw_defaults)
         (kwarg ,kwarg . ,kwarg-type)
         (defaults . ,defaults))
       ;=>
        (set! args-list ids)
        (set! argtype-list arg-types)
        `(Arguments
         (args . ,ids)
         (arg-types ,@(map (lambda (x) #f) arg-types))
         (vararg ,vararg . ,vararg-type)
         (kwonlyargs . ,kwonlyargs)
         (kwonlyarg-types . ,kwonlyarg-types)
         (kw_defaults . ,kw_defaults)
         (kwarg ,kwarg . ,kwarg-type)
         (defaults . ,defaults))]))

  (match stmt
    [`(FunctionDef
       (name ,id)
       (args ,args)
       (body . ,body)
       (decorator_list . ,decorators)
       (returns ,returns))

     ;(error "reconstitute the FunctionDef")]
        (set! returns-list returns)
     (list `(FunctionDef
       (name ,id)
       (args ,(strip-annot! args))
       (body . ,body)
       (decorator_list . ,decorators)
       (returns #f))
            (cond
                [(and (equal? argtype-list (list #f)) (equal? returns #f))
                `(Assign
                (targets (Attribute (Name ,id) __annotations__))
                (value (Dict (keys) (values))))]
                [(equal? argtype-list (list #f))
                `(Assign
               (targets (Attribute (Name ,id) __annotations__))
                (value (Dict (keys (Str "return")) (values ,(if (> 1 (length returns-list)) `(Tuple returns-list) returns-list)))))]
                [(equal? returns (list #f))
                `(Assign
                (targets (Attribute (Name ,id) __annotations__))
                (value (Dict (keys ,@(map (lambda (x)
                                                `(Str ,(symbol->string x))) (reverse args-list))) (values ,@(reverse argtype-list)))))]
                [`(Assign
                (targets (Attribute (Name ,id) __annotations__))
                (value (Dict (keys (Str "return") ,@(map (lambda (x)
                                                `(Str ,(symbol->string x))) (reverse args-list)))
                                (values ,(if (> 1 (length returns-list)) `(Tuple returns-list) returns-list) ,@(reverse argtype-list)))))]))]

    [else (list stmt)]))
       
 
    
    
(define name '())

;;; Lift decorators
(define (lift-decorators stmt)
  
  (define (apply-decorators id decs)
    (match decs
      ['()  '()]
      [(cons dec rest)
       (cons (assign `(Name ,id) (call dec (list `(Name ,id))))
             (apply-decorators id rest))]))
  
  (match stmt
    [`(FunctionDef 
       (name ,id)
       (args ,args)
       (body . ,body)
       (decorator_list . ,decorators)
       (returns ,returns))

      (begin
        (set! name id)
        (append (list `(FunctionDef  
             (name ,name)
             (args ,args)
             (body . ,body)
             (decorator_list  )
             (returns ,returns)))
                (apply-decorators id decorators))

        )]
     
     ;(error "finish me")]
     
     [`(ClassDef
        (name ,id)
        (bases . ,bases)
        (keywords . ,keywords)
        (starargs ,starargs)
        (kwargs ,kwargs)
        (body . ,body)
        (decorator_list . ,decorators))

       (begin
         (apply-decorators id decorators)
        (append (list `(ClassDef
             (name ,id)
             (bases . ,bases)
             (keywords . ,keywords)
             (starargs ,starargs)
             (kwargs ,kwargs)
             (body . ,body)
             (decorator_list . ,decorators)))
                 (apply-decorators id decorators))
      
      ;(error "finish me")]
        )]

    [else     (list stmt)]))
    
    
    


;;; Flatten assignments
;;;Auxiliary functions
(define ind -1)
(define (ind-set)
(set! ind (+ 1 ind)) ind)

(define (flatten-assign stmt)
  (match stmt
    [`(Assign (targets (Name ,id)) (value ,expr))
     (list stmt)]

    [`(Assign (targets (,(or 'Tuple 'List) . ,exprs)) (value ,expr))
      `(,
       `(Assign
                (targets (Name _tmp_3))
                (value (Call
                        (func (Name list))
                        (args ,expr)
                        (keywords)
                        (starargs #f)
                        (kwargs #f))))
         ,@(map (lambda (x)
                `(Assign (targets ,x) (value (Subscript (Name _tmp_3) (Index (Num ,(ind-set)))))))
                exprs))]

    [`(Assign (targets ,t1 ,t2 . ,ts) (value ,expr))
       `(,
       `(Assign
                (targets (Name _tmp_3))
                (value ,expr))
         ,@(map (lambda (x)
                `(Assign (targets ,x) (value (Name _tmp_3))))
                (if (equal? ts '()) (list t1 t2) `(,t1 ,t2 ,@ts))))]

    [else (list stmt)]))



(define tmp5 "_tmp_5")
(define tmp6 "_tmp_6")
(define test '())
(define target1 '())
(define body1 '())
(define body2 '())
(define handlers1 '())

;;; Convert for to while
(define (eliminate-for stmt)
  (match stmt
    
    ; (For (target <expr>) (iter <expr>) (body <stmt>*) (orelse <stmt>*))
    [`(For (target ,target)
           (iter ,iter)
           (body . ,body)
           (orelse . ,orelse))

      (begin
        (display "ORIGINAL BODY-")
        (display body)
        (newline)
        (display "Performign compare")
        (newline)
        (set! test (list `(Compare (left ,`(Name ,(string->symbol tmp5)))
                                   (ops . ,(list 'IsNot))
                                   (comparators , `(NameConstant False)))))
                                   ;(comparators ,(list `(NameConstant False))))))
                                   ;(list `(NameConstant False)) is not required here. It adds one more paranthesis.

        (display "Completed compare - ")
        (display test)
        (newline)
        (set! target1 `(Name ,(string->symbol tmp6)))
        (display target1)
        (newline)
        ;(set! body2 (list `(Assign (targets . ,(list `(Name ,(string->symbol tmp5))))
        (set! body2 (list `(Assign (targets  ,target)
                                   (value ,(call `(Attribute ,`(Name ,(string->symbol tmp5)) __next__)
                                                 '())))))
        (display "Completed body2 - ")
        (display body2)
        (newline)

        (set! handlers1 (list `(except
                                 (Name StopIteration)
                                 ,#f
                                 ,`(Assign (targets  ,`(Name ,(string->symbol tmp5))) 
                                                 (value (NameConstant False)))
                                 (Continue))))
        (display "Completed Handlers1- ")
        (display handlers1)
        (newline)

        (set! body1 (list `(Try (body . ,body2)
                                (handlers . ,handlers1)
                                (orelse)
                                (finalbody))))
        (display "Completed body1- ")
        (display body1)
        (newline)
        (set! body1 (append body1 body))

        (append (list `(Assign (targets . ,(list `(Name ,(string->symbol tmp5))))
                                (value ,`(GeneratorExp  ,`(Name ,(string->symbol tmp6)) 
                                                         ,`(for ,target1 in ,iter if )))))
                                ;(value ,`(GeneratorExp  ,`(Name ,(string->symbol tmp6)) (for ,`(Name ,(string->symbol tmp6)) in iter if)))))
                (list `(While 
                         (test .,test) 
                         (body . ,body1) 
                         (orelse . ,orelse))))
        ;(list `(For (target ,target) (iter ,iter) (body . ,body) (orelse . ,orelse)))
      )]
     
     ;(error "todo: build a while stmt")]
              
    
    [else    (list stmt)]))


;;; Insert locals


;;; Insert locals adds a (Local <var> ...) statement to the top
;;; of function and class bodies, so that you know which variables
;;; are assigned in that scope.
(define (insert-locals stmt)
  (match stmt
    
    [`(FunctionDef 
       (name ,id)
       (args ,args)
       (body . ,body)
       (decorator_list . ,decorators)
       (returns ,returns))
     
     (list
      `(FunctionDef 
       (name ,id)
       (args ,args)
       (body (Local ,@(set->list (locally-assigned body))) . ,body)
       (decorator_list . ,decorators)
       (returns ,returns)))]
    
    [`(ClassDef
       (name ,id)
       (bases . ,bases)
       (keywords . ,keywords)
       (starargs ,starargs)
       (kwargs ,kwargs)
       (body . ,body)
       (decorator_list . ,decorators))
     (append
      (list 
       `(ClassDef
         (name ,id)
         (bases ,@bases)
         (keywords ,@keywords)
         (starargs ,starargs)
         (kwargs ,kwargs)
         (body (Local ,@(set->list (locally-assigned body))) ,@body)
         (decorator_list . ,decorators))))]
       
    [else (list stmt)]))
     
      
        



;;; Eliminate classes

(define (eliminate-classes-expr expr . _)
  
  ; Store the fields in a dictionary:
  (define $fields '(Name __dict__))


  (match expr
    [`(Name ,var) 
     ; =>
     (if (and (set-member? (local-vars) var) (eq? (var-scope) 'class))
         `(Subscript ,$fields (Index (Str ,(symbol->string var))))
         expr)]
    
    [else expr]))


(define arguments1 '())
(define tmp9 "_tmp_9")
(define tmp10 "_tmp_10")
(define tmp11 "_tmp_11")
(define tmp12 "_tmp_12")
(define bodyc1 '())

(define (mycall fun args kargs)
    `(Call (func ,fun)
           (args . ,args)
           (keywords)
           (starargs #f)
           (kwargs ,kargs)))



(define (eliminate-classes-stmt stmt)
  
  (match stmt
    [`(ClassDef
       (name ,id)
       (bases . ,bases)
       (keywords . ,keywords)
       (starargs ,starargs)
       (kwargs ,kwargs)
       (body . ,body)
       (decorator_list . ,decorators))

      (begin
        ;(display id)
        ;(newline)
        ;(display "bases - ")
        ;(display bases)
        ;(newline)
        ;(display "Keywords - ")
        ;(display keywords)
        ;(newline)
        ;(display "starargs - ")
        ;(display starargs)
        ;(newline)
        ;(display "kwargs - ")
        ;(display kwargs)
        ;(newline)
        ;(display "body - ")
        ;(display body)
        ;(newline)
        ;(display "decorators - ")
        ;(display decorators)
        ;(newline)
        (set! arguments1 (list `(Arguments
                                    (args  ,(string->symbol tmp10))
                                    (arg-types  ,#f) 
                                    (vararg ,(string->symbol tmp11)) 
                                    (kwonlyargs  ,(string->symbol "metaclass"))
                                    (kwonlyarg-types  ,#f)
                                    (kw_defaults  (Name type))
                                    (kwarg ,(string->symbol tmp12))
                                    (defaults  (Name object)))))
        ;(display "Contructed arguments1 - ")
        ;(display arguments1)
        ;(newline)

        (set! bodyc1 (append
                       (list `(Assign (targets  (Name __dict__))
                                      (value    (Dict (keys) (values)))))
                       body
                       (list `(Return ,(mycall `(Name metaclass) (list 
                                                                 `(Str ,(symbol->string id))
                                                                 `(BinOp (Tuple (Name ,(string->symbol tmp10))) Add (Name ,(string->symbol tmp11)))
                                                                 `(Name __dict__))
                                                `(Name ,(string->symbol tmp12)))))))
        ;(display "Constructed bodyc1 - ")
        ;(display bodyc1)
        ;(newline)
        ;(display "Symbol - ")
        ;(display (symbol->string id))
        ;(newline)

        (append 
          (list 
           `(FunctionDef 
             (name ,(string->symbol tmp9))
             (args .,arguments1)
             (body . ,bodyc1)
             (decorator_list)
             (returns ,#f)))
          ;(list `(Assign (targets  (Name ,id)) (value ,(call `(Name ,(string->symbol tmp9)) '()))))
          ;`(Comment "scope: global")
          ;`(Pass)
          ;(list `(Assign (targets  (Name ,id)) (value ,(call `(Name ,id) '())))))
          (list `(Assign (targets  (Name ,id)) (value ,(call `(Name ,(string->symbol tmp9)) '())))))


        )]


     
     ;(error "complete me!")]
    
    [else  (list stmt)]))

       
      

(define prog (read))



(set! prog (walk-module prog #:transform-stmt insert-locals))

(set! prog (walk-module prog #:transform-stmt canonicalize-return))

;; Uncomment each of these as you finish them:

(set! prog (walk-module prog #:transform-stmt lift-decorators))

(set! prog (walk-module prog #:transform-stmt lift-defaults))

(set! prog (walk-module prog #:transform-stmt lift-annotations))

(set! prog (walk-module prog #:transform-stmt eliminate-for))

(set! prog (walk/fix prog #:transform-stmt flatten-assign))

;This is already implemented by Matt
(set! prog (walk-module prog #:transform-expr/bu eliminate-classes-expr))

(set! prog (walk-module prog #:transform-stmt eliminate-classes-stmt))


(write prog)


        ; Anil Kumar Konasale krishna
        ;   akumarkk@cs.utah.edu 
        ;       UID - u0939372

        ; Sahana Sandeep
        ;   sahana@cs.utah.edu
        ;       UID - u0932424
