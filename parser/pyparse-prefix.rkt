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



;; Auxiliary definitions:


;; Supply auxiliary helpers here, like process-trailers:

(define (process-trailers base trailers)
 (match trailers
  ['()      base]
  [else     (error "can't handle trailers yet")]))


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
     ;       (newline))
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



(define (process-globals base variables)
  (display "Base-")
  (display base)
  (newline)
  (display "Varible-")
  (display variables)
  (newline)

  (match variables
   ['()
    base]

   [(cons (list comma var) rest)
    (begin
      (set! base (append base (list (string->symbol var))))
      (process-globals base  rest))]))

    ;(begin
     ;   (if (equal? first 1)
      ;    (begin 
       ;     (set! first 0)
        ;    (set! base (list (string->symbol base)))
         ;   (display "FIRST__")
         ;   (display base)
         ;   (newline))
         ; (void))

       ; (display "PRINTING")
       ; (display `(, base ,  (string->symbol var)))
       ; (newline)
        ;(process-globals `(, base ,var) rest) )]))
        ;(process-globals (set! base (append (list base)  (list (string->symbol var))))  rest))]))
    ;(process-globals '(, (string->symbol base), (string->symbol var)) rest)]))

;; You may want to put definitions here rather than defining
;; them in the grammar itself.



;; The parser:
(define pyparse
  (cfg-parser
   
   (tokens ID LIT PUNCT KEYWORD SPECIAL)
   
   (end EOF)
  
   ; ATTENTION: To support working "bottom-up" through the grammar,
   ; the start symbol is set to `power` instead of `file_input`.
   ; You should change the start symbol as you move up the kinds
   ; of expressions.
   (start dotted_name)
   
   (error (λ (tok-ok? tok-name tok-value)
            (if tok-ok?
                (error (format "Unexpected token: ~a ~a" tok-name tok-value))
                (error (format "Invalid token: " ~a)))))
   
   (grammar


