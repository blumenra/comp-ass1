(load "pc.scm")

(define <endOfLine>
    (new
        (*parser (char #\newline))
        done))
     
(define <endOfFile> <end-of-input>)

(define <end-of-line-comment-char>
  (new
    (*parser <endOfLine>)
    (*parser <endOfFile>)
    (*disj 2)
    done))
        
(define <whitespace> (range #\nul #\space))

(define <line-comment>
  (new 
    (*parser (char #\;))
    (*parser <any-char>)
    (*parser <end-of-line-comment-char>)
    *diff
    *star
    (*caten 2)
    (*pack-with
      (lambda (semi str)
        "What to return here?"))
    
    done))

(define <exp-comment>
  (new 
        (*parser (char #\;))




        ;IMPLEMENT ME





    done))

(define <Comments>
    (new 
        (*parser <line-comment>)
        (*parser <exp-comment>)
        (*disj 2)

    done))
        
(define <Ignore>
    (new
        (*parser <whitespace>)
        (*parser <Comments>)
        (*disj 2)
        done))
        
        
(define <Boolean>
    (new
    	(*parser (word-ci "#t"))
    	(*parser (word-ci "#f"))
    	(*disj 2)
    	(*pack (lambda (list)
    		(let ((l (char-downcase (cadr list))))
 				(cond
 					((char=? l #\t) #t)
 					((char=? l #\f) #f)
 					(else (display "The value is no valid!"))))))

    	done))


(define <CharPrefix>
    (new
    	(*parser (word "#\\"))
    	(*pack 
    		(lambda (_)
    			_))
    	done))


(define <VisibleSimpleChar> (range #\! #\delete))

(define <digit-0-9> (range #\0 #\9))

(define <letter-a-f> (range-ci #\a #\f))

(define <letter-a-z> (range-ci #\a #\z))

(define <HexChar>
  (new
   (*parser <digit-0-9>)
   (*parser <letter-a-f>)
   (*disj 2)
   done))
   
(define <HexUnicodeChar>
    (new 
    (*parser (char-ci #\x))
    (*parser <HexChar>)
    *plus
    (*caten 2)
    done))
    
(define ^<NamedChar>
    (lambda (str ch)
    (new (*parser (word-ci str)) 
        (*pack (lambda (_) ch))
    done)))
    
(define <NamedChar>
    (new
       (*parser (^<NamedChar> "space" #\space))
       (*parser (^<NamedChar> "nul" #\nul))
       (*parser (^<NamedChar> "newline" #\newline))
       (*parser (^<NamedChar> "return" #\return))
       (*parser (^<NamedChar> "tab" #\tab))
       (*parser (^<NamedChar> "page" #\page)) 
       (*parser (^<NamedChar> "lambda" (integer->char 955)))
       (*disj 7)
       done))


(define <Char>
    (new
        (*parser <CharPrefix>)
        (*parser <NamedChar>)
        (*parser <VisibleSimpleChar>)
        (*parser <HexUnicodeChar>)
        (*disj 3)
        (*caten 2)
        (*pack (lambda (list)
            (cadr list)))
        
        
        done))
                  
    
(define <Natural>
    (new
        (*parser <digit-0-9>)
        *plus
        
         (*pack
            (lambda (numList)
                (string->number (list->string numList))))
        done))
        
        
        
        
        
(define <op>
    (new

        (*parser (char #\+))
        (*parser (char #\-))

        (*disj 2)

        (*pack (lambda (op-char) ; transform the output
            (string->symbol (string op-char))))

    done))
        

(define <Integer>
    (new
        (*parser (maybe <op>))
        (*parser <Natural>)
        (*caten 2)
        
        (*pack-with
            (lambda (opList num)
                (let
                    ((opFound (car opList))
                     (operator (cadr opList)))
                     (if opFound
                        (cond
                            ((symbol=? operator '+) num)
                            ((symbol=? operator '-) (* -1 num))
                            (else (display "The value is no valid!")))
                    num))))
                    
        
        done))
        
        
(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*caten 3)
       (*pack-with (lambda (integer div natural)
	  (/ integer natural)))
       done))
       
        
        
        
(define <Number>
    (new
        (*parser <Fraction>)
        (*parser <Integer>)
        
        (*disj 2)
    done))
    
(define <NotLiteral>
    (new 
        (*parser (char #\\))
        (*parser (char #\"))
        (*disj 2)
        done))


(define <StringLiteralChar>
  (new
    (*parser <any-char>)
    (*parser <NotLiteral>)
    *diff
    done))

    
(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word-ci str))
	 (*pack (lambda (_) ch))
	 done))) 
   
(define <StringMetaChar>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page))
       (*disj 6)
       done)) 

(define <StringHexChar>
    (new
        (*parser (word-ci "\\x"))
        (*parser <HexChar>)
        *star
        (*parser (char #\;))
        (*caten 3)
        (*pack-with
         (lambda (x hex c) 
           (integer->char (string->number (list->string hex) 16))))
     done))



(define <StringChar>
  (new
   (*parser <StringLiteralChar>)
   (*parser <StringMetaChar>)
   (*parser <StringHexChar>)
   (*disj 3)
   done))

   
(define <String>
    (new
        (*parser (char #\"))
        (*parser <StringChar>)
        *star
        (*parser (char #\"))
        (*caten 3)
        (*pack-with
                (lambda (c1 str c2)
                (list->string str)))
  done))  ;Check whitespaces
  

(define <SymbolChar>
  (new
   (*parser <digit-0-9>)
   (*parser <letter-a-z>)
   (*parser (char #\!))
   (*parser (char #\$))
   (*parser (char #\^))
   (*parser (char #\*))
   (*parser (char #\-))
   (*parser (char #\_))
   (*parser (char #\=))
   (*parser (char #\+))
   (*parser (char #\<))
   (*parser (char #\>))
   (*parser (char #\?))
   (*parser (char #\/))
   (*disj 14)
   done))

(define <Symbol>
  (new
   (*parser <SymbolChar>)
   *plus 
   (*pack
    (lambda (symch)
      (string->symbol (string-downcase (list->string symch)))))
   done))
   

; (define comment-pack
;   (lambda (com1 exp com2) exp))
   

(define ^wrap-with-comments
  (lambda (parser)
    (new
      (*parser <Comments>) *star
      (*parser parser)
      (*parser <Comments>) *star
      (*caten 3)
      (*pack-with
        (lambda (com1 parsed-exp com2)
          parsed-exp))
      done)))


;;;;;;;;;;;;;;;;;;; <SEXPR>
(define <sexpr>
  (new
    (*parser <Comments>)
    (*parser <Boolean>)
    (*parser <Char>)
    (*parser <Number>)
    (*parser <String>)
    (*parser <Symbol>)
    (*delayed (lambda () <ProperList>))
    (*delayed (lambda () <ImproperList>))
    (*delayed (lambda () <Vector>))
    (*delayed (lambda () <Quoted>))
    (*delayed (lambda () <QuasiQuoted>))
    (*delayed (lambda () <Unquoted>))
    (*delayed (lambda () <UnquoteAndSpliced>))
    (*delayed (lambda () <CBName>))
    (*delayed (lambda () <InfixExtension>))
    (*disj 15)
  
  done))
;;;;;;;;;;;;;;;;;;;; <SEXPR>
  

(define <sexpr-with-space>
    (new
        (*parser <sexpr>)
        (*parser (char #\space))
        (*caten 2)
        (*pack-with (lambda (sexp s)
            sexp))
        done))

(define <ProperList>
    (new
        (*parser (word "()"))
        (*pack (lambda (_) '()))
        
        (*parser (char #\())
        (*parser <sexpr-with-space>)
        *star
        (*parser <sexpr>)
        (*parser (char #\)))
        (*caten 4)
        (*pack-with
                (lambda (c1 sexp* sexp c2) (append sexp* `(,sexp))))
        (*disj 2)
                
    done))  

(define <ImproperList>
    (new
        (*parser (char #\())
        (*parser <sexpr-with-space>)
        *plus
        (*parser (char #\.))
        (*parser (char #\space))
        (*parser <sexpr>)
        (*parser (char #\)))
        (*caten 6)
        (*pack-with
                (lambda (c1 sexp1 dot space sexp2 c2) `(,@sexp1 . ,sexp2)))          
    done))
    

(define <Vector>
    (new
        (*parser (word "#()"))
        (*pack (lambda (_) '#()))
        
        (*parser (char #\#))
        (*parser (char #\())
        (*parser <sexpr-with-space>)
        *star
        (*parser <sexpr>)
        (*parser (char #\)))
        (*caten 5)
        (*pack-with
                (lambda (hash c1 sexp* sexp c2) (list->vector (append sexp* `(,sexp)))))
        (*disj 2)
                    
    done))  


(define <Quoted>  
    (new
        (*parser (char #\'))
        (*parser <sexpr>)
        (*caten 2)
        (*pack-with (lambda (qu sexp) `',sexp))
    done))
    
    
(define <QuasiQuoted>  
    (new
        (*parser (char #\`))
        (*parser <sexpr>)
        (*caten 2)
        (*pack-with (lambda (qu sexp) (list 'quasiquote `(,@sexp))))
        
    done))
    
    
(define <Unquoted>  
    (new
        (*parser (char #\,))
        (*parser <sexpr>)
        (*caten 2)
        (*pack-with (lambda (qu sexp) (list 'unquote `(,@sexp))))
        
    done))
    
(define <UnquoteAndSpliced>
  (new
    (*parser (word ",@"))
    (*parser <sexpr>)    
    (*caten 2)
    (*pack-with (lambda (unq-shtrudel sexp) (list 'unquote-splicing `(,@sexp))))
    done))
    

(define <CBNameSyntax1>
  (new
    (*parser (char #\@))
    (*parser <sexpr>)    
    (*caten 2)
    (*pack-with (lambda (at-mark sexp) sexp))
    done))

(define <CBNameSyntax2>
  (new
    (*parser (char #\{))
    (*parser <sexpr>)
    (*parser (char #\}))
    (*caten 3)
    (*pack-with (lambda (left-pret sexp right-pret) sexp))
    done))
    
(define <CBName>
  (new
   (*parser <CBNameSyntax1>)
   (*parser <CBNameSyntax2>)
   (*disj 2)
  done))
  


(define <InfixPrefixExtensionPrefix>
  (new
   (*parser (word "##"))
   (*parser (word "#%"))
   (*disj 2)
  done))


(define <infix-Special-Symbol>
  (new
    (*parser (char #\+))
    (*parser (char #\-))
    (*parser (char #\*))
    (*parser (word "**"))
    (*parser (char #\^))
    (*parser (char #\/))
    (*disj 6)
    done))


(define <InfixSymbol>
  (new
    (*parser <Symbol>)
    (*parser <infix-Special-Symbol>)
    *diff
    done))


(define <InfixSexprEscape>
  (new
    (*parser <InfixPrefixExtensionPrefix>)
    (*parser <sexpr>)
    (*caten 2)
    (*pack-with (lambda (prefix sexp) sexp))
    
    done))


(define <InfixExpression>
  (new
    (*delayed (lambda () <layer-1>))
    (*parser <sexpr>)
    (*disj 2)
  done))


(define <InfixExtension>
  (new
    (*parser <InfixPrefixExtensionPrefix>)
    (*parser <InfixExpression>)
    (*caten 2)
    (*pack (lambda (list)
                    (cadr list)))
    done))


(define func
  (lambda (list1 list2)
    (let
      ((op (car list2))
      (param (cdr list2)))
      (cons op (cons list1 param)))))


(define func2
  (lambda (list1 list2)
    (let
      ((op (car list2))
      (param (cadr list2)))
      (cons op (cons param `(,list1))))))


(define append-left
  (lambda (num lst)
    (fold-left func num lst)))


(define <infix-operation-parser>
  (lambda (op-parser upper-layer-exp handler)
    (new
      (*parser upper-layer-exp)
      (*parser op-parser)
      (*parser upper-layer-exp)
      (*caten 2)
      *star

      (*caten 2)
      (*pack-with
        (lambda (num suffix)
          (if (null? suffix) num
          `(,@(handler num suffix)))))
            
    done)))


;; example: #\+ ==> parser of '+' that returns a symbol of '+'
(define ^<charOp->symbol>
  (lambda (op)
    (new
      (*parser (char op))
      (*pack (lambda (op-char) ; transform the output
          (string->symbol (string op-char))))
      done)))


(define <PowerSymbol>
  (new
    (*parser (char #\^))
    (*parser (word "**"))
    (*disj 2)
    (*pack (lambda (_) 'expt))
    
    done))


(define <op-mul> (^<charOp->symbol> #\*))
(define <op-div> (^<charOp->symbol> #\/))
(define <op-add> (^<charOp->symbol> #\+))
(define <op-sub> (^<charOp->symbol> #\-))


(define <layer-1-op>
  (new
    (*parser <op-add>)
    (*parser <op-sub>)
    (*disj 2)
    done))


(define <layer-2-op>
  (new
    (*parser <op-mul>)
    (*parser <op-div>)
    (*disj 2)
    done))


(define <layer-3-op>
  (new
    (*parser <PowerSymbol>)
    done))


(define <layer-4>
  (new
    (*parser <op-sub>)
    *maybe
    (*parser <Number>)
    (*caten 2)
    (*pack-with
      (lambda (neg num)
        (if (car neg)
          `(,(cadr neg) ,num)
          num)))
    
    (*parser (char #\())
    (*delayed (lambda () <layer-1>))
    (*parser (char #\)))
    (*caten 3)
    (*pack-with
      (lambda (open exp close)
       exp))
    
    
    (*parser <InfixSymbol>)
    
    (*disj 3)

    (*delayed (lambda () <layer-5>))
    *plus
    *maybe
   
    (*caten 2)
    (*pack-with 
        (lambda (name exp)
            (if (car exp)
                (cond ((null? (caadr exp)) name)
                    ((eq? (caaadr exp) 'vector-ref)
                        (fold-left func name (cadr exp)))
                  ((cons name (caadr exp))))
                name)))
    done))
 

      
(define <layer-5>
    (new
        (*parser (char #\[))
        (*delayed (lambda () <layer-1>))
        (*parser (char #\]))
        (*caten 3)
        
        (*pack-with
        (lambda (open exp close)
            (list 'vector-ref exp)))
        
        (*parser (char #\())
        (*delayed (lambda () <layer-1>))
        
        (*parser (char #\,))
        (*delayed (lambda () <layer-1>))
        (*caten 2)
        (*pack-with 
            (lambda (ch exp) exp))
        *star
        
        (*caten 2)
        (*pack-with 
            (lambda (exp lexp) `(,exp ,@lexp)))
        (*parser <epsilon>)
        (*disj 2)
        (*parser (char #\)))
        (*caten 3)
        (*pack-with (lambda (open listExp close)
            `(,@listExp)))
            
        (*disj 2)

    done))


(define (reverse l) 
   (fold-left (lambda (i j) 
                (cons j i)) 
              '() 
              l)) 


(define append-temp
  (lambda (num lst)
    (let ((reversed-list (reverse (cons (list (caar lst) num) lst)))
            (last-element (car (reverse lst))))
      (fold-left func2 (cadr last-element) (cdr reversed-list)))))
    

(define <layer-3> (<infix-operation-parser> <layer-3-op> <layer-4> append-temp))
(define <layer-2> (<infix-operation-parser> <layer-2-op> <layer-3> append-left))
(define <layer-1> (<infix-operation-parser> <layer-1-op> <layer-2> append-left))









