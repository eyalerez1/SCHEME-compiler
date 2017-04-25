(load "pc.scm")

(define <digit-0-9>
  (range #\0 #\9))

(define <digit-1-9>
  (range #\1 #\9))

(define <lower-case>
  (range #\a #\z))

(define <upper-case>
  (range #\A #\Z))

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <LineComment>
  (new (*parser (char #\;))
       (*parser <any-char>)
       (*parser <end-of-input>)
       (*parser (char #\newline))
       (*disj 2)
       *diff
       *star
       (*parser <end-of-input>)
       (*parser (char #\newline))
       (*disj 2)
       (*caten 3)
       done))

(define <SexprComment>
  (new (*parser (word "#;"))
       (*delayed (lambda() <Sexpr>))
       (*caten 2)
       done))

(define <InfixExpressionComment>
  (new (*parser (word "#;"))
       (*delayed (lambda() <InfixExpression>))
       (*caten 2)
       done))

(define <InfixComment>
  (new (*parser <LineComment>)
       (*parser <InfixExpressionComment>)
       (*disj 2)
       done))

(define <Comment>
  (new (*parser <LineComment>)
       (*parser <SexprComment>)
       (*disj 2)
       done))

(define flatten
  (lambda (list)
    (cond ((null? list) list)
    ((list? (car list)) (append (flatten (car list)) (flatten (cdr list))))
    (else
     (cons (car list) (flatten (cdr list)))))))

(define <Boolean>
  (new (*parser <whitespace>)
       *star
       (*parser (char #\#))
       (*parser (char-ci #\f))
       (*parser (char-ci #\t))
       (*disj 2)
       (*parser <whitespace>)
       *star
       (*caten 4)
       (*pack-with
  (lambda (s1 _ bool s2)
    (if (char-ci=? bool #\t) #t #f)))
       done))

(define <HexChar>
  (new (*parser <digit-0-9>)
       (*parser (range #\a #\f))
       (*parser (range #\A #\F))
       (*disj 3)
       done))

(define <HexUnicodeChar>
  (new (*parser (char-ci #\x))
       (*parser <HexChar>)
       *plus
       (*caten 2)
       (*pack-with
  (lambda (x s)
    (string->number
     (list->string
      s) 16)))
       (*guard (lambda (x)
     (< x 1114112)))
       (*pack
  (lambda (x)
    (integer->char x)))
       done))

(define <CharPrefix>
  (new (*parser (char #\#))
       (*parser (char #\\))
       (*caten 2)
       done))

(define <VisibleChar>
  (range #\! #\~))

(define <VisibleSimpleChar>
  (new (*parser <VisibleChar>)
       (*parser (char-ci #\x))
       *diff
       ;; (*parser <VisibleChar>)
       ;; *not-followed-by

       (*parser (char-ci #\x))
       (*parser <HexChar>)
       *not-followed-by
       
       (*disj 2)
       done))

(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word-ci str))
   (*pack
    (lambda (_)
      ch))
   done)))

(define <NamedChar>
  (new (*parser (^<meta-char> "lambda" (integer->char 955)))
       (*parser (^<meta-char> "newline" #\newline))
       (*parser (^<meta-char> "tab" #\tab))
       (*parser (^<meta-char> "page" #\page))
       (*parser (^<meta-char> "return" #\return))
       (*parser (^<meta-char> "space" #\space))
       (*parser (^<meta-char> "nul" #\nul))
       (*disj 7)
       done))

(define <Char>
  (new (*parser <whitespace>)
       *star
       (*parser <CharPrefix>)
       (*parser <NamedChar>)
       (*parser <HexUnicodeChar>)
       (*parser <VisibleSimpleChar>)
       (*disj 3)
       (*parser <whitespace>)
       *star
       (*caten 4)
       (*pack-with
  (lambda (s1 cp char s2)
    char))
       done))

(define <Natural>
  (new (*parser (char #\0))
       *star
       (*parser <digit-1-9>)
       (*parser <digit-0-9>)
       *star
       (*caten 3)
       (*pack-with
  (lambda (_ a s)
    (string->number
     (list->string
      (cons a s)))))
       
       (*parser (char #\0))
       *plus
       (*pack (lambda (_) 0))
       (*disj 2)
       done))

(define <Integer>
  (new (*parser (char #\+))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
  (lambda (plus n) n))

       (*parser (char #\-))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
  (lambda (minus n) (- n)))

       (*parser <Natural>)

       (*disj 3)
       done))

(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*caten 3)
       (*pack-with
  (lambda (a divide b)
    (/ a b)))
       done))

(define <VisibleString>
  (range #\space #\~))

(define <StringLiteralChar>
  (new (*parser <VisibleString>)
       (*parser (char #\\))
       *diff
       done))

(define <StringMetaChar>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*disj 6)
       (*pack
  (lambda (exp)
    exp))
       done))

(define <StringHexChar>
  (new (*parser (char #\\))
       (*parser (char-ci #\x))
       (*parser <HexChar>)
       *star
       (*parser (char #\;))
       (*caten 4)
       (*pack-with
  (lambda (bs x str sem)
    (string->number
     (list->string
      str)
     16)))
       (*guard (lambda (x)
     (< x 1114112)))
       (*pack
  (lambda (x)
    (integer->char x)))
       done))

(define <StringChar>
  (new  (*parser <StringHexChar>)
  (*parser <StringMetaChar>)
  (*parser <StringLiteralChar>)
        (*disj 3)
        done))

(define <String>
  (new (*parser <whitespace>)
       *star
       (*parser (char #\"))
       (*parser <StringChar>)
       (*parser (char #\"))
       *diff
       *star
       (*parser (char #\"))
       (*parser <whitespace>)
       *star
       (*caten 5)
       (*pack-with
  (lambda (s1 q1 lst q2 s2)
    (list->string lst)))
       done))

(define <SymbolChar>
  (new (*parser <lower-case>)
       (*parser <upper-case>)
       (*pack
  (lambda (ch)
    (char-downcase ch)))
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
  (new (*parser <whitespace>)
       *star
       (*parser <digit-0-9>)
       *star
       (*parser <SymbolChar>)
       *plus
       (*parser <digit-0-9>)
       *star
       (*caten 2)
       *plus
       (*parser <whitespace>)
       *star
       (*caten 4)
       (*pack-with
  (lambda (s1 nums others s2)
    (string->symbol
     (list->string
      (flatten (cons nums others))))))
       done))

(define <Number>
  (new (*parser <whitespace>)
       *star
       (*parser <Fraction>)
       (*parser <Integer>)
       (*disj 2)
       (*parser <SymbolChar>)
       *not-followed-by
       (*parser <whitespace>)
       *star
       (*caten 3)
       (*pack-with
  (lambda (s1 num s2)
    num))
       done))

(define <ProperList>
  (new (*parser <whitespace>)
       *star
       (*parser (char #\())
       (*delayed (lambda() <Sexpr>))
       (*parser (char #\)))
       *diff
       *star
       (*parser (char #\)))
       (*parser <whitespace>)
       *star
       (*caten 5)
       (*pack-with
  (lambda (s1 q1 lst q2 s2)
    lst))
       done))

(define <ImproperList>
  (new (*parser <whitespace>)
       *star
       (*parser (char #\())
       (*delayed (lambda() <Sexpr>))
       *plus
       (*parser (char #\.))
       (*delayed (lambda() <Sexpr>))
       (*parser (char #\)))
       (*parser <whitespace>)
       *star
       (*caten 7)
       (*pack-with
  (lambda (s1 q1 lst1 dot lst2 q2 s2)
    `(,@lst1 . ,lst2)))
       done))

(define <Vector>
  (new (*parser <whitespace>)
       *star
       (*parser (char #\#))
       (*parser (char #\())
       (*delayed (lambda() <Sexpr>))
       *star
       (*parser (char #\)))
       (*parser <whitespace>)
       *star
       (*caten 6)
       (*pack-with
  (lambda (s1 _ q1 lst q2 s2)
    (list->vector lst)))
       done))

(define <Quoted>
  (new (*parser <whitespace>)
       *star
       (*parser (char #\'))
       (*delayed (lambda () <Sexpr>))
       (*parser <whitespace>)
       *star
       (*caten 4)
       (*pack-with
  (lambda (s1 _ sexpr s2)
    (list 'quote sexpr)))
       done))

(define <QuasiQuoted>
  (new (*parser <whitespace>)
       *star
       (*parser (char #\`))
       (*delayed (lambda () <Sexpr>))
       (*parser <whitespace>)
       *star
       (*caten 4)
       (*pack-with
  (lambda (s1 _ sexpr s2)
    (list 'quasiquote sexpr)))
       done))

(define <Unquoted>
  (new (*parser <whitespace>)
       *star
       (*parser (char #\,))
       (*delayed (lambda () <Sexpr>))
       (*parser <whitespace>)
       *star
       (*caten 4)
       (*pack-with
  (lambda (s1 _ sexpr s2)
    (list 'unquote sexpr)))
       done))

(define <UnquoteAndSpliced>
  (new (*parser <whitespace>)
       *star
       (*parser (char #\,))
       (*parser (char #\@))
       (*delayed (lambda () <Sexpr>))
       (*parser <whitespace>)
       *star
       (*caten 5)
       (*pack-with
  (lambda (s1 co sh sexpr s2)
    (list 'unquote-splicing sexpr)))
       done))

(define <InfixPrefixExtensionPrefix>
  (new (*parser (char #\#))
       (*parser (char #\#))
       (*parser (char #\%))
       (*disj 2)
       (*caten 2)
       done))

(define <NotWanted>
  (new (*parser (char #\+))
       (*parser (char #\-))
       (*parser (char #\*))
       (*parser (word "**"))
       (*parser (char #\^))
       (*parser (char #\/))
       (*disj 6)
       done))

(define <InfixSymbol>
  (new (*parser <whitespace>)
       *star
       (*parser <InfixComment>)
       *maybe
       (*parser <digit-0-9>)
       *star
       (*parser <SymbolChar>)
       (*parser <NotWanted>)
       *diff
       *plus
       (*parser <digit-0-9>)
       *star
       (*caten 2)
       *plus
       (*parser <whitespace>)
       *star
       (*parser <InfixComment>)
       *maybe
       (*caten 6)
       (*pack-with
  (lambda (s1 c1 nums others s2 c2)
    (string->symbol
     (list->string
      (flatten (cons nums others))))))
       done))

(define <InfixParen>
  (new (*parser (char #\())
       (*parser <whitespace>)
       *star
       (*parser <InfixComment>)
       *maybe
       (*delayed (lambda() <InfixExpression>))
       (*parser <whitespace>)
       *star
       (*parser <InfixComment>)
       *maybe
       (*parser (char #\)))
       (*caten 7)
       (*pack-with
  (lambda (q1 s1 c1 a s2 c2 q2)
    `(,@a)))
       done))

(define <InfixSexprEscape>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*delayed (lambda() <Sexpr>))
       (*caten 2)
       (*pack-with
  (lambda (pre exp)
    exp))
       done))

(define <InfixNum>
  (new (*parser <Fraction>)
       (*parser <Integer>)
       (*disj 2)
       (*parser <SymbolChar>)
       (*parser <NotWanted>)
       (*parser (char #\())
       (*parser (char #\)))
       (*parser (char #\[))
       (*parser (char #\]))
       (*disj 5)
       *diff
       *not-followed-by
       done))

(define <InfixNumber>
  (new (*parser <InfixNum>)
       (*parser <InfixSymbol>)
       (*parser <InfixParen>)
       (*parser <InfixSexprEscape>)
       (*disj 4)
       done))

(define <InfixAdd>
  (new (*delayed (lambda () <InfixMul>))
       (*parser (char #\+))
       (*parser (char #\-))
       (*disj 2)
       (*delayed (lambda () <InfixMul>))
       (*caten 2)
       *star
       (*caten 2)
       (*pack-with
  (lambda (a lst)
    (letrec ((help (lambda (ans rest)
         (if (null? rest)
             ans
             (help `(,(string->symbol (make-string 1 (car (car rest)))) ,ans ,@(cdr (car rest))) (cdr rest))))))
      
      (help a lst))))
       done))

(define <InfixMul>
  (new (*delayed (lambda () <InfixPow>))
       (*parser (char #\*))
       (*parser (char #\/))
       (*disj 2)
       (*delayed (lambda () <InfixPow>))
       (*caten 2)
       *star
       (*caten 2)
       (*pack-with
  (lambda (a lst)
    (letrec ((help (lambda (ans rest)
         (if (null? rest)
             ans
             (help `(,(string->symbol (make-string 1 (car (car rest)))) ,ans ,@(cdr (car rest))) (cdr rest))))))
      (help a lst))))
       done))

(define <InfixPow>
  (new (*delayed (lambda() <InfixNeg>))
       (*parser (char #\^))
       (*parser (word "**"))
       (*disj 2)
       (*delayed (lambda() <InfixNeg>))
       (*caten 2)
       *star
       (*caten 2)
       (*pack-with
  (lambda (a lst)
    (letrec ((help (lambda (ans rest)
         (if (null? rest)
             ans
             `(expt ,ans ,(help (cadr (car rest)) (cdr rest)))))))
      (help a lst))))
       done))

(define removeComma
  (lambda (lst)
    (if (null? lst)
  lst
  (cons (cadar lst) (removeComma (cdr lst))))))

(define <InfixArgList>
  (new (*delayed (lambda() <InfixExpression>))
       (*parser (char #\,))
       (*delayed (lambda() <InfixExpression>))
       (*caten 2)
       *star
       (*caten 2)
       (*parser <epsilon>)
       (*disj 2)
       (*parser <whitespace>)
       *star
       (*parser <InfixComment>)
       *maybe
       (*caten 3)
       (*pack-with
  (lambda (lst spaces comments)
    (cond ((null? lst) lst)
    ((null? (cdr lst)) (car lst))
    (else
     (cons (car lst) (removeComma (cadr lst)))))))
       done))

(define <InfixFuncOrArray>
  (new (*parser <whitespace>)
       *star
       (*parser <InfixComment>)
       *maybe

       (*delayed (lambda() <InfixNumber>))

       (*parser <whitespace>)
       *star
       (*parser <InfixComment>)
       *maybe

       (*parser (char #\[))
       (*delayed (lambda() <InfixExpression>))
       (*parser (char #\]))
       (*caten 3)

       (*parser (char #\())
       (*parser <InfixArgList>)
       (*parser (char #\)))
       (*caten 3)

       (*disj 2)
       *star

       (*parser <whitespace>)
       *star
       (*parser <InfixComment>)
       *maybe

       (*caten 8)
       (*pack-with
  (lambda (s1 c1 name s2 c2 args s3 c3)
    (letrec ((help (lambda (ans rest)
         (cond ((null? rest) ans)
         ((char=? (caar rest) #\()
          (help (cons ans (cadar rest)) (cdr rest)))
         (else
          (help `(vector-ref ,ans ,(cadar rest)) (cdr rest)))))))
      (help name args))))
       done))

(define <InfixNeg>
  (new (*parser (char #\-))
       *maybe
       (*parser <whitespace>)
       *star
       (*parser <InfixFuncOrArray>)
       (*caten 3)
       (*pack-with
  (lambda (minus spaces a)
    (if (car minus)
        (if (number? a)
      (if (null? spaces)
          (- a)
          `(-,a))
      `(-,a))
        `,a)))
       done))       

(define <InfixExpression>
  (new (*parser <whitespace>)
       *star
       (*parser <InfixComment>)
       *maybe
       
       (*parser <InfixAdd>)
       (*parser <InfixParen>)
       (*parser <InfixSexprEscape>)
       (*disj 3)
       (*parser <whitespace>)
       *star
       (*parser <InfixComment>)
       *maybe
       (*caten 5)
       (*pack-with
  (lambda (s1 c1 exp s2 c2)
    exp))
       done))

(define <InfixExtension>
  (new (*parser <whitespace>)
       *star
       (*parser <InfixComment>)
       *maybe
       (*parser <InfixPrefixExtensionPrefix>)
       (*parser <whitespace>)
       *star
       (*parser <InfixComment>)
       *maybe
       (*parser <InfixExpression>)
       (*parser <whitespace>)
       *star
       (*parser <InfixComment>)
       *maybe
       (*caten 8)
       (*pack-with
  (lambda (s1 c1 pre s2 c2 exp s3 c3)
    exp))
       done))

(define <Sexpr>
  (new (*parser <whitespace>)
       *star
       (*parser <Comment>)
       *maybe
       (*parser <InfixExtension>)
       (*parser <Boolean>)
       (*parser <Char>)
       (*parser <String>)
       (*parser <Number>)
       (*parser <Symbol>)
       (*parser <ProperList>)
       (*parser <ImproperList>)
       (*parser <Vector>)
       (*parser <Quoted>)
       (*parser <QuasiQuoted>)
       (*parser <Unquoted>)
       (*parser <UnquoteAndSpliced>)
       (*disj 13)
       (*parser <whitespace>)
       *star
       (*parser <Comment>)
       *maybe
       (*caten 5)
       (*pack-with
  (lambda (space1 comment1 sexpr space2 comment2)
    sexpr))
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HW2 BEGINS HERE!! - TAG-PARSER! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "pattern-matcher.scm")

(define *reserved-words*
  '(and begin cond define do else if lambda
  let let* letrec or quasiquote unquote
  unquote-splicing quote set!))

(define var?
  (lambda (x)
    (and (symbol? x)
   (not (member x *reserved-words*)))))

(define val?
  (lambda (x)
    #t))

(define no_duplicates?
  (lambda (list)
    (if (null? list)
  #t
  (and (= (- (length list) 1) (length (remove (car list) list))) (no_duplicates? (cdr list))))))

(define get-vars
  (lambda (list)
    (map car list)))

(define get-expr
  (lambda (list)
    (map cadr list)))

(define remove_begin
  (lambda (lst)
    (if (or (null? lst) (not (pair? lst)))
  lst
  (if (pair? (car lst))
      (if (eq? (caar lst) 'begin)
    (improper->proper (append (cdr (remove_begin (car lst))) (remove_begin (cdr lst))))
    (if (eq? (caar lst) 'lambda)
        (improper->proper (append (list (car lst)) (remove_begin (cdr lst))))
        (improper->proper (append (list (remove_begin (car lst))) (remove_begin (cdr lst))))))
      (append (list (car lst)) (remove_begin (cdr lst)))))))

(define simple-const?
  (lambda (c)
    (or (eq? c ''()) (vector? c) (boolean? c) (char? c) (number? c) (string? c))))

(define parse
  (let ((run
   (compose-patterns
          ; const
    (pattern-rule
     (? 'c simple-const?)
     (lambda (c) `(const ,c)))
          ; quote
    (pattern-rule
     `(quote ,(? 'c))
     (lambda (c) `(const ,c)))
          ; var
    (pattern-rule
     (? 'v var?)
     (lambda (v) `(var ,v)))
          ; if without else
    (pattern-rule
     `(if ,(? 'test) ,(? 'dit))
     (lambda (test dit) `(if3 ,(parse test) ,(parse dit) (const ,(void)))))
          ; if
    (pattern-rule
     `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
     (lambda (test dit dif) `(if3 ,(parse test) ,(parse dit) ,(parse dif))))
          ; or
    (pattern-rule
     `(or ,@(? 'rest))
     (lambda (rest)
       (cond ((null? rest) (parse #f))
       ((null? (cdr rest)) (parse (car rest)))
       (else `(or ,(map parse rest))))))
          ; lambda-simple
    (pattern-rule
     `(lambda ,(? 'args list? (lambda (args) (andmap var? args)) (lambda (args) (no_duplicates? args))) ,@(? 'body))
     (lambda (args body)
       (if (null? (cdr body))
     `(lambda-simple ,args ,(parse (car body)))
     `(lambda-simple ,args ,(parse `(begin ,@body))))))
          ; lambda-opt
    (pattern-rule
     `(lambda ,(? 'args pair? (lambda (args) (not (list? args)))) ,@(? 'body))
     (lambda (args body)
       (let* ((proper (improper->proper args))
        (vars (list->head proper (- (length proper) 1)))
        (rest (list-tail proper (- (length proper) 1))))
         (if (null? (cdr body))
       `(lambda-opt ,vars ,@rest ,(parse (car body)))
       `(lambda-opt ,vars ,@rest ,(parse `(begin ,@body)))))))
          ; lambda-var
    (pattern-rule
     `(lambda ,(? 'args var?) ,@(? 'body))
     (lambda (args body)
       (if (null? (cdr body))
     `(lambda-var ,args ,(parse (car body)))
     `(lambda-var ,args ,(parse `(begin ,@body))))))
          ; define
    (pattern-rule
     `(define ,(? 'var var?) ,@(? 'def))
     (lambda (var def)
       `(def ,(parse var) ,(parse (beginify def)))))
          ; MIT-define
    (pattern-rule
     `(define ,(? 'args pair?) ,@(? 'body))
     (lambda (args body)
       `(def ,(parse (car args)) ,(parse `(lambda ,(cdr args) ,(beginify body))))))
          ; application
    (pattern-rule
     (? 'func pair? (lambda (func) (not (member (car func) *reserved-words*))))
     (lambda (func)
       `(applic ,(parse (car func)) ,(map parse (cdr func)))))
          ; sqeuence
    (pattern-rule
     `(begin ,@(? 'expr list?))
     (lambda (expr)
       (cond ((null? expr) `(const ,(void)))
       ((null? (cdr expr)) (parse (car expr)))
       (else `(seq ,(map parse (remove_begin expr)))))))
          ; let
    (pattern-rule
     `(let ,(? 'expr list? (lambda (list) (andmap pair? list)) (lambda (expr) (no_duplicates? (get-vars expr)))) ,@(? 'body))
     (lambda (expr body)
       (parse `((lambda ,(get-vars expr) ,@body) ,@(get-expr expr)))))
          ; let*
    (pattern-rule
     `(let* ,(? 'expr list? (lambda (list) (andmap pair? list))) ,@(? 'body))
     (lambda (expr body)
       (cond ((null? expr) (parse `((lambda () ,@body))))
       ((null? (cdr expr)) (parse `((lambda (,(caar expr)) ,(beginify body)) ,(cadar expr))))
       (else (parse `((lambda ,(list (car (get-vars expr))) (let* ,(cdr expr) ,@body)) ,(car (get-expr expr))))))))   
          ; letrec
    (pattern-rule
     `(letrec ,(? 'expr list? (lambda (list) (andmap pair? list)) (lambda (expr) (no_duplicates? (get-vars expr)))) ,@(? 'body))
     (lambda (expr body)
       (parse `((lambda ,(get-vars expr)
      (begin ,@(append (map (lambda (pair) (list 'set! (car pair) (cadr pair))) expr) `(((lambda () ,@body))))))
          ,@(map (lambda (x) #f) expr)))))
          ; and
    (pattern-rule
     `(and ,@(? 'expr list?))
     (lambda (expr)
       (cond ((null? expr) (parse #t))
       ((null? (cdr expr)) (parse (car expr)))
       (else (parse `(if ,(car expr) (and ,@(cdr expr)) #f))))))
          ; cond
    (pattern-rule
     `(cond ,@(? 'expr list?))
     (lambda (expr)
       (cond ((null? (cdr expr)) (if (and (symbol? (caar expr)) (symbol=? (caar expr) 'else))
             (parse (beginify (cdar expr)))
             (parse `(if ,(caar expr) ,(beginify (cdar expr))))))
       (else (parse `(if ,(caar expr) ,(beginify (cdar expr)) (cond ,@(cdr expr))))))))
          ; set!
    (pattern-rule
     `(set! ,(? `var var?) ,(? `val))
     (lambda (var val)
       `(set ,(parse var) ,(parse val))))
          ; quasiquote
    (pattern-rule
     (? 'expr pair? (lambda (exp) (eq? (car exp) 'quasiquote)))
     (lambda (expr)
       (parse (expand-qq (cadr expr)))))

    )))
    (lambda (e)
      (run e
     (lambda ()
       (error 'parse
        (format "I can't recognize this: ~s" e)))))))

(define beginify
  (lambda (s)
    (cond
     ((null? s) *void-object*)
     ((null? (cdr s)) (car s))
     (else `(begin ,@s)))))

(define improper->proper
  (lambda (lst)
    (if (list? lst)
  lst
  (if (not (pair? lst))
      (cons lst '())
      (cons (car lst) (improper->proper (cdr lst)))))))

(define list->head
  (lambda (lst n)
    (if (= n 0)
  '()
  (cons (car lst) (list->head (cdr lst) (- n 1))))))


;;;;;;;;;;;;;;;;;
;; quasi-quote ;;
;;;;;;;;;;;;;;;;;
(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
     (eq? (car e) tag)
     (pair? (cdr e))
     (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
   (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
     simple-sexprs-predicates)
    (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
      (pair? e)
      (symbol? e)
      (vector? e))
  `',e
  e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
  (cadr e)
  e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
   (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
      (lambda (e)
        (cond ((unquote? e) (cadr e))
        ((unquote-splicing? e)
         (error 'expand-qq
          "unquote-splicing here makes no sense!"))
        ((pair? e)
         (let ((a (car e))
         (b (cdr e)))
           (cond ((unquote-splicing? a)
            `(append ,(cadr a) ,(expand-qq b)))
           ((unquote-splicing? b)
            `(cons ,(expand-qq a) ,(cadr b)))
           (else `(cons ,(expand-qq a) ,(expand-qq b))))))
        ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
        ((or (null? e) (symbol? e)) `',e)
        (else e))))
     (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
     (optimizer
      (compose-patterns
       (pattern-rule
        `(append ,(? 'e) '())
        (lambda (e) (optimize-qq-expansion e)))
       (pattern-rule
        `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
        (lambda (c1 c2 e)
    (let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
          (e (optimize-qq-expansion e)))
      (optimize-qq-expansion `(append ,c ,e)))))
       (pattern-rule
        `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
        (lambda (c1 c2)
    (let ((c (quotify (append (unquotify c1) (unquotify c2)))))
      c)))
       (pattern-rule
        `(append ,(? 'e1) ,(? 'e2))
        (lambda (e1 e2)
    (let ((e1 (optimize-qq-expansion e1))
          (e2 (optimize-qq-expansion e2)))
      `(append ,e1 ,e2))))
       (pattern-rule
        `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
        (lambda (c1 c2 e)
    (let ((c (quotify (list (unquotify c1) (unquotify c2))))
          (e (optimize-qq-expansion e)))
      (optimize-qq-expansion `(append ,c ,e)))))
       (pattern-rule
        `(cons ,(? 'e1) ,(? 'e2))
        (lambda (e1 e2)
    (let ((e1 (optimize-qq-expansion e1))
          (e2 (optimize-qq-expansion e2)))
      (if (and (const? e1) (const? e2))
          (quotify (cons (unquotify e1) (unquotify e2)))
          `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HW3 BEGINS HERE!! - ELIMINATE-NESTED-DEFINES! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          ; expr is a parsed exprassion!
(define eliminate-nested-defines
  (lambda (expr)
    (cond ((or (not (pair? expr)) (null? expr)) expr)
    ((or (eq? (car expr) 'lambda-simple) (eq? (car expr) 'lambda-var))
     (if (and (pair? (caddr expr)) (eq? (caaddr expr) 'def))
         (cons (car expr) (cons (cadr expr) (eliminate-nested-defines (^defletrec (caddr expr)))))
         (if (and (pair? (caddr expr)) (eq? (caaddr expr) 'seq) (eq? (caaar (cdaddr expr)) 'def))
       (cons (car expr) (cons (cadr expr) (eliminate-nested-defines (^defletrec (car (cdaddr expr))))))
       (cons (car expr) (cons (cadr expr) (eliminate-nested-defines (cddr expr)))))))
    ((eq? (car expr) 'lambda-opt)
     (if (and (pair? (cadddr expr)) (eq? (car (cadddr expr)) 'def))
         (cons (car expr) (cons (cadr expr) (cons (caddr expr) (eliminate-nested-defines (^defletrec (cdddr expr))))))
         (if (and (pair? (cadddr expr)) (eq? (car (cadddr expr)) 'seq) (eq? (caaadr (cadddr expr)) 'def))
       (cons (car expr) (cons (cadr expr) (cons (caddr expr) (eliminate-nested-defines (^defletrec (cadr (cadddr expr)))))))
       (cons (car expr) (cons (cadr expr) (cons (caddr expr) (eliminate-nested-defines (cdddr expr))))))))     
    (else (cons (eliminate-nested-defines (car expr)) (eliminate-nested-defines (cdr expr)))))
    ))

(define ^defletrec
  (lambda (expr)
    (let* ((deflist (^deflist expr))
     (body (^body expr)))
      `((applic (lambda-simple ,(map (lambda (x) (cadadr x)) deflist) (seq (,@(map (lambda (x) `(set ,@(cdr x))) deflist) ,@body))) ,(map (lambda (x) (parse #f)) deflist))))
    ))

(define ^deflist
  (lambda (expr)
    (if (and (pair? expr) (pair? (car expr)) (eq? (caar expr) 'def))
  (cons (car expr) (^deflist (cdr expr)))
  '())
    ))

(define ^body
  (lambda (expr)
    (if (and (pair? expr) (pair? (car expr)) (eq? (caar expr) 'def))
  (^body (cdr expr))
  expr)
    ))

(define remove-applic-lambda-nil
  (lambda (expr)
    (if (pair? expr)
  (if (and (eq? (car expr) 'applic) (pair? (cdr expr)) (eq? (caadr expr) 'lambda-simple) (null? (cadadr expr)) (null? (caddr expr)))
      (remove-applic-lambda-nil (car (cddadr expr)))
      (cons (remove-applic-lambda-nil (car expr)) (remove-applic-lambda-nil (cdr expr))))
  expr)))

(define box-set
  (lambda (expr)
    (cond ((or (not (pair? expr)) (null? expr)) expr)
    ((eq? (car expr) 'lambda-simple) (cons (car expr) (cons (cadr expr) (box-vars (cadr expr) '() (caddr expr)))))
    ((eq? (car expr) 'lambda-var) (cons (car expr) (cons (cadr expr) (box-vars (improper->proper (cadr expr)) '() (caddr expr)))))
    ((eq? (car expr) 'lambda-opt) (cons (car expr) (cons (cadr expr) (cons (caddr expr) (box-vars (improper->proper (append (cadr expr) (caddr expr))) '() (cadddr expr))))))
    (else (cons (box-set (car expr)) (box-set (cdr expr)))))
    ))

(define box-vars
  (lambda (vars bvars body)
    (let* ((tvars (filter (lambda (x) (and (set? x body) (get? x body) (bound? x body))) vars))
     (cut_vars (filter (lambda (x) (not (member x bvars))) tvars))
     (vars2box (append bvars cut_vars))
     (seq_body (if (and (eq? (car body) 'seq) (not (null? tvars)))
       (cadr body)
       body))
     (temp_body (create_body vars2box seq_body))
     (new_body (if (null? (cdr temp_body))
         (car temp_body)
         temp_body)))
      (if (null? tvars)
    (list new_body)
    `((seq (,@(map (lambda (x) `(set (var ,x) (box (var ,x)))) tvars) ,@(if (eq? (car body) 'seq) `(,@new_body) `(,new_body)))))))
    ))

(define create_body
  (lambda (vars body)
    (cond ((or (not (pair? body)) (null? body)) body)
    ((and (eq? (car body) 'var) (member (cadr body) vars)) `(box-get ,body))
    ((and (eq? (car body) 'set) (member (cadadr body) vars)) `(box-set ,(cadr body) ,(create_body vars (caddr body))))
    ((or (eq? (car body) 'lambda-simple) (eq? (car body) 'lambda-var))
     (let* ((cut_vars (filter (lambda (x) (not (member x (improper->proper (cadr body))))) vars))
      (new_vars (filter (lambda (x) (and (set? x (caddr body)) (get? x (caddr body)) (bound? x (caddr body)))) (improper->proper (cadr body))))
      (vars2box (append cut_vars new_vars)))
       (cons (car body) (cons (cadr body) (box-vars (improper->proper (cadr body)) vars2box (caddr body))))))
    ((eq? (car body) 'lambda-opt)
     (let* ((cut_vars (filter (lambda (x) (not (member x (improper->proper (cons (cadr body) (caddr body)))))) vars))
      (new_vars (filter (lambda (x) (and (set? x (cadddr body)) (get? x (cadddr body)) (bound? x (cadddr body)))) (improper->proper (cons (cadr body) (caddr body)))))
      (vars2box (append cut_vars new_vars)))
       (cons (car body) (cons (cadr body) (cons (caddr body) (box-vars (improper->proper (append (cadr body) (caddr body))) vars2box (cadddr body)))))))
    (else (cons (create_body vars (car body)) (create_body vars (cdr body)))))
    ))
     
(define bound?
  (lambda (var body)
    (cond ((not (pair? body)) #f)
    ((eq? (car body) 'lambda-simple) (or (set? var body) (get? var body)))
    ((eq? (car body) 'lambda-var) (or (set? var body) (get? var body)))
    ((eq? (car body) 'lambda-opt) (or (set? var body) (get? var body)))
    (else (or (bound? var (car body)) (bound? var (cdr body)))))
    ))

(define set?
  (lambda (var body)
    (cond ((not (pair? body)) #f)
    ((and (eq? (car body) 'set) (eq? (cadadr body) var) (eq? (caadr body) 'var)) #t)
    ((eq? (car body) 'lambda-simple) (if (member var (cadr body)) #f (set? var (cddr body))))
    ((eq? (car body) 'lambda-var) (if (eq? var (cadr body)) #f (set? var (cddr body))))
    ((eq? (car body) 'lambda-opt) (if (or (member var (cadr body)) (eq? var (caddr body))) #f (set? var (cdddr body))))
    (else (or (set? var (car body)) (set? var (cdr body)))))
    ))

(define get?
  (lambda (var body)
    (cond ((not (pair? body)) #f)
    ((eq? (car body) 'set) (get? var (cddr body)))
    ((eq? (car body) 'lambda-simple) (if (member var (cadr body)) #f (get? var (cddr body))))
    ((eq? (car body) 'lambda-var) (if (eq? var (cadr body)) #f (get? var (cddr body))))
    ((eq? (car body) 'lambda-opt) (if (or (member var (cadr body)) (eq? var (caddr body))) #f (get? var (cdddr body))))
    ((and (eq? (car body) 'var) (eq? (cadr body) var)) #t)
    (else (or (get? var (car body)) (get? var (cdr body)))))
    ))

(define pe->lex-pe
  (lambda (expr)
    (lex expr '())
    ))

(define lex
  (lambda (expr lst)
    (cond ((not (pair? expr)) expr)
    ((eq? (car expr) 'var) (if (ormap (lambda (x) (if (pair? x) (member (cadr expr) x) (eq? (cadr expr) x))) lst)
             (check_level (cadr expr) lst)
             `(fvar ,(cadr expr))))
    ((eq? (car expr) 'lambda-simple) (cons (car expr) (cons (cadr expr) (lex (cddr expr) (cons (cadr expr) lst)))))
    ((eq? (car expr) 'lambda-var) (cons (car expr) (cons (cadr expr) (lex (cddr expr) (cons (improper->proper (cadr expr)) lst)))))
    ((eq? (car expr) 'lambda-opt)
     (cons (car expr) (cons (cadr expr) (cons (caddr expr) (lex (cdddr expr) (cons (append (cadr expr) (list (caddr expr))) lst))))))
    (else (cons (lex (car expr) lst) (lex (cdr expr) lst))))          
    ))

          ; -1 means parameter, otherwise it's bound in level n
(define check_level
  (lambda (var lst)
    (letrec ((major_level (lambda (var lst n)
          (if (member var (car lst))
        (cons (car lst) n)
        (major_level var (cdr lst) (+ n 1))))))
      (let* ((level (major_level var lst -1))
       (major (cdr level))
       (minor (minor_level var (car level) 0)))
  (if (< major 0)
      `(pvar ,var ,minor)
      `(bvar ,var ,major ,minor))))
    ))


(define minor_level
  (lambda (var lst n)
    (if (eq? (car lst) var)
  n
  (minor_level var (cdr lst) (+ n 1)))
    ))

(define annotate-tc
  (lambda (expr)
    (^tc expr #f)
    ))

(define ^tc
  (lambda (expr bool)
    (cond ((not (pair? expr)) expr)
    ((and (null? (cdr expr)) (pair? (car expr)))
     (cond ((and bool (eq? (caar expr) 'applic)) `((tc-applic ,@(^tc (cdar expr) #f))))
     ((or (eq? (caar expr) 'lambda-simple) (eq? (caar expr) 'lambda-var)) (list (cons (caar expr) (cons (cadar expr) (^tc (cddar expr) #t)))))
     ((eq? (caar expr) 'lambda-opt) (list (cons (caar expr) (cons (cadar expr) (cons (caddar expr) (^tc (cdddar expr) #t))))))
     ((eq? (caar expr) 'if3) `((if3 ,(^tc (cadar expr) #f) ,(^tc (caddar expr) bool) ,(^tc (car (cdddar expr)) bool))))
     ((eq? (caar expr) 'or) (list (^tc (car expr) bool)))
     ((eq? (caar expr) 'seq) `((seq ,(^tc (cadar expr) bool))))
     (else (list (^tc (car expr) #f)))))
    ((or (eq? (car expr) 'lambda-simple) (eq? (car expr) 'lambda-var)) (cons (car expr) (cons (cadr expr) (^tc (cddr expr) #t))))
    ((eq? (car expr) 'lambda-opt) (cons (car expr) (cons (cadr expr) (cons (caddr expr) (^tc (cdddr expr) #t)))))
    ((and bool (eq? (car expr) 'applic)) `(tc-applic ,@(^tc (cdr expr) #f)))
    ((eq? (car expr) 'if3) `(if3 ,(^tc (cadr expr) #f) ,(^tc (caddr expr) bool) ,(^tc (cadddr expr) bool)))
    ((eq? (car expr) 'or) (cons (car expr) (list (^tc (cadr expr) bool))))
    ((eq? (car expr) 'seq) (cons (car expr) (list (^tc (cadr expr) bool))))
    ((or (eq? (car expr) 'set) (eq? (car expr) 'set-box) (eq? (car expr) 'def)) (cons (car expr) (cons (cadr expr) (list (^tc (caddr expr) #f)))))
    (else (cons (^tc (car expr) #f) (^tc (cdr expr) bool))))
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IT'S THE FINAL PRO-JECT! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define magic
  (lambda (expr)
    (annotate-tc (pe->lex-pe (box-set (remove-applic-lambda-nil (eliminate-nested-defines (parse expr))))))
    ))

					; Mayer's code to opening a file
(define file->string
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
		(lambda ()
		  (let ((ch (read-char in-port)))
		    (if (eof-object? ch)
			(begin
			  (close-input-port in-port)
			  '())
			(cons ch (run)))))))
	(list->string
	 (run))))))

(define string->file
  (lambda (file str)
    (let ((out (open-output-file file 'replace)))
      (display str out)
      (close-output-port out))
    ))

					; we will open a file with file->string
					; use (test-string <Sexpr> String)
					; the rest is magic

;;
;#define T_VOID 		937610
;#define T_NIL 		722689
;#define T_BOOL 		741553
;#define T_CHAR 		181048
;#define T_INTEGER 	945311
;#define T_STRING 	799345
;#define T_SYMBOL 	368031
;#define T_PAIR 		885397
;#define T_VECTOR 	335728
;#define T_CLOSURE 	276405
;#define T_FRAC 		276409
;;

(define first_const `((1000 ,(void) (937610)) (1001 ,'() (722689)) (1002 #t (741553 1)) (1004 #f (741553 0))))

(define first_vars
  `((4000 append) (4001 apply) (4002 <) (4003 =) (4004 >) (4005 +) (4006 /) (4007 *) (4008 -)
    (4009 boolean?) (4010 car) (4011 cdr) (4012 char->integer) (4013 char?) (4014 cons)
    (4015 denominator) (4016 eq?) (4017 integer?) (4018 integer->char) (4019 list)
    (4020 make-string) (4021 make-vector) (4022 map) (4023 not) (4024 null?) (4025 number?)
    (4026 numerator) (4027 pair?) (4028 procedure?) (4029 rational?) (4030 remainder) (4031 set-car!)
    (4032 set-cdr!) (4033 string-length) (4034 string-ref) (4035 string-set!) (4036 string->symbol)
    (4037 string?) (4038 symbol?) (4039 symbol->string) (4040 vector) (4041 vector-length)
    (4042 vector-ref) (4043 vector-set!) (4044 vector?) (4045 zero?)))

(define bad_vars
  (map (lambda (x) (cadr x)) first_vars))



(define compile-scheme-file
  (lambda (source target)
    (let* ((sstring (file->string source))
	   (sexprs (^sexprs (test-string <Sexpr> sstring)))
	   (optimized (map magic sexprs))
	   (temp_const_list (remove_duplicates (remove_empties (^clist optimized))))
	   (const_list (remove_duplicates (append (fold-left (lambda (x y) (improper->proper (append x y))) '() (map (lambda (x) (remove_duplicates (find_sub x))) temp_const_list)) temp_const_list)))
     (const_table (^const_table (^symbol_strings const_list) first_const 1006)) ; 1000-1006 will be void, nil, true & false
     (var_list (map (lambda (x) (list -1 x)) (filter (lambda (x) (not (member x bad_vars))) (remove_duplicates (remove_empties (^vlist optimized))))))
	   (var_table (^var_table (append first_vars var_list) (+ (car (list-ref const_table (- (length const_table) 1))) (length (caddr (list-ref const_table (- (length const_table) 1)))))))
     (code
	    (string-append (proluge const_table var_table)
			   (apply string-append (map (lambda(x) (string-append (code-gen x const_table var_table 0) (if (eq? 'fvar (car x))
														      (string-append "MOV(R0,IND(R0));\n" (end-code-gen))
														      (end-code-gen))))
						     optimized))
			   epilogue
			   )) 
	   )
     (string->file target code)
      )))

(define ^symbol_strings
  (lambda (lst)
    (if (null? lst)
      lst
      (if (symbol? (car lst))
        (cons (symbol->string (car lst)) (cons (car lst) (^symbol_strings (cdr lst))))
        (cons (car lst) (^symbol_strings (cdr lst)))))
    ))

(define ^sexprs
  (lambda (e)
    (if (eq? (cadadr e) "")
	(list (cadar e))
	(cons (cadar e) (^sexprs (test-string <Sexpr> (cadadr e)))))
    ))

(define ^vlist
  (lambda (expr)
    (cond ((or (null? expr) (not (pair? expr))) '())
	  ((and (pair? (car expr)) (eq? (caar expr) 'fvar)) (cons (cadar expr) (remove_empties (^vlist (cdr expr)))))
	  (else (append (improper->proper (remove_empties (^vlist (car expr)))) (remove_empties (^vlist (cdr expr))))))
    ))

(define ^clist
  (lambda (expr)
    (cond ((or (null? expr) (not (pair? expr))) '())
	  ((and (pair? (car expr)) (eq? (caar expr) 'const)) (if (symbol? (cadar expr))
								 (append (list (symbol->string (cadar expr)) (cadar expr)) (remove_empties (^clist (cdr expr))))
								 (cons (cadar expr) (remove_empties (^clist (cdr expr))))))
	  (else (append (remove_empties (^clist (car expr))) (remove_empties (^clist (cdr expr))))))
    ))

(define find_sub
  (lambda (expr)
    (cond ((null? expr) expr)
          ((vector? expr) (append (apply append (map find_sub (vector->list expr))) (list expr)))
          ((pair? expr) (append (find_sub (car expr)) (find_sub (cdr expr)) (list expr)))
          (else (list expr)))
    ))

;(define find_sub
;  (lambda (expr)
;    (cond ((vector? expr) (append (map find_sub (vector->list expr)) (vector->list expr)))
	;  ((symbol? expr) (list (symbol->string expr) expr))
;	  ((not (pair? expr)) expr)
;	  ((and (pair? (car expr)) (null? (cdr expr))) (append (find_sub (car expr)) (car expr)))
;	  ((and (vector? (car expr)) (null? (cdr expr))) (map find_sub (vector->list (car expr))))
;	  ((and (not (pair? (car expr))) (null? (cdr expr))) (car expr))
;	  ((not (pair? (car expr))) (append (improper->proper (cons (find_sub (car expr)) (find_sub (cdr expr)))) (list (cdr expr))))
;	  (else (append (append (improper->proper (find_sub (car expr))) (list (car expr))) (append (improper->proper (find_sub (list (cdr expr)))) (list (cdr expr))))))
 ;   ))

(define remove_duplicates
  (lambda (lst)
    (if (or (null? lst) (not (pair? lst)))
	lst
	(cons (car lst) (remove_duplicates (remove (car lst) (cdr lst)))))
    ))

					; removes empty lists from lists
(define remove_empties
  (lambda (lst)
    (if (list? lst)
	(filter (lambda (x) (not (null? x))) lst)
	lst)
    ))

(define ^const_table
  (lambda (clist ctable addr)
    (cond ((null? clist) (append ctable clist))
	  ((pair? (car clist)) (^const_table (cdr clist)
					     (append ctable (list (list addr (car clist) `(885397 ,(lookup (caar clist) ctable) ,(lookup (cdar clist) ctable)))))
					     (+ addr 3)))
	  ((and (number? (car clist)) (not (integer? (car clist)))) (^const_table (cdr clist)
										  (append ctable
											  (list (list addr (car clist)
												      `(276409 ,(numerator (car clist)) ,(denominator (car clist))))))
										  (+ addr 3)))
	  ((number? (car clist)) (^const_table (cdr clist)
					       (append ctable (list (list addr (car clist) `(945311 ,(car clist)))))
					       (+ addr 2)))
	  ((vector? (car clist)) (^const_table (cdr clist)
					       (append ctable (list (list addr (car clist) `(335728
											     ,(vector-length (car clist))
											     ,@(map (lambda(x) (lookup x ctable)) (vector->list (car clist)))))))
					       (+ addr (vector-length (car clist)) 2)))
	  ((string? (car clist)) (^const_table (cdr clist)
					       (append ctable (list (list addr (car clist) `(799345 ,(string-length (car clist)) ,@(map char->integer (string->list (car clist)))))))
					       (+ addr (string-length (car clist)) 2)))
	  ((char? (car clist)) (^const_table (cdr clist)
					     (append ctable (list (list addr (car clist) `(181048 ,(char->integer (car clist))))))
					     (+ addr 2)))
	  ((symbol? (car clist)) (^const_table (cdr clist)
					       (append ctable (list (list addr (car clist) `(368031 ,(lookup (symbol->string (car clist)) ctable)))))
					       (+ addr 2)))
	  ((boolean? (car clist)) (^const_table (cdr clist) ctable addr))
	  ((eq? (car clist) `,(void)) (^const_table (cdr clist) ctable addr))
	  (else (begin (display "ERROR!!!!! ") (display (car clist)) (newline) 'closure)))
    ))

(define ^var_table
  (lambda (vlist addr)
    (if (null? vlist)
	vlist
	(cons (list addr (cadar vlist) (caar vlist)) (^var_table (cdr vlist) (+ addr 1))))
    ))


(define lookup
  (lambda (exp table)
    (if (or (eq? exp (cadar table))
	    (and (number? exp)(number? (cadar table)) (= exp (cadar table)))
	    (and (list? exp) (list? (cadar table)) (list-eq? exp (cadar table)))
	    (and (string? exp) (string? (cadar table)) (string=? exp (cadar table)))
	    (and (vector? exp) (vector? (cadar table)) (list-eq? (vector->list exp) (vector->list (cadar table)))))
	(caar table)
	(lookup exp (cdr table)))
    ))


(define code-gen
  (lambda (expr ctable vtable l_count)
    (cond ((or (not (list? expr)) (null? expr)) "")
	  ((eq? (car expr) 'if3) (code-gen-if3 (cdr expr) ctable vtable l_count))
	  ((eq? (car expr) 'seq) (code-gen-seq (cadr expr) ctable vtable l_count))
	  ((eq? (car expr) 'or) (code-gen-or (cadr expr) ctable vtable l_count))
	  ((eq? (car expr) 'applic) (code-gen-applic (cdr expr) ctable vtable l_count))
	  ((eq? (car expr) 'pvar) (code-gen-pvar (cdr expr)))
	  ((eq? (car expr) 'set) (code-gen-set (cdr expr) ctable vtable l_count))
	  ((eq? (car expr) 'box-get) (code-gen-box-get (cadr expr) ctable vtable l_count))
	  ((eq? (car expr) 'box-set) (code-gen-box-set (cdr expr) ctable vtable l_count))
	  ((eq? (car expr) 'bvar) (code-gen-bvar (cdr expr)))
	  ((eq? (car expr) 'fvar) (code-gen-fvar (cadr expr) vtable))
	  ((eq? (car expr) 'const) (code-gen-const (cadr expr) ctable vtable))
	  ((eq? (car expr) 'lambda-simple) (code-gen-lambda-simple (cdr expr) ctable vtable l_count))
	  ((eq? (car expr) 'lambda-opt) (code-gen-lambda-opt (cdr expr) ctable vtable l_count))
	  ((eq? (car expr) 'lambda-var) (code-gen-lambda-var (cdr expr) ctable vtable l_count))
	  ((eq? (car expr) 'tc-applic) (code-gen-tc-applic (cdr expr) ctable vtable l_count))
	  ((eq? (car expr) 'def) (code-gen-def (cdr expr) ctable vtable l_count))
	  ((eq? (car expr) 'box) (code-gen-box (cdr expr) ctable vtable l_count))
	  (else (display "ERROR! ") (display expr) (newline))
	  )
    ))

(define code-gen-box
  (lambda (expr ctable vtable l_count)
    (string-append (code-gen (car expr) ctable vtable l_count)
		   "MOV(R1,R0);
PUSH(IMM(1));
CALL(MALLOC);
DROP(1);
MOV(IND(R0),R1);\n")
    ))

(define code-gen-def
  (lambda (expr ctable vtable l_count)
    (let ((var (code-gen (car expr) ctable vtable l_count))
	  (val (code-gen (cadr expr) ctable vtable l_count)))
      (string-append var
		     "PUSH(R0);\n"
		     val
		     "POP(R1);
MOV(IND(R1),R0);
MOV(R0,IMM(1000));\n"))
    ))
					; assuming #f is in 1004!!!

(define if_count 0)

(define code-gen-if3
  (lambda (expr ctable vtable l_count)
    (let ((if_label if_count)
	  (test (code-gen (car expr) ctable vtable l_count))
	  (dit (code-gen (cadr expr) ctable vtable l_count))
	  (dif (code-gen (caddr expr) ctable vtable l_count)))
      (set! if_count (+ if_count 1))
      (string-append test
		     "CMP(R0,IMM(1004));
JUMP_EQ(LIF_ELSE_"
		     (number->string if_label)
		     ");\n"
		     dit
		     "JUMP(LIF_EXIT_"
		     (number->string if_label)
		     ");
LIF_ELSE_"
		     (number->string if_label)
		     ":\n"
		     dif
		     "LIF_EXIT_"
		     (number->string if_label)
		     ":\n"
		     ))
    ))

(define code-gen-const
  (lambda (expr ctable vtable)
    (string-append "MOV(R0,IMM(" (number->string (lookup expr ctable)) "));\n")
    ))

(define code-gen-seq
  (lambda (expr ctable vtable l_count)
    (if (null? expr)
	""
	(string-append (code-gen (car expr) ctable vtable l_count)
		       (if (eq? (caar expr) 'fvar)
			   "MOV(R0,IND(R0));\n"
			   "")
		       (code-gen-seq (cdr expr) ctable vtable l_count)))
    ))


(define or_count 1)

(define code-gen-or
  (lambda (expr ctable vtable l_count)
    (let ((or_label or_count))
      (if (null? expr)
	  (begin (set! or_count (+ or_count 1)) (string-append  "LOR_EXIT_" (number->string or_label) ":\n"))
	  (string-append (code-gen (car expr) ctable vtable l_count) "CMP(R0,IMM(1004));\nJUMP_NE(LOR_EXIT_" (number->string or_label) ");\n" (code-gen-or (cdr expr) ctable vtable l_count)))
      )
    ))

(define code-gen-applic
  (lambda (expr ctable vtable l_count)
    (let* ((rev (reverse (cadr expr)))
	   (m (length rev)))
      (string-append (apply string-append (map (lambda (x) (string-append (code-gen x ctable vtable l_count) 
                                                                          (if (eq? 'fvar (car x)) "PUSH(IND(R0));\n" "PUSH(R0);\n"))) rev))
		     "PUSH(IMM(" (number->string m) "));\n"
		     (code-gen (car expr) ctable vtable l_count)
		     (if (eq? 'fvar (caar expr))
			 "MOV(R0,IND(R0));"
			 "")
"CMP(IND(R0),IMM(T_CLOSURE));
JUMP_NE(LERROR_CANNOT_APPLY_NON_CLOSURE);
PUSH(INDD(R0,1));
CALLA(INDD(R0,2));
DROP(1);
POP(R1);
DROP(R1);\n")
      )
    ))

(define code-gen-pvar
  (lambda (expr)
    (string-append "MOV(R0,FPARG(" (number->string (+ 2 (cadr expr))) "));\n")
    ))

(define code-gen-set
  (lambda (expr ctable vtable l_count)
    (let ((var
	   (cond ((eq? (caar expr) 'pvar) (string-append "MOV(FPARG(" (number->string (+ 2 (caddar expr))) "), R1);\n"))
		 ((eq? (caar expr) 'bvar) (string-append "MOV(R0, FPARG(0));\nMOV(R0, INDD(R0, " (number->string (caddar expr)) "));\nMOV(INDD(R0, " (number->string (car (cdddar expr))) "), R1);\n"))
		 ((eq? (caar expr) 'fvar) (string-append "MOV(R0, IMM(" (number->string (lookup (cadar expr) vtable)) "));\nMOV(IND(R0), R1);\n"))
		 )))
      (string-append (code-gen (cadr expr) ctable vtable l_count)
		     (if (eq? (caadr expr) 'fvar)
			 "MOV(R0,IND(R0));\n"
			 "")
		     "MOV(R1,R0);\n" var "MOV(R0,IMM(1000));\n"))
    ))

(define code-gen-box-get
  (lambda (expr ctable vtable l_count)
    (cond ((eq? (car expr) 'pvar) (string-append "MOV(R0,FPARG(" (number->string (+ 2 (caddr expr))) "));\nMOV(R0,IND(R0));\n"))
	  ((eq? (car expr) 'bvar) (string-append "MOV(R0,FPARG(0));\nMOV(R0, INDD(R0, " (number->string (caddr expr)) "));\nMOV(R0,INDD(R0, " (number->string (cadddr expr)) "));\nMOV(R0, IND(R0));\n"))
	  ((eq? (car expr) 'fvar) (string-append "MOV(R0, IMM(" (number->string (lookup (cadr expr) vtable)) "));\nMOV(R0, IND(R0));\n")))
    ))

(define code-gen-box-set
  (lambda (expr ctable vtable l_count)
    (let ((var
	   (cond ((eq? (caar expr) 'pvar) (string-append "MOV(R0,FPARG("(number->string (+ 2 (caddar expr)))"));\n"))
		 ((eq? (caar expr) 'bvar) (string-append "MOV(R0, FPARG(0));\nMOV(R0, INDD(R0, " (number->string (caddar expr)) "));\nMOV(R0, INDD(R0, " (number->string (car (cdddar expr))) "));\n"))
		 ((eq? (caar expr) 'fvar) (string-append "MOV(R0, IMM(" (number->string (lookup (cadar expr) vtable)) "));\nMOV(R0, IND(R0));\n"))
		 )))
      (string-append (code-gen (cadr expr) ctable vtable l_count) "MOV(R1,R0);\n" var "MOV(IND(R0),R1);\nMOV(R0,IMM(1000));\n"))
    ))

(define code-gen-bvar
  (lambda (expr)
    (string-append "MOV(R0,FPARG(0));\nMOV(R0, INDD(R0," (number->string (cadr expr)) "));\nMOV(R0, INDD(R0," (number->string (caddr expr)) "));\n")
    ))

(define code-gen-fvar
  (lambda (expr vtable)
    (string-append "MOV(R0, IMM(" (number->string (lookup expr vtable)) "));\n")
    ))




(define lambda_count 0)

(define code-gen-lambda-simple
  (lambda (expr ctable vtable l_count)
    (set! lambda_count (+ lambda_count 1))
    (let ((label lambda_count))
      (string-append "MOV(R1,FPARG(0));
PUSH(IMM("
		     (number->string (+ l_count 1))
		     "));
CALL(MALLOC);
DROP(1);
MOV(R2,R0);
MOV(R3,IMM(0));
MOV(R4,IMM(1));
LAMBDA_LOOP_"
		     (number->string label)
		     ":
CMP(R3,IMM("
		     (number->string l_count)
		     "));
JUMP_GE(AFTER_LOOP_"
		     (number->string label)
		     ");
MOV(INDD(R2,R4),INDD(R1,R3));
INCR(R3);
INCR(R4);
JUMP(LAMBDA_LOOP_"
		     (number->string label)
		     ");
AFTER_LOOP_"
		     (number->string label)
		     ":
MOV(R3,FPARG(1));
PUSH(R3);
CALL(MALLOC);
DROP(1);
MOV(IND(R2),R0);
MOV(R4, IMM(0));
MOV(R5, IMM(2));
MOV(R6,IND(R2));
LAMBDA_DEEP_LOOP_"
		     (number->string label)
		     ":
CMP(R4, R3);
JUMP_GE(AFTER_DEEP_LOOP_"
		     (number->string label)
		     ");
MOV(INDD(R6,R4),FPARG(R5));
INCR(R4);
INCR(R5);
JUMP(LAMBDA_DEEP_LOOP_"
		     (number->string label)
		     ");
AFTER_DEEP_LOOP_"
		     (number->string label)
		     ":
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(IND(R0), IMM(T_CLOSURE));
MOV(INDD(R0,1), R2);
MOV(INDD(R0,2), LABEL(LCLOS_BODY_"
		     (number->string label)
		     "));
JUMP(LCLOS_EXIT_"
		     (number->string label)
		     ");
LCLOS_BODY_"
		     (number->string label)
		     ":
PUSH(FP);
MOV(FP,SP);
CMP(FPARG(1),IMM("
		     (number->string (length (car expr)))
		     "));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);\n"
		     (code-gen (cadr expr) ctable vtable (+ l_count 1))
		     "POP(FP);
		     RETURN;
LCLOS_EXIT_"
		     (number->string label)
		     ":\n"))
    ))

(define code-gen-lambda-opt
  (lambda (expr ctable vtable l_count)
    (set! lambda_count (+ lambda_count 1))
    (let ((label lambda_count))
      (string-append "MOV(R1,FPARG(0));
PUSH(IMM("
		     (number->string (+ l_count 1))
		     "));
CALL(MALLOC);
DROP(1);
MOV(R2,R0);
MOV(R3,IMM(0));
MOV(R4,IMM(1));
LAMBDA_LOOP_"
		     (number->string label)
		     ":
CMP(R3,IMM("
		     (number->string l_count)
		     "));
JUMP_GE(AFTER_LOOP_"
		     (number->string label)
		     ");
MOV(INDD(R2,R4),INDD(R1,R3));
INCR(R3);
INCR(R4);
JUMP(LAMBDA_LOOP_"
		     (number->string label)
		     ");
AFTER_LOOP_"
		     (number->string label)
		     ":
MOV(R3,FPARG(1));
PUSH(R3);
CALL(MALLOC);
DROP(1);
MOV(IND(R2),R0);
MOV(R4, IMM(0));
MOV(R5, IMM(2));
MOV(R6,IND(R2));
LAMBDA_DEEP_LOOP_"
		     (number->string label)
		     ":
CMP(R4, R3);
JUMP_GE(AFTER_DEEP_LOOP_"
		     (number->string label)
		     ");
MOV(INDD(R6,R4),FPARG(R5));
INCR(R4);
INCR(R5);
JUMP(LAMBDA_DEEP_LOOP_"
		     (number->string label)
		     ");
AFTER_DEEP_LOOP_"
		     (number->string label)
		     ":
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(IND(R0), IMM(T_CLOSURE));
MOV(INDD(R0,1), R2);
MOV(INDD(R0,2), LABEL(LCLOS_BODY_"
		     (number->string label)
		     "));
JUMP(LCLOS_EXIT_"
		     (number->string label)
		     ");
LCLOS_BODY_"
		     (number->string label)
		     ":
PUSH(FP);
MOV(FP,SP);
CMP(FPARG(1),IMM("
		     (number->string (length (car expr)))
		     "));
JUMP_LT(LERROR_INCORRECT_NUM_OF_ARGS);
JUMP_EQ(LLAMBDA_OPT_MAKE_NIL"
		     (number->string label)
		     ");
MOV(R7,IMM(1001));
PUSH(R7);
MOV(R8,FPARG(1));
INCR(R8);
LOPT_PUSHING_LIST"
		     (number->string label)
		     ":
CMP(R8,IMM("
		     (number->string (+ (length (car expr)) 1))
		     "));
JUMP_LE(LOPT_AFTER_PUSH"
		     (number->string label)
		     ");
PUSH(FPARG(R8));
CALL(MAKE_SOB_PAIR);
DROP(2);
PUSH(R0);
DECR(R8);
JUMP(LOPT_PUSHING_LIST"
		     (number->string label)
		     ");
LLAMBDA_OPT_MAKE_NIL"
		     (number->string label)
		     ":
MOV(R7,IMM(-2));
MOV(R8,IMM(-3));
LLAMBDA_OPT_NIL_LOOP"
		     (number->string label)
		     ":
CMP(R7,IMM("
		     (number->string (+ (length (car expr)) 2))
		     "));
JUMP_EQ(LLAMBDA_OPT_AFTER_NIL"
		     (number->string label)
		     ");
MOV(FPARG(R8),FPARG(R7));
INCR(R7);
INCR(R8);
JUMP(LLAMBDA_OPT_NIL_LOOP"
		     (number->string label)
		     ");

LLAMBDA_OPT_AFTER_NIL"
		     (number->string label)
		     ":
MOV(FPARG(R8),IMM(1001));
INCR(FP);
INCR(SP);
JUMP(LLAMBDA_OPT_FINISH"
		     (number->string label)
		     ");

LOPT_AFTER_PUSH"
		     (number->string label)
		     ":
POP(R8);
MOV(FPARG("
		     (number->string (+ (length (car expr)) 2))
		     "), R8);
MOV(FPARG("
		     (number->string (+ (length (car expr)) 3))
		     "),IMM(1001));

LLAMBDA_OPT_FINISH"
		     (number->string label)
		     ":\n"
		     (code-gen (caddr expr) ctable vtable (+ l_count 1))
		     "POP(FP);
		     RETURN;
LCLOS_EXIT_"
		     (number->string label)
		     ":\n"))
    ))

(define code-gen-lambda-var
  (lambda (expr ctable vtable l_count)
    (code-gen-lambda-opt (cons '() expr) ctable vtable l_count)
    ))

(define tc_count 0)

(define code-gen-tc-applic
  (lambda (expr ctable vtable l_count)
    (set! tc_count (+ tc_count 1))
    (let* ((rev (reverse! (cadr expr)))
	   (m (length rev))
	   (tc_label tc_count))
      (string-append (apply string-append (map (lambda (x) (string-append (code-gen x ctable vtable l_count) "PUSH(R0);\n")) rev))
		     "PUSH(IMM(" (number->string m) "));\n"
		     (code-gen (car expr) ctable vtable l_count)
		     (if (eq? 'fvar (caar expr))
			 "MOV(R0,IND(R0));"
			 "")
		     "CMP(IND(R0),IMM(T_CLOSURE));
 JUMP_NE(LERROR_CANNOT_APPLY_NON_CLOSURE);
PUSH(INDD(R0,1));
PUSH(FPARG(-1));
MOV(R1, FPARG(-2));

MOV(R4,FP);
MOV(R2,FP);
MOV(R3,FPARG(1));
SUB(R4,R3);
SUB(R4,IMM(4));
CMP(STACK(R4),IMM(1001));
JUMP_EQ(LTC_APP_LABEL"
		     (number->string tc_label)
		     ");
DECR(R4);
JUMP(LTC_APP_LOOP"
		     (number->string tc_label)
		     ");
LTC_APP_LABEL"
		     (number->string tc_label)
		     ":
DECR(R4);
CMP(STACK(R4),IMM(1001));
JUMP_EQ(LTC_APP_LOOP"
		     (number->string tc_label)
		     ");
INCR(R4);
LTC_APP_LOOP"
		     (number->string tc_label)
		     ":
CMP(R2,SP);
JUMP_EQ(LTC_APP_END"
		     (number->string tc_label)
		     ");
MOV(STACK(R4),STACK(R2));
INCR(R2);
INCR(R4);
JUMP(LTC_APP_LOOP"
		     (number->string tc_label)
		     ");
LTC_APP_END"
		     (number->string tc_label)
		     ":
MOV(SP,R4);

MOV(FP,R1);
JUMPA(INDD(R0,2));\n
")
      )
    ))

(define proluge
  (lambda (ctable vtable)
    (string-append "#include <stdio.h>
#include <stdlib.h>

/* change to 0 for no debug info to be printed: */
#define DO_SHOW 0

#include \"cisc.h\"

int main()
{
 START_MACHINE;
 int const_table [] = {"
(apply string-append (map (lambda(x) (if (number? x) (number->string x) (if (char? x) (make-string 1 x) ""))) (flatten (cons (cdar ctable) (map (lambda (x) (map (lambda (y) (list #\, y)) (caddr x))) (cdr ctable))))))
"};
int arraySize = sizeof(const_table)/sizeof(int);
int arrayCounter, memCounter;
for (arrayCounter = 0, memCounter = 1000 ; arrayCounter < arraySize ; ++arrayCounter, ++memCounter) {
machine->mem[memCounter] = const_table[arrayCounter];
}
PUSH(arraySize+999);
CALL(MALLOC);
DROP(1);

int var_table [] = {"
(apply string-append (map (lambda(x) (if (number? x) (number->string x) (if (char? x) (make-string 1 x) ""))) (flatten (cons (caddar vtable) (map (lambda (x) (list #\, (caddr x))) (cdr vtable))))))
"};
arraySize = sizeof(var_table)/sizeof(int);
for (arrayCounter = 0; arrayCounter < arraySize ; ++arrayCounter, ++memCounter){
machine->mem[memCounter] = var_table[arrayCounter];
}
PUSH(arraySize);
CALL(MALLOC);
DROP(1);

machine->mem[100] = 1001; // our symbols linked list!

 JUMP(CONTINUE);

 #include \"char.lib\"
 #include \"io.lib\"
 #include \"math.lib\"
 #include \"string.lib\"
 #include \"system.lib\"
 #include \"scheme.lib\"

 CONTINUE:
JUMP(LmakePlusClos);

LplusBody:
PUSH(FP);
MOV(FP, SP);
XOR(R0,R0);
MOV(R0, FPARG(1));
CMP(R0,IMM(0));
JUMP_NE(PLUS_MORE_THAN_ONE);
PUSH(IMM(0));
CALL(MAKE_SOB_INTEGER);
DROP(1);
JUMP(PLUS_EXIT);

PLUS_MORE_THAN_ONE:
PUSH(IMM(1));
PUSH(IMM(0));
CALL(MAKE_SOB_FRAC);
DROP(2);
MOV(R3,R0);
MOV(R1,FPARG(1));
INCR(R1);
PLUS_PROC_LOOP:
CMP(R1,IMM(2));
JUMP_LT(PLUS_FINISH_LOOP);
PUSH(R1);
MOV(R2,FPARG(R1));
PUSH(R2);
CALL(IS_SOB_FRAC);
DROP(1);
CMP(R0,IMM(1));
JUMP_EQ(ADDING);
PUSH(R2);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(0));
JUMP_EQ(LERROR_NOT_A_NUMBER);
PUSH(IMM(1));
PUSH(INDD(R2,1));
CALL(MAKE_SOB_FRAC);
DROP(2);
MOV(R2,R0);

ADDING:
MOV(R4, INDD(R2,2));
MOV(R5, INDD(R3,1));
MUL(R4,R5);
MOV(R6, INDD(R2,1));
MOV(R5, INDD(R3,2));
MUL(R5,R6);
ADD(R4,R5);
MOV(R5, INDD(R3,2));
MOV(R6, INDD(R2,2));
MUL(R5,R6);
PUSH(R5);
PUSH(R4);
CALL(MAKE_SOB_FRAC);
DROP(2);
MOV (R3, R0);
POP(R1);
DECR(R1);
JUMP(PLUS_PROC_LOOP);

PLUS_FINISH_LOOP:
CMP(INDD(R3,2),IMM(1));
JUMP_NE(PLUS_EXIT);
PUSH(INDD(R3,1));
CALL(MAKE_SOB_INTEGER);
DROP(1);
MOV(R3,R0);

PLUS_EXIT:
POP(FP);
RETURN;

LmakePlusClos:
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10002));
MOV(INDD(R0, 2), LABEL(LplusBody));
MOV(IND("
(number->string (lookup '+ vtable))
"), R0);

JUMP(LmakeMinusClos);

LminusBody:
PUSH(FP);
MOV(FP, SP);
XOR(R0,R0);
MOV(R0, FPARG(1));
CMP(R0,IMM(0));
JUMP_EQ(LERROR_INCORRECT_NUM_OF_ARGS);
CMP(R0, IMM(1));
JUMP_NE(MINUS_MORE_THAN_ONE);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_FRAC);
DROP(1);
CMP(R0,IMM(1));
JUMP_EQ(MINUS_ONE_IS_FRAC);
PUSH(R1);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_NUMBER);
MOV(R3,INDD(R1,1))
MUL(R3,IMM(-1));
PUSH(R3);
CALL(MAKE_SOB_INTEGER);
DROP(1);
JUMP(MINUS_EXIT);

MINUS_ONE_IS_FRAC:
MOV(R3,INDD(R1,1));
MUL(R3,IMM(-1));
PUSH(INDD(R1,2));
PUSH(R3);
CALL(MAKE_SOB_FRAC);
DROP(2);
JUMP(MINUS_EXIT);

MINUS_MORE_THAN_ONE:
MOV(R3, FPARG(2));
PUSH(R3);
CALL(IS_SOB_FRAC);
POP(R3);
CMP(R0,IMM(0));
JUMP_NE(MINUS_PRE_LOOP);
PUSH(R3);
CALL(IS_SOB_INTEGER);
POP(R3);
CMP(R0,IMM(0));
JUMP_EQ(LERROR_NOT_A_NUMBER);
PUSH(IMM(1));
PUSH(INDD(R3,1));
CALL(MAKE_SOB_FRAC);
DROP(2);
MOV(R3,R0);

MINUS_PRE_LOOP:
MOV(R1, FPARG(1));
ADD(R1,IMM(2));
MOV(R2,IMM(3));

MINUS_PROC_LOOP:
CMP(R2,R1);
JUMP_EQ(MINUS_FINISH_LOOP);
PUSH(R1);
PUSH(R2);
MOV(R4,FPARG(R2));
PUSH(R4);
CALL(IS_SOB_FRAC);
DROP(1);
CMP(R0,IMM(1));
JUMP_EQ(SUBSTRACTING);
PUSH(R4);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_NUMBER);
PUSH(IMM(1));
PUSH(INDD(R4,1));
CALL(MAKE_SOB_FRAC);
DROP(2);
MOV(R4,R0);

SUBSTRACTING:
MOV(R5, INDD(R4,2));
MOV(R6, INDD(R3,1));
MUL(R5,R6);
MOV(R6, INDD(R4,1));
MOV(R7, INDD(R3,2));
MUL(R6,R7);
SUB(R5,R6);
MOV(R6, INDD(R4,2));
MUL(R6,R7);
PUSH(R6);
PUSH(R5);
CALL(MAKE_SOB_FRAC);
DROP(2);
MOV (R3, R0);
POP(R2);
INCR(R2);
POP(R1);
JUMP(MINUS_PROC_LOOP);

MINUS_FINISH_LOOP:
CMP(INDD(R3,2),IMM(1));
JUMP_NE(MINUS_EXIT);
PUSH(INDD(R3,1));
CALL(MAKE_SOB_INTEGER);
DROP(1);

MINUS_EXIT:
POP(FP);
RETURN;

LmakeMinusClos:
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10042));
MOV(INDD(R0, 2), LABEL(LminusBody));
MOV(IND("
(number->string (lookup '- vtable))
"), R0);\n
JUMP(LmakeMultiplyClos);


JUMP(LmakeMultiplyClos);

LmultiplyBody:
PUSH(FP);
MOV(FP, SP);
XOR(R0,R0);
MOV(R0, FPARG(1));
CMP(R0,IMM(0));
JUMP_NE(MULTIPLY_MORE_THAN_ONE);
PUSH(IMM(1));
CALL(MAKE_SOB_INTEGER);
DROP(1);
JUMP(MULTIPLY_EXIT);

MULTIPLY_MORE_THAN_ONE:
PUSH(IMM(1));
PUSH(IMM(1));
CALL(MAKE_SOB_FRAC);
DROP(2);
MOV(R3,R0);
MOV(R1,FPARG(1));
INCR(R1);

MULTIPLY_PROC_LOOP:
CMP(R1,IMM(2));
JUMP_LT(MULTIPLY_FINISH_LOOP);
PUSH(R1);
MOV(R2,FPARG(R1));
PUSH(R2);
CALL(IS_SOB_FRAC);
DROP(1);
CMP(R0,IMM(1));
JUMP_EQ(MULTIPLYING);
PUSH(R2);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_NUMBER);
PUSH(IMM(1));
PUSH(INDD(R2,1));
CALL(MAKE_SOB_FRAC);
DROP(2);
MOV(R2,R0);

MULTIPLYING:
MOV(R4, INDD(R2,1));
MOV(R5, INDD(R3,1));
MUL(R4,R5);
MOV(R5, INDD(R2,2));
MOV(R6, INDD(R3,2));
MUL(R5,R6);
PUSH(R5);
PUSH(R4);
CALL(MAKE_SOB_FRAC);
DROP(2);
MOV (R3, R0);
POP(R1);
DECR(R1);
JUMP(MULTIPLY_PROC_LOOP);

MULTIPLY_FINISH_LOOP:
CMP(INDD(R3,2),IMM(1));
JUMP_NE(MULTIPLY_EXIT);
PUSH(INDD(R3,1));
CALL(MAKE_SOB_INTEGER);
DROP(1);

MULTIPLY_EXIT:
POP(FP);
RETURN;

LmakeMultiplyClos:
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10044));
MOV(INDD(R0, 2), LABEL(LmultiplyBody));
MOV(IND("
(number->string (lookup '* vtable))
"), R0);\n

JUMP(LmakeDivideClos);

LdivideBody:
PUSH(FP);
MOV(FP, SP);
XOR(R0,R0);
MOV(R0, FPARG(1));
CMP(R0,IMM(0));
JUMP_EQ(LERROR_INCORRECT_NUM_OF_ARGS);
CMP(R0,IMM(1));
JUMP_NE(DIVIDE_MORE_THAN_ONE);

MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_FRAC);
DROP(1);
CMP(R0,IMM(1));
JUMP_EQ(DIVIDE_ONE_IS_FRAC);
PUSH(INDD(R1,1));
PUSH(IMM(1));
CALL(MAKE_SOB_FRAC);
DROP(2);
JUMP(DIVIDE_EXIT);

DIVIDE_ONE_IS_FRAC:
PUSH(INDD(R1,1));
PUSH(INDD(R1,2));
CALL(MAKE_SOB_FRAC);
DROP(2);
MOV(R3,R0);
JUMP(DIVIDE_FINISH_LOOP);

DIVIDE_MORE_THAN_ONE:
MOV(R1,FPARG(1));
INCR(R1);
MOV(R2,IMM(3));
MOV(R3,FPARG(2));
PUSH(R1);
PUSH(R2);
PUSH(R3);
CALL(IS_SOB_FRAC);
POP(R3);
POP(R2);
POP(R1);
CMP(R0,IMM(0));
JUMP_NE(DIVIDE_PROC_LOOP);
PUSH(R3);
CALL(IS_SOB_INTEGER);
POP(R3);
CMP(R0,IMM(0));
JUMP_EQ(LERROR_NOT_A_NUMBER);
PUSH(R1);
PUSH(R2);
PUSH(IMM(1));
PUSH(INDD(R3,1));
CALL(MAKE_SOB_FRAC);
DROP(2);
POP(R2);
POP(R1);
MOV(R3,R0);

DIVIDE_PROC_LOOP:
CMP(R2,R1);
JUMP_GT(DIVIDE_FINISH_LOOP);
PUSH(R1);
PUSH(R2);
MOV(R4,FPARG(R2));
PUSH(R4);
CALL(IS_SOB_FRAC);
DROP(1);
CMP(R0,IMM(1));
JUMP_EQ(DIVIDING);
PUSH(R4);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_NUMBER);
PUSH(IMM(1));
PUSH(INDD(R4,1));
CALL(MAKE_SOB_FRAC);
DROP(2);
MOV(R4,R0);

DIVIDING:
MOV(R5, INDD(R3,1));
MOV(R6, INDD(R4,2));
MUL(R5,R6);
MOV(R6, INDD(R3,2));
MOV(R7, INDD(R4,1));
MUL(R6,R7);
PUSH(R6);
PUSH(R5);
CALL(MAKE_SOB_FRAC);
DROP(2);
MOV (R3, R0);
POP(R2);
INCR(R2);
POP(R1);
JUMP(DIVIDE_PROC_LOOP);

DIVIDE_FINISH_LOOP:
CMP(INDD(R3,2),IMM(1));
JUMP_NE(DIVIDE_EXIT);
PUSH(INDD(R3,1));
CALL(MAKE_SOB_INTEGER);
DROP(1);

DIVIDE_EXIT:
POP(FP);
RETURN;

LmakeDivideClos:
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10043));
MOV(INDD(R0, 2), LABEL(LdivideBody));
MOV(IND("
(number->string (lookup '/ vtable))
"), R0);\n

JUMP(LmakeGreaterClos);

LGreaterBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(0));
JUMP_EQ(LERROR_INCORRECT_NUM_OF_ARGS);
CMP(R2,IMM(1));
JUMP_NE(GREATER_MORE_THAN_ONE);
PUSH(FPARG(2));
CALL(IS_SOB_NUMBER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_NUMBER);
MOV(R0,IMM(1002));
JUMP(GREATER_EXIT);

GREATER_MORE_THAN_ONE:
MOV(R1,IMM(3));
ADD(R2,IMM(2));
MOV(R3,FPARG(2));
PUSH(R3);
CALL(IS_SOB_FRAC);
DROP(1);
CMP(R0,IMM(1));
JUMP_EQ(GREATER_LOOP);
PUSH(R3);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(0));
JUMP_EQ(LERROR_NOT_A_NUMBER);
PUSH(R2);
PUSH(R1);
PUSH(IMM(1));
PUSH(INDD(R3,1));
CALL(MAKE_SOB_FRAC);
DROP(2);
MOV(R3,R0);
POP(R1);
POP(R2);


GREATER_LOOP:
CMP(R1,R2);
JUMP_EQ(GREATER_FINISH_LOOP);
PUSH(R2);
PUSH(R1);
MOV(R4,FPARG(R1));
PUSH(R4);
CALL(IS_SOB_FRAC);
POP(R4);
CMP(R0,IMM(0));
JUMP_NE(GREATER_BOTH_FRAC);
PUSH(R4);
CALL(IS_SOB_INTEGER);
POP(R4);
CMP(R0,IMM(0));
JUMP_EQ(LERROR_NOT_A_NUMBER);
PUSH(R3);
PUSH(IMM(1));
PUSH(INDD(R4,1));
CALL(MAKE_SOB_FRAC);
DROP(2);
POP(R3);
MOV(R4,R0);

GREATER_BOTH_FRAC:
POP(R1);
POP(R2);
MOV(R5,INDD(R3,1));
MOV(R6,INDD(R4,1));
MUL(R5,INDD(R4,2));
MUL(R6,INDD(R3,2));
CMP(R5,R6);
JUMP_GT(GREATER_CONTINUE_LOOP);
MOV(R0,IMM(1004));
JUMP(GREATER_EXIT);

GREATER_CONTINUE_LOOP:
INCR(R1);
MOV(R3,R4);
JUMP(GREATER_LOOP);

GREATER_FINISH_LOOP:
MOV(R0,IMM(1002));

GREATER_EXIT:
POP(FP);
RETURN;

LmakeGreaterClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10005));
MOV(INDD(R0, 2), LABEL(LGreaterBody));
MOV(IND("
(number->string (lookup '> vtable))
"), R0);\n

JUMP(LmakeLowerClos);

LLowerBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(0));
JUMP_EQ(LERROR_INCORRECT_NUM_OF_ARGS);
CMP(R2,IMM(1));
JUMP_NE(LOWER_MORE_THAN_ONE);
PUSH(FPARG(2));
CALL(IS_SOB_NUMBER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_NUMBER);
MOV(R0,IMM(1002));
JUMP(LOWER_EXIT);

LOWER_MORE_THAN_ONE:
MOV(R1,IMM(3));
ADD(R2,IMM(2));
MOV(R3,FPARG(2));
PUSH(R3);
CALL(IS_SOB_FRAC);
DROP(1);
CMP(R0,IMM(1));
JUMP_EQ(LOWER_LOOP);
PUSH(R3);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(0));
JUMP_EQ(LERROR_NOT_A_NUMBER);
PUSH(R2);
PUSH(R1);
PUSH(IMM(1));
PUSH(INDD(R3,1));
CALL(MAKE_SOB_FRAC);
DROP(2);
MOV(R3,R0);
POP(R1);
POP(R2);


LOWER_LOOP:
CMP(R1,R2);
JUMP_EQ(LOWER_FINISH_LOOP);
PUSH(R2);
PUSH(R1);
MOV(R4,FPARG(R1));
PUSH(R4);
CALL(IS_SOB_FRAC);
POP(R4);
CMP(R0,IMM(0));
JUMP_NE(LOWER_BOTH_FRAC);
PUSH(R4);
CALL(IS_SOB_INTEGER);
POP(R4);
CMP(R0,IMM(0));
JUMP_EQ(LERROR_NOT_A_NUMBER);
PUSH(R3);
PUSH(IMM(1));
PUSH(INDD(R4,1));
CALL(MAKE_SOB_FRAC);
DROP(2);
POP(R3);
MOV(R4,R0);

LOWER_BOTH_FRAC:
POP(R1);
POP(R2);
MOV(R5,INDD(R3,1));
MOV(R6,INDD(R4,1));
MUL(R5,INDD(R4,2));
MUL(R6,INDD(R3,2));
CMP(R5,R6);
JUMP_LT(LOWER_CONTINUE_LOOP);
MOV(R0,IMM(1004));
JUMP(LOWER_EXIT);

LOWER_CONTINUE_LOOP:
INCR(R1);
MOV(R3,R4);
JUMP(LOWER_LOOP);

LOWER_FINISH_LOOP:
MOV(R0,IMM(1002));

LOWER_EXIT:
POP(FP);
RETURN;

LmakeLowerClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10005));
MOV(INDD(R0, 2), LABEL(LLowerBody));
MOV(IND("
(number->string (lookup '< vtable))
"), R0);\n

JUMP(LmakeShaveClos);

LShaveBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(0));
JUMP_EQ(LERROR_INCORRECT_NUM_OF_ARGS);
CMP(R2,IMM(1));
JUMP_NE(LSHAVE_MORE_THAN_ONE);
PUSH(FPARG(2));
CALL(IS_SOB_NUMBER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_NUMBER);
MOV(R0,IMM(1002));
JUMP(SHAVE_EXIT);

LSHAVE_MORE_THAN_ONE:
MOV(R1,IMM(3));
ADD(R2,IMM(2));
MOV(R3,FPARG(2));
PUSH(R3);
CALL(IS_SOB_FRAC);
DROP(1);
CMP(R0,IMM(1));
JUMP_EQ(SHAVE_LOOP);
PUSH(R3);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(0));
JUMP_EQ(LERROR_NOT_A_NUMBER);
PUSH(R2);
PUSH(R1);
PUSH(IMM(1));
PUSH(INDD(R3,1));
CALL(MAKE_SOB_FRAC);
DROP(2);
MOV(R3,R0);
POP(R1);
POP(R2);


SHAVE_LOOP:
CMP(R1,R2);
JUMP_EQ(SHAVE_FINISH_LOOP);
PUSH(R2);
PUSH(R1);
MOV(R4,FPARG(R1));
PUSH(R4);
CALL(IS_SOB_FRAC);
POP(R4);
CMP(R0,IMM(0));
JUMP_NE(SHAVE_BOTH_FRAC);
PUSH(R4);
CALL(IS_SOB_INTEGER);
POP(R4);
CMP(R0,IMM(0));
JUMP_EQ(LERROR_NOT_A_NUMBER);
PUSH(R3);
PUSH(IMM(1));
PUSH(INDD(R4,1));
CALL(MAKE_SOB_FRAC);
DROP(2);
POP(R3);
MOV(R4,R0);

SHAVE_BOTH_FRAC:
POP(R1);
POP(R2);
MOV(R5,INDD(R3,1));
MOV(R6,INDD(R4,1));
MUL(R5,INDD(R4,2));
MUL(R6,INDD(R3,2));
CMP(R5,R6);
JUMP_EQ(SHAVE_CONTINUE_LOOP);
MOV(R0,IMM(1004));
JUMP(SHAVE_EXIT);

SHAVE_CONTINUE_LOOP:
INCR(R1);
MOV(R3,R4);
JUMP(SHAVE_LOOP);

SHAVE_FINISH_LOOP:
MOV(R0,IMM(1002));

SHAVE_EXIT:
POP(FP);
RETURN;

LmakeShaveClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10005));
MOV(INDD(R0, 2), LABEL(LShaveBody));
MOV(IND("
(number->string (lookup '= vtable))
"), R0);\n

JUMP(LmakeCarClos);

LcarBody:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1, FPARG(2));
CMP(INDD(R1, 0), IMM(T_PAIR));
JUMP_NE(LERROR_INCORRECT_TYPE);
MOV(R0, INDD(R1, 1));
POP(FP);
RETURN;

LmakeCarClos:
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10000));
MOV(INDD(R0, 2), LABEL(LcarBody));
 MOV(IND("
(number->string (lookup 'car vtable))
"), R0);\n

JUMP(LmakeCdrClos);

LcdrBody:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1, FPARG(2));
CMP(INDD(R1, 0), IMM(T_PAIR));
JUMP_NE(LERROR_INCORRECT_TYPE);
MOV(R0, INDD(R1, 2));
POP(FP);
RETURN;

LmakeCdrClos:
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10001));
MOV(INDD(R0, 2), LABEL(LcdrBody));
MOV(IND("
(number->string (lookup 'cdr vtable))
"), R0);\n


JUMP(LmakeIsBooleanClos);

LIsBooleanBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R0,FPARG(2));
PUSH(R0);
CALL(IS_SOB_BOOL);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(IS_BOOLEAN_NOT_BOOLEAN);
MOV(R0,IMM(1002));
JUMP(IS_BOOLEAN_EXIT);

IS_BOOLEAN_NOT_BOOLEAN:
MOV(R0,IMM(1004));

IS_BOOLEAN_EXIT:
POP(FP);
RETURN;

LmakeIsBooleanClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10006));
MOV(INDD(R0, 2), LABEL(LIsBooleanBody));
MOV(IND("
(number->string (lookup 'boolean? vtable))
"), R0);\n


JUMP(LmakeIsCharClos);

LIsCharBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R0,FPARG(2));
PUSH(R0);
CALL(IS_SOB_CHAR);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(IS_CHAR_NOT_CHAR);
MOV(R0,IMM(1002));
JUMP(IS_CHAR_EXIT);

IS_CHAR_NOT_CHAR:
MOV(R0,IMM(1004));

IS_CHAR_EXIT:
POP(FP);
RETURN;

LmakeIsCharClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10007));
MOV(INDD(R0, 2), LABEL(LIsCharBody));
MOV(IND("
(number->string (lookup 'char? vtable))
"), R0);\n


JUMP(LmakeIsZeroClos);

LIsZeroBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_NUMBER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_NUMBER);
CMP(INDD(R1,1),IMM(0));
JUMP_NE(IS_ZERO_NOT_ZERO);
MOV(R0,IMM(1002));
JUMP(IS_ZERO_EXIT);

IS_ZERO_NOT_ZERO:
MOV(R0,IMM(1004));

IS_ZERO_EXIT:
POP(FP);
RETURN;

LmakeIsZeroClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10040));
MOV(INDD(R0, 2), LABEL(LIsZeroBody));
MOV(IND("
(number->string (lookup 'zero? vtable))
"), R0);\n


JUMP(LmakeIsVectorClos);
LIsVectorBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R0,FPARG(2));
PUSH(R0);
CALL(IS_SOB_VECTOR);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(IS_VECTOR_NOT_VECTOR);
MOV(R0,IMM(1002));
JUMP(IS_VECTOR_EXIT);

IS_VECTOR_NOT_VECTOR:
MOV(R0,IMM(1004));

IS_VECTOR_EXIT:
POP(FP);
RETURN;

LmakeIsVectorClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10039));
MOV(INDD(R0, 2), LABEL(LIsVectorBody));
MOV(IND("
(number->string (lookup 'vector? vtable))
"), R0);\n


JUMP(LmakeIsSymbolClos);

LIsSymbolBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R0,FPARG(2));
PUSH(R0);
CALL(IS_SOB_SYMBOL);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(IS_SYMBOL_NOT_SYMBOL);
MOV(R0,IMM(1002));
JUMP(IS_SYMBOL_EXIT);

IS_SYMBOL_NOT_SYMBOL:
MOV(R0,IMM(1004));

IS_SYMBOL_EXIT:
POP(FP);
RETURN;

LmakeIsSymbolClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10033));
MOV(INDD(R0, 2), LABEL(LIsSymbolBody));
MOV(IND("
(number->string (lookup 'symbol? vtable))
"), R0);\n


JUMP(LmakeIsStringClos);

LIsStringBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R0,FPARG(2));
PUSH(R0);
CALL(IS_SOB_STRING);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(IS_STRING_NOT_STRING);
MOV(R0,IMM(1002));
JUMP(IS_STRING_EXIT);

IS_STRING_NOT_STRING:
MOV(R0,IMM(1004));

IS_STRING_EXIT:
POP(FP);
RETURN;

LmakeIsStringClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10032));
MOV(INDD(R0, 2), LABEL(LIsStringBody));
MOV(IND("
(number->string (lookup 'string? vtable))
"), R0);\n


JUMP(LmakeIsRationalClos);

LIsRationalBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_NUMBER);
DROP(1);
CMP(R0,IMM(1));
JUMP_EQ(IS_RATIONAL_MAKE_TRUE);
MOV(R0,IMM(1004));
JUMP(IS_RATIONAL_EXIT);

IS_RATIONAL_MAKE_TRUE:
MOV(R0,IMM(1002));

IS_RATIONAL_EXIT:
POP(FP);
RETURN;

LmakeIsRationalClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10024));
MOV(INDD(R0, 2), LABEL(LIsRationalBody));
MOV(IND("
(number->string (lookup 'rational? vtable))
"), R0);\n


JUMP(LmakeIsProcedureClos);

LIsProcedureBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_CLOSURE);
DROP(1);
CMP(R0,IMM(1));
JUMP_EQ(IS_PROCEDURE_MAKE_TRUE);
MOV(R0,IMM(1004));
JUMP(IS_PROCEDURE_EXIT);

IS_PROCEDURE_MAKE_TRUE:
MOV(R0,IMM(1002));

IS_PROCEDURE_EXIT:
POP(FP);
RETURN;

LmakeIsProcedureClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10023));
MOV(INDD(R0, 2), LABEL(LIsProcedureBody));
MOV(IND("
(number->string (lookup 'procedure? vtable))
"), R0);\n


JUMP(LmakeIsPairClos);

LIsPairBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_PAIR);
DROP(1);
CMP(R0,IMM(1));
JUMP_EQ(IS_PAIR_MAKE_TRUE);
MOV(R0,IMM(1004));
JUMP(IS_PAIR_EXIT);

IS_PAIR_MAKE_TRUE:
MOV(R0,IMM(1002));

IS_PAIR_EXIT:
POP(FP);
RETURN;

LmakeIsPairClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10022));
MOV(INDD(R0, 2), LABEL(LIsPairBody));
MOV(IND("
(number->string (lookup 'pair? vtable))
"), R0);\n


JUMP(LmakeIsNumberClos);

LIsNumberBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_NUMBER);
DROP(1);
CMP(R0,IMM(1));
JUMP_EQ(IS_NUMBER_MAKE_TRUE);
CALL(IS_SOB_FRAC);
CMP(R0,IMM(1));
JUMP_EQ(IS_NUMBER_MAKE_TRUE);
MOV(R0,IMM(1004));
JUMP(IS_NUMBER_EXIT);

IS_NUMBER_MAKE_TRUE:
MOV(R0,IMM(1002));

IS_NUMBER_EXIT:
POP(FP);
RETURN;

LmakeIsNumberClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10020));
MOV(INDD(R0, 2), LABEL(LIsNumberBody));
MOV(IND("
(number->string (lookup 'number? vtable))
"), R0);\n


JUMP(LmakeIsNullClos);

LIsNullBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_NIL);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(IS_NULL_MAKE_FALSE);
MOV(R0,IMM(1002));
JUMP(IS_NULL_EXIT);

IS_NULL_MAKE_FALSE:
MOV(R0,IMM(1004));

IS_NULL_EXIT:
POP(FP);
RETURN;

LmakeIsNullClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10019));
MOV(INDD(R0, 2), LABEL(LIsNullBody));
MOV(IND("
(number->string (lookup 'null? vtable))
"), R0);\n


JUMP(LmakeIsIntegerClos);

LIsIntegerBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R0,FPARG(2));
PUSH(R0);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(IS_INTEGER_NOT_INTEGER);
MOV(R0,IMM(1002));
JUMP(IS_INTEGER_EXIT);

IS_INTEGER_NOT_INTEGER:
MOV(R0,IMM(1004));

IS_INTEGER_EXIT:
POP(FP);
RETURN;

LmakeIsIntegerClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10012));
MOV(INDD(R0, 2), LABEL(LIsIntegerBody));
MOV(IND("
(number->string (lookup 'integer? vtable))
"), R0);\n


JUMP(LmakeIsEQClos);

LIsEQBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(2));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
MOV(R2,FPARG(3));
CMP(IND(R1),IND(R2));
JUMP_NE(IS_EQ_NOT_EQUAL);
CALL(IS_SOB_INTEGER);
CMP(R0,IMM(1));
JUMP_NE(IS_EQ_NOT_INTEGER);
CMP(INDD(R1,1),INDD(R2,1));
JUMP_EQ(IS_EQ_ARE_EQUAL);
JUMP(IS_EQ_NOT_EQUAL);

IS_EQ_NOT_INTEGER:
CALL(IS_SOB_FRAC);
CMP(R0,IMM(1));
JUMP_NE(IS_EQ_NOT_FRACTION);
CMP(INDD(R1,1),INDD(R2,1));
JUMP_NE(IS_EQ_NOT_EQUAL);
CMP(INDD(R1,2),INDD(R2,2));
JUMP_EQ(IS_EQ_ARE_EQUAL);
JUMP(IS_EQ_NOT_EQUAL);

IS_EQ_NOT_FRACTION:
CALL(IS_SOB_CHAR);
CMP(R0,IMM(1));
JUMP_NE(IS_EQ_NOT_CHAR);
CMP(INDD(R1,1),INDD(R2,1));
JUMP_EQ(IS_EQ_ARE_EQUAL);
JUMP(IS_EQ_NOT_EQUAL);

IS_EQ_NOT_CHAR:
CALL(IS_SOB_SYMBOL);
CMP(R0,IMM(1));
JUMP_NE(IS_EQ_COMPARE_OBJECTS);
CMP(INDD(R1,1),INDD(R2,1));
JUMP_EQ(IS_EQ_ARE_EQUAL);
JUMP(IS_EQ_NOT_EQUAL);

IS_EQ_COMPARE_OBJECTS:
CMP(R1,R2);
JUMP_EQ(IS_EQ_ARE_EQUAL);

IS_EQ_NOT_EQUAL:
MOV(R0,IMM(1004));
JUMP(IS_EQ_EXIT);

IS_EQ_ARE_EQUAL:
MOV(R0,IMM(1002));

IS_EQ_EXIT:
DROP(1);
POP(FP);
RETURN;

LmakeIsEQClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10011));
MOV(INDD(R0, 2), LABEL(LIsEQBody));
MOV(IND("
(number->string (lookup 'eq? vtable))
"), R0);\n

JUMP(LmakeVectorSetClos);
LVectorSetBody:
PUSH(FP);
MOV(FP, SP);
MOV(R1,FPARG(1));
CMP(R1,IMM(3));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_VECTOR);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_INCORRECT_TYPE);
MOV(R2,FPARG(3));
PUSH(R2);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_AN_INTEGER);
MOV(R3,FPARG(4));

CMP(INDD(R2,1),IMM(0));
JUMP_LT(LERROR_INVALID_INPUT);
MOV(R0,INDD(R1,1));
CMP(INDD(R2,1),R0);
JUMP_GE(LERROR_INVALID_INPUT);
MOV(R2,INDD(R2,1));
ADD(R2,IMM(2));

MOV(INDD(R1,R2),R3);
CALL(MAKE_SOB_VOID);
POP(FP);
RETURN;

LmakeVectorSetClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10038));
MOV(INDD(R0, 2), LABEL(LVectorSetBody));
MOV(IND("
(number->string (lookup 'vector-set! vtable))
"), R0);\n


JUMP(LmakeVectorRefClos);

LVectorRefBody:
PUSH(FP);
MOV(FP, SP);
MOV(R1,FPARG(1));
CMP(R1,IMM(2));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_VECTOR);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_INCORRECT_TYPE);
MOV(R2,FPARG(3));
PUSH(R2);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_NUMBER);
CMP(INDD(R2,1),IMM(0));
JUMP_LT(LERROR_INVALID_INPUT);
MOV(R0,INDD(R1,1));
CMP(INDD(R2,1),R0);
JUMP_GE(LERROR_INVALID_INPUT);
MOV(R2,INDD(R2,1));
ADD(R2,IMM(2));
MOV(R0,INDD(R1,R2));
POP(FP);
RETURN;

LmakeVectorRefClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10037));
MOV(INDD(R0, 2), LABEL(LVectorRefBody));
MOV(IND("
(number->string (lookup 'vector-ref vtable))
"), R0);\n

JUMP(LmakeVectorLengthClos);

LvectorLengthBody:
PUSH(FP);
MOV(FP, SP);
XOR(R0,R0);
MOV(R0, FPARG(1));
CMP(R0,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_VECTOR);
DROP(1);
CMP(R0,IMM(0));
JUMP_EQ(LERROR_INCORRECT_TYPE);
PUSH(INDD(R1,1));
CALL(MAKE_SOB_INTEGER);
DROP(1);
POP(FP);
RETURN;

LmakeVectorLengthClos:
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10036));
MOV(INDD(R0, 2), LABEL(LvectorLengthBody));
MOV(IND("
(number->string (lookup 'vector-length vtable))
"), R0);\n

JUMP(LmakeVectorClos);

LvectorBody:
PUSH(FP);
MOV(FP, SP);
MOV(R1, FPARG(1));
ADD(R1,IMM(2));
MOV(R2,IMM(2));

Make_Vector_PUSH_ARGS_LOOP:
CMP(R2,R1);
JUMP_EQ(Make_Vector_FINISH_LOOP);
PUSH(FPARG(R2));
INCR(R2);
JUMP(Make_Vector_PUSH_ARGS_LOOP);

Make_Vector_FINISH_LOOP:
PUSH(FPARG(1));
CALL(MAKE_SOB_VECTOR);
POP(R1);
DROP(R1);
POP(FP);
RETURN;

LmakeVectorClos:
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10035));
MOV(INDD(R0, 2), LABEL(LvectorBody));
MOV(IND("
(number->string (lookup 'vector vtable))
"), R0);\n


JUMP(LmakeSymbolToStringClos);

LSymbolToStringBody:
PUSH(FP);
MOV(FP, SP);
MOV(R1,FPARG(1));
CMP(R1,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_SYMBOL);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_INCORRECT_TYPE);
MOV(R0,INDD(R1,1));
POP(FP);
RETURN;

LmakeSymbolToStringClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10034));
MOV(INDD(R0, 2), LABEL(LSymbolToStringBody));
MOV(IND("
(number->string (lookup 'symbol->string vtable))
"), R0);\n

JUMP(LmakeStringToSymbolClos);

LStringToSymbolBody:
PUSH(FP);
MOV(FP, SP);
MOV(R1,FPARG(1));
CMP(R1,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_STRING);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_INCORRECT_TYPE);
MOV(R2,IMM(100));
PUSH(IND(R2));
CALL(IS_SOB_NIL);
DROP(1);
CMP(R0,IMM(1));
JUMP_EQ(STS_EMPTY_LIST);

CHECK_LIST_LOOP:
MOV(R3,IND(R2));
PUSH(R2);
PUSH(R3);
PUSH(R1);
PUSH(IMM(2));
PUSH(IMM(0));
CALL(LIsEQBody);
DROP(1);
POP(R3);
DROP(R3);
POP(R2);
CMP(INDD(R0,1),IMM(1));
JUMP_EQ(STS_CREATE_SYMBOL);
MOV(R3,R2);
MOV(R3,INDD(R3,1));
MOV(R1,FPARG(2));
CMP(R3,IMM(1001));
JUMP_EQ(CREATE_NODE);
MOV(R2,R3);
JUMP(CHECK_LIST_LOOP);

STS_EMPTY_LIST:
MOV(IND(R2),FPARG(2));
MOV(INDD(R2,1),IMM(1001));
MOV(R1,FPARG(2));
JUMP(STS_CREATE_SYMBOL);

CREATE_NODE:
PUSH(IMM(2));
CALL(MALLOC);
DROP(1);
MOV(INDD(R2,1),R0);
MOV(IND(R0),R1);
MOV(INDD(R0,1),IMM(1001));
MOV(R1,IND(R0));

STS_CREATE_SYMBOL:
PUSH(R1);
CALL(MAKE_SOB_SYMBOL);
DROP(1);
POP(FP);
RETURN;

LmakeStringToSymbolClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10031));
MOV(INDD(R0, 2), LABEL(LStringToSymbolBody));
MOV(IND("
(number->string (lookup 'string->symbol vtable))
"), R0);\n



JUMP(LmakeStringSetClos);

LStringSetBody:
PUSH(FP);
MOV(FP, SP);
MOV(R1,FPARG(1));
CMP(R1,IMM(3));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_STRING);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_INCORRECT_TYPE);
MOV(R2,FPARG(3));
PUSH(R2);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_NUMBER);
MOV(R3,FPARG(4));
PUSH(R3);
CALL(IS_SOB_CHAR);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_CHAR);
CMP(INDD(R2,1),IMM(0));
JUMP_LT(LERROR_INVALID_INPUT);
MOV(R0,INDD(R1,1));
CMP(INDD(R2,1),R0);
JUMP_GE(LERROR_INVALID_INPUT);
MOV(R2,INDD(R2,1));
ADD(R2,IMM(2));

MOV(R3,INDD(R3,1));
MOV(INDD(R1,R2),R3);
CALL(MAKE_SOB_VOID);
POP(FP);
RETURN;

LmakeStringSetClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10030));
MOV(INDD(R0, 2), LABEL(LStringSetBody));
MOV(IND("
(number->string (lookup 'string-set! vtable))
"), R0);\n

JUMP(LmakeStringRefClos);

LStringRefBody:
PUSH(FP);
MOV(FP, SP);
MOV(R1,FPARG(1));
CMP(R1,IMM(2));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_STRING);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_INCORRECT_TYPE);
MOV(R2,FPARG(3));
PUSH(R2);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_AN_INTEGER);
CMP(INDD(R2,1),IMM(0));
JUMP_LT(LERROR_INVALID_INPUT);
MOV(R0,INDD(R1,1));
CMP(INDD(R2,1),R0);
JUMP_GE(LERROR_INVALID_INPUT);
MOV(R2,INDD(R2,1));
ADD(R2,IMM(2));
MOV(R0,INDD(R1,R2));
PUSH(R0);
CALL(MAKE_SOB_CHAR);
DROP(1);
POP(FP);
RETURN;

LmakeStringRefClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10029));
MOV(INDD(R0, 2), LABEL(LStringRefBody));
MOV(IND("
(number->string (lookup 'string-ref vtable))
"), R0);\n


JUMP(LmakeStringLengthClos);

LStringLengthBody:
PUSH(FP);
MOV(FP, SP);
MOV(R1,FPARG(1));
CMP(R1,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_STRING);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_INCORRECT_TYPE);
MOV(R0,INDD(R1,1));
PUSH(R0);
CALL(MAKE_SOB_INTEGER);
DROP(1);
POP(FP);
RETURN;

LmakeStringLengthClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10028));
MOV(INDD(R0, 2), LABEL(LStringLengthBody));
MOV(IND("
(number->string (lookup 'string-length vtable))
"), R0);\n


JUMP(LmakeSetCdrClos);

LSetCdrBody:
PUSH(FP);
MOV(FP, SP);
MOV(R1,FPARG(1));
CMP(R1,IMM(2));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_PAIR);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_INCORRECT_TYPE);
MOV(R2,FPARG(3));
MOV(INDD(R1,2),R2);
CALL(MAKE_SOB_VOID);
POP(FP);
RETURN;

LmakeSetCdrClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10027));
MOV(INDD(R0, 2), LABEL(LSetCdrBody));
MOV(IND("
(number->string (lookup 'set-cdr! vtable))
"), R0);\n


JUMP(LmakeSetCarClos);

LSetCarBody:
PUSH(FP);
MOV(FP, SP);
MOV(R1,FPARG(1));
CMP(R1,IMM(2));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_PAIR);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_INCORRECT_TYPE);
MOV(R2,FPARG(3));
MOV(INDD(R1,1),R2);
CALL(MAKE_SOB_VOID);
POP(FP);
RETURN;

LmakeSetCarClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10026));
MOV(INDD(R0, 2), LABEL(LSetCarBody));
MOV(IND("
(number->string (lookup 'set-car! vtable))
"), R0);\n


JUMP(LmakeRemainderClos);

LRemainderBody:
PUSH(FP);
MOV(FP, SP);
MOV(R1,FPARG(1));
CMP(R1,IMM(2));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_AN_INTEGER);
MOV(R2,FPARG(3));
PUSH(R2);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_AN_INTEGER);
MOV(R0,INDD(R1,1));
REM(R0,INDD(R2,1));
PUSH(R0);
CALL(MAKE_SOB_INTEGER);
DROP(1);
POP(FP);
RETURN;

LmakeRemainderClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10025));
MOV(INDD(R0, 2), LABEL(LRemainderBody));
MOV(IND("
(number->string (lookup 'remainder vtable))
"), R0);\n

JUMP(LmakeNumeratorClos);

LNumeratorBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
PUSH(FPARG(2));
CALL(IS_SOB_FRAC);
CMP(R0,IMM(1));
JUMP_EQ(IS_FRAC);
CALL(IS_SOB_INTEGER);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_NUMBER);

IS_FRAC:
MOV(R1,FPARG(2));
PUSH(INDD(R1,1));

EXIT:
CALL(MAKE_SOB_INTEGER);
DROP(2);
POP(FP);
RETURN;

LmakeNumeratorClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10021));
MOV(INDD(R0, 2), LABEL(LNumeratorBody));
MOV(IND("
(number->string (lookup 'numerator vtable))
"), R0);\n

JUMP(LmakeNotClos);

LNotBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_BOOL);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(NOT_MAKE_FALSE);
CMP(INDD(R1,1),IMM(0));
JUMP_NE(NOT_MAKE_FALSE);
MOV(R0,IMM(1002));
JUMP(NOT_EXIT);

NOT_MAKE_FALSE:
MOV(R0,IMM(1004));

NOT_EXIT:
POP(FP);
RETURN;

LmakeNotClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10018));
MOV(INDD(R0, 2), LABEL(LNotBody));
MOV(IND("
(number->string (lookup 'not vtable))
"), R0);\n


JUMP(LmakeMapClos);

LMapBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(2));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));

PUSH(R1);
CALL(IS_SOB_CLOSURE);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_CLOSURE);
MOV(R2,FPARG(3))
CMP(R2,IMM(1001));
JUMP_EQ(MAP_EMPTY_LIST);
PUSH(R2);
CALL(IS_LIST);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_LIST);
XOR(R3,R3);

MAP_APPLIC_LOOP:
PUSH(R1);
PUSH(R2);
PUSH(R3);
PUSH(INDD(R2,1));
PUSH(IMM(1));
PUSH(INDD(R1,1));
CALLA(INDD(R1,2));
DROP(1);
POP(R4);
DROP(R4);
POP(R3);
POP(R2);
POP(R1);
PUSH(R0);
INCR(R3);
MOV(R2,INDD(R2,2));
CMP(R2,IMM(1001));
JUMP_NE(MAP_APPLIC_LOOP);
PUSH(IMM(1001));

MAP_MAKE_LIST_LOOP:
POP(R5);
POP(R6);
PUSH(R5);
PUSH(R6);
CALL(MAKE_SOB_PAIR);
DROP(2);
PUSH(R0);
DECR(R3);
CMP(R3,IMM(0));
JUMP_NE(MAP_MAKE_LIST_LOOP);
POP(R0);
JUMP(MAP_EXIT);

MAP_EMPTY_LIST:
MOV(R0,IMM(1001));

MAP_EXIT:
POP(FP);
RETURN;

LmakeMapClos:
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10017));
MOV(INDD(R0, 2), LABEL(LMapBody));
MOV(IND("
(number->string (lookup 'map vtable))
"), R0);\n

JUMP(LmakeMakeVectorClos);

LMakeVectorBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_EQ(MAKE_VECTOR_NO_ARGS);
CMP(R2,IMM(2));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_NUMBER);
MOV(R2,FPARG(3))

MAKE_VECTOR:
MOV(R3,INDD(R1,1));

MAKE_VECTOR_LOOP:
CMP(R3,IMM(0));
JUMP_EQ(MAKE_VECTOR_FINISH_LOOP);
PUSH(R2);
DECR(R3);
JUMP(MAKE_VECTOR_LOOP);

MAKE_VECTOR_NO_ARGS:
MOV(R1,FPARG(2));
PUSH(IMM(0));
CALL(MAKE_SOB_INTEGER);
DROP(1);
MOV(R2,R0);
JUMP(MAKE_VECTOR);

MAKE_VECTOR_FINISH_LOOP:
PUSH(INDD(R1,1));
CALL(MAKE_SOB_VECTOR);
POP(R1);
DROP(R1);
POP(FP);
RETURN;

LmakeMakeVectorClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10016));
MOV(INDD(R0, 2), LABEL(LMakeVectorBody));
MOV(IND("
(number->string (lookup 'make-vector vtable))
"), R0);\n

JUMP(LmakeStringClos);

LStringBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_EQ(MAKE_STRING_NO_ARG);
CMP(R2,IMM(2));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);

MOV(R2,FPARG(3))
PUSH(R2);
CALL(IS_SOB_CHAR);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_CHAR);
JUMP(MAKE_STRING_CHECK_CHAR);

MAKE_STRING_NO_ARG:
PUSH(IMM(0));
CALL(MAKE_SOB_CHAR);
DROP(1);
MOV(R2,R0);

MAKE_STRING_CHECK_CHAR:
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_NUMBER);

MAKE_STRING:
MOV(R3,INDD(R1,1));

MAKE_STRING_LOOP:
CMP(R3,IMM(0));
JUMP_EQ(MAKE_STRING_FINISH_LOOP);
PUSH(INDD(R2,1));
DECR(R3);
JUMP(MAKE_STRING_LOOP);

MAKE_STRING_FINISH_LOOP:
PUSH(INDD(R1,1));
CALL(MAKE_SOB_STRING);
POP(R1);
DROP(R1);
POP(FP);
RETURN;

LmakeStringClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10015));
MOV(INDD(R0, 2), LABEL(LStringBody));
MOV(IND("
(number->string (lookup 'make-string vtable))
"), R0);\n

JUMP(LmakeListClos);

LListBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2, FPARG(1));
PUSH(IMM(1001));
INCR(R2);

MAKE_LIST_LOOP:
CMP(R2, IMM(1));
JUMP_EQ(MAKE_LIST_FINISH_LOOP);
PUSH(FPARG(R2));
CALL(MAKE_SOB_PAIR);
DROP(2);
PUSH(R0);
DECR(R2);
JUMP(MAKE_LIST_LOOP);

MAKE_LIST_FINISH_LOOP:
POP(R0);
POP(FP);
RETURN;

LmakeListClos:
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10014));
MOV(INDD(R0, 2), LABEL(LListBody));
MOV(IND("
(number->string (lookup 'list vtable))
"), R0);\n

JUMP(LmakeIntToCharClos);

LIntToCharBody:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1, FPARG(2));
PUSH(R1);
CALL(IS_SOB_INTEGER);
POP(R1);
CMP(R0, IMM(1));
JUMP_NE(LERROR_INCORRECT_TYPE);
PUSH(INDD(R1, 1));
CALL(MAKE_SOB_CHAR);
DROP(1);
POP(FP);
RETURN;

LmakeIntToCharClos:
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10013));
MOV(INDD(R0, 2), LABEL(LIntToCharBody));
MOV(IND("
(number->string (lookup 'integer->char vtable))
"), R0);\n

JUMP(LmakeDenominatorClos);

LDenominatorBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
PUSH(FPARG(2));
CALL(IS_SOB_FRAC);
CMP(R0,IMM(1));
JUMP_EQ(DENOMINATOR_IS_FRAC);
CALL(IS_SOB_INTEGER);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_NUMBER);
PUSH(IMM(1));
JUMP(DENOMINATOR_EXIT);

DENOMINATOR_IS_FRAC:
MOV(R1,FPARG(2));
PUSH(INDD(R1,2));

DENOMINATOR_EXIT:
CALL(MAKE_SOB_INTEGER);
DROP(2);
POP(FP);
RETURN;

LmakeDenominatorClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10010));
MOV(INDD(R0, 2), LABEL(LDenominatorBody));
MOV(IND("
(number->string (lookup 'denominator vtable))
"), R0);\n


JUMP(LmakeConsClos);

LConsBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(2));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
PUSH(FPARG(3));
PUSH(FPARG(2));
CALL(MAKE_SOB_PAIR);
DROP(2);
POP(FP);
RETURN;

LmakeConsClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10009));
MOV(INDD(R0, 2), LABEL(LConsBody));
MOV(IND("
(number->string (lookup 'cons vtable))
"), R0);\n


JUMP(LmakeCharToIntClos);

LCharToIntBody:
PUSH(FP);
MOV(FP, SP);
MOV(R2,FPARG(1));
CMP(R2,IMM(1));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1,FPARG(2));
PUSH(R1);
CALL(IS_SOB_CHAR);
POP(R1);
CMP(R0,IMM(1));
JUMP_NE(LERROR_NOT_A_CHAR);
PUSH(INDD(R1,1));
CALL(MAKE_SOB_INTEGER);
DROP(1);
POP(FP);
RETURN;

LmakeCharToIntClos:
PUSH(3);
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10007));
MOV(INDD(R0, 2), LABEL(LCharToIntBody));
MOV(IND("
(number->string (lookup 'char->integer vtable))
"), R0);\n

JUMP(LmakeAppendClos);
LappendBody:
PUSH(FP);
MOV(FP, SP);
MOV(R1, FPARG(1));
CMP(R1,IMM(0));
JUMP_EQ(APPEND_EMPTY_LIST);
ADD(R1,IMM(1));
MOV(R2,IMM(2));
XOR(R8,R8);

APPEND_LOOP:
CMP(R1,R2);
JUMP_EQ(APPEND_FINISH_LOOP);
PUSH(FPARG(R2));
CALL(IS_LIST);
DROP(1);
CMP(R0,IMM(1));
JUMP_EQ(APPEND_PRE_LOOP);
CMP(FPARG(R2),IMM(1001));
JUMP_NE(LERROR_INCORRECT_TYPE);
INCR(R2);
JUMP(APPEND_LOOP);

APPEND_PRE_LOOP:
MOV(R3,FPARG(R2));

APPEND_PUSH_LOOP:
PUSH(INDD(R3,1));
INCR(R8);
CMP(INDD(R3,2),IMM(1001));
JUMP_EQ(APPEND_WITH_NEXT);
MOV(R3,INDD(R3,2));
JUMP(APPEND_PUSH_LOOP);

APPEND_WITH_NEXT:
INCR(R2);
JUMP(APPEND_LOOP);

APPEND_FINISH_LOOP:
PUSH(FPARG(R1));

APPEND_PAIRING_LOOP:
CMP(R8,IMM(0));
JUMP_EQ(APPEND_FINISH_PLOOP);
POP(R5);
POP(R6);
PUSH(R5);
PUSH(R6);
CALL(MAKE_SOB_PAIR);
DROP(2);
PUSH(R0);
DECR(R8);
JUMP(APPEND_PAIRING_LOOP);

APPEND_FINISH_PLOOP:
POP(R0);
JUMP(APPEND_EXIT);

APPEND_EMPTY_LIST:
MOV(R0,IMM(1001));

APPEND_EXIT:
POP(FP);
RETURN;

LmakeAppendClos:
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10041));
MOV(INDD(R0, 2), LABEL(LappendBody));
MOV(IND("
(number->string (lookup 'append vtable))
"), R0);\n


JUMP(LmakeApplyClos);

LapplyBody:
PUSH(FP);
MOV(FP, SP);
MOV(R1, FPARG(1));
CMP(R1, IMM(2));
JUMP_LT(LERROR_INCORRECT_NUM_OF_ARGS);
INCR(R1);
XOR(R9,R9);

PUSH(FPARG(R1));
CALL(IS_LIST);
DROP(1);
CMP(R0,IMM(0));
JUMP_NE(APPLY_LAST_ARG_IS_LIST);
JUMP(APPLY_PUSHING_SMALL_LOOP);
//
//	JUMP TO PUSH ALL OTHER ARGS
//

APPLY_LAST_ARG_IS_LIST:
XOR(R8,R8);
MOV(R7,FPARG(R1));

APPLY_LOOP_SIZE:
CMP(R7,IMM(1001));
JUMP_EQ(APPLY_GOT_SIZE);
INCR(R8);
MOV(R7,INDD(R7,2));
JUMP(APPLY_LOOP_SIZE);

APPLY_GOT_SIZE:
DECR(R8);
XOR(R6,R6);
MOV(R7,FPARG(R1));

APPLY_PUSHING_BIG_LOOP:
CMP(R6,R8);
JUMP_EQ(APPLY_PUSH_FROM_LIST);
INCR(R6);
MOV(R7,INDD(R7,2));
JUMP(APPLY_PUSHING_BIG_LOOP);

APPLY_PUSH_FROM_LIST:
PUSH(INDD(R7,1));
INCR(R9);
CMP(R8,IMM(0));
JUMP_NE(APPLY_GOT_SIZE);

APPLY_PUSHING_SMALL_LOOP:
DECR(R1);
CMP(R1,IMM(2));
JUMP_EQ(APPLY_FINISH_LOOP);
PUSH(FPARG(R1));
INCR(R9);
JUMP(APPLY_PUSHING_SMALL_LOOP);

APPLY_FINISH_LOOP:
MOV(R0,FPARG(2));
CMP(IND(R0),IMM(T_CLOSURE));
JUMP_NE(LERROR_NOT_A_CLOSURE);
PUSH(R9);
PUSH(INDD(R0,1));
CALLA(INDD(R0,2));
DROP(1);
POP(R9);
DROP(R9);

POP(FP);
RETURN;


LmakeApplyClos:
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), IMM(T_CLOSURE));
MOV(INDD(R0, 1), IMM(10035));
MOV(INDD(R0, 2), LABEL(LapplyBody));
MOV(IND("
(number->string (lookup 'apply vtable))
"), R0);\n\n\n\n
/******************************************************************************************************************************************/
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// ~ * ~ * ~                               Actual code starts here! Enjoy you compiled code!                                    ~ * ~ * ~ //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/******************************************************************************************************************************************/
\n\n\n\n
")
))

(define print_label 0)

(define end-code-gen
  (lambda ()
    (set! print_label (+ print_label 1))
  (string-append
  "PUSH(R0);
CALL(IS_SOB_VOID);
CMP(R0,IMM(0));
POP(R0);
JUMP_NE(DO_NOT_PRINT_"
  (number->string print_label)
  ");
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
DO_NOT_PRINT_"
  (number->string print_label)
  ":\n")
  ))

(define epilogue
  "END:
  STOP_MACHINE;
  return 0;

LERROR_INCORRECT_NUM_OF_ARGS:
  printf(\"Error! Incorrect number of args...\\n\");
  JUMP(END);

LERROR_NOT_AN_INTEGER:
  printf(\"Error! Not an integer...\\n\");
  JUMP(END);

LERROR_SECOND_ARG_CANNOT_BE_ZERO:
  printf(\"Error! Second arg cannot be zero...\\n\");
  JUMP(END);

LERROR_NOT_A_LIST:
  printf(\"Error! Not a list...\\n\");
  JUMP(END);

LERROR_NOT_A_NUMBER:
  printf(\"Error! Not a number...\\n\");
  JUMP(END);

LERROR_NOT_A_CHAR:
  printf(\"Error! Not a char...\\n\");
  JUMP(END);

LERROR_INCORRECT_TYPE:
  printf(\"Error! Incorrect type...\\n\");
  JUMP(END);

LERROR_INVALID_INPUT:
  printf(\"Error! Invalid input...\\n\");
  JUMP(END);

LERROR_NOT_A_CLOSURE:
  printf(\"Error! Not a closure...\\n\");
  JUMP(END);

LERROR_CANNOT_APPLY_NON_CLOSURE:
  printf(\"Error! Cannot apply non closure...\\n\");
  JUMP(END);
}"
)

(define list-eq?
  (lambda (a b)
    (if (or (not (list? a)) (not (list? b)))
	#f
	(if (not (= (length a) (length b)))
	    #f
	    (if (or (eq? (car a) (car b)) (and (number? (car a)) (number? (car b)) (= (car a) (car b))) (and (list? (car a)) (list? (car b)) (list-eq? (car a) (car b))))
		(if (and (null? (cdr a)) (null? (cdr b)))
		    #t
		    (list-eq? (cdr a) (cdr b)))
		#f)
	    )
	)
    ))
