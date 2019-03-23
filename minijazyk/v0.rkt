#lang racket/base
(require (for-syntax racket/base syntax/parse)
         "run-time.rkt")
(provide (all-from-out "run-time.rkt")
         jazyk)

;; (jazyk Block ...) : Expr[Phrasebook]
;; where  Block = #:translate p:part-of-speech-expr Translation ...
;;              | #:entries entry-expr ...
;;  Translation = lhs:word rhs:word
;;         Word = Identifier | String

(begin-for-syntax
  (define-splicing-syntax-class block
    #:attributes (code) ;; code : Expr[Phrasebook]
    (pattern (~seq #:translate part-of-speech:expr t:translation ...)
             #:with code #'(let ([p part-of-speech])
                             (list (translation (p t.lhs.code) t.rhs.code) ...)))
    (pattern (~seq #:entries e:expr ...)
             #:with code #'(list e ...)))

  (define-splicing-syntax-class translation
    #:attributes (lhs.code rhs.code) ;; lhs.code, rhs.code : Expr[String]
    (pattern (~seq lhs:id-or-string rhs:id-or-string)))

  (define-syntax-class id-or-string
    #:attributes (code) ;; code : Expr[String]
    (pattern x:id     #:with code #'(symbol->string (quote x)))
    (pattern x:string #:with code #'(quote x))))

(define-syntax jazyk
  (syntax-parser
    [(_ b:block ...)
     #'(append b.code ...)]))

;; ============================================================
;; Changelog
;;
;; v0: basic `jazyk` expression macro
