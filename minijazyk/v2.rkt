#lang racket/base
(require (for-syntax racket/base syntax/parse)
         "run-time.rkt")
(provide (all-from-out "run-time.rkt")
         (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [module-begin #%module-begin]))

;; SYNTAX:
;; The module body consists of a sequnces of Blocks
;; where  Block = #:translate p:part-of-speech-expr Translation ...
;;              | #:entries entry-expr ...
;;  Translation = lhs:word rhs:word
;;         Word = Identifier | String

;; MEANING:
;; A `#lang minijazyk/v2` module has a `phrasebook` export bound to a
;; (run time) Phrasebook, a list of structs representing all of the
;; entries and translations in the module (see Phrasebook in
;; minijazyk/run-time). It has a `main` submodule that calls
;; `run-phrasebook` on the module's phrasebook.

(begin-for-syntax
  (define-splicing-syntax-class block
    #:attributes (code) ;; code : Expr[Phrasebook]
    (pattern (~seq #:translate part-of-speech t:translation ...)
             #:declare part-of-speech (expr/c #'part-of-speech/c)
             #:with code #'(let ([p part-of-speech.c])
                             (list (translation (p t.lhs.code) t.rhs.code) ...)))
    (pattern (~seq #:entries e ...)
             #:declare e (expr/c #'entry/c)
             #:with code #'(list e ...)))

  (define-splicing-syntax-class translation
    #:attributes (lhs.code rhs.code) ;; lhs.code, rhs.code : Expr[String]
    (pattern (~seq lhs:id-or-string rhs:id-or-string)))

  (define-syntax-class id-or-string
    #:attributes (code) ;; code : Expr[String]
    (pattern x:id     #:with code #'(symbol->string (quote x)))
    (pattern x:string #:with code #'(quote x))))

(define-syntax module-begin
  (syntax-parser
    [(_ b:block ...)
     #'(#%module-begin
        (define phrasebook
          (append b.code ...))
        (provide phrasebook)

        (module* main #f
          (#%module-begin
           (run-phrasebook phrasebook)))
        )]))

;; Magic incantation to make `#lang minijazyk/v2` work
(module reader syntax/module-reader minijazyk/v2)

;; ============================================================
;; Changelog
;;
;; v2: replace `jazyk` macro with module-begin hook,
;;     now can use `#lang minijazyk/v2`
;; v1: add macro argument contracts
;; v0: basic `jazyk` expression macro
