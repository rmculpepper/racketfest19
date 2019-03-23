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
;;              | #:definitions form ...
;;  Translation = lhs:word rhs:word
;;         Word = Identifier | String

;; MEANING:
;; A `#lang minijazyk/v3` module has a `phrasebook` export bound to a
;; (run time) Phrasebook, a list of structs representing all of the
;; entries and translations in the module (see Phrasebook in
;; minijazyk/run-time).
;;
;; It has a `main` submodule that calls `run-phrasebook` on the
;; module's phrasebook.
;;
;; The Racket forms (definitions, requires, provides, etc) in a
;; `#:definition` block are moved to the top of the module and
;; evaluated before the translations and entries.

(begin-for-syntax
  (define-splicing-syntax-class block
    #:attributes ([def 1] code) ;; def : ModuleTopLevelForm, code : Expr[Phrasebook]
    (pattern (~seq #:translate part-of-speech t:translation ...)
             #:declare part-of-speech (expr/c #'part-of-speech/c)
             #:with (def ...) '()
             #:with code #'(let ([p part-of-speech.c])
                             (list (translation (p t.lhs.code) t.rhs.code) ...)))
    (pattern (~seq #:entries e ...)
             #:declare e (expr/c #'entry/c)
             #:with (def ...) '()
             #:with code #'(list e ...))
    (pattern (~seq #:definitions def:expr ...)
             #:with code #''()))

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
        b.def ... ...
        (define phrasebook
          (append b.code ...))
        (provide phrasebook)

        (module* main #f
          (#%module-begin
           (run-phrasebook phrasebook)))
        )]))

;; Magic incantation to make `#lang minijazyk/v3` work
(module reader syntax/module-reader minijazyk/v3)

;; ============================================================
;; CHALLENGES

;; 3.1 Add an `#:include` form to the language:
;;
;;     Block = .... | #:include module-path
;;
;; When a minijazyk module contains a `#:include` block, the module it
;; refers to is treated like another minijazyk module, and the second
;; module's phrasebook is appended as part of the first module's
;; phrasebook.

;; 3.2 Moving the contents of `#:definitions` blocks to the top of the
;; module means expressions in the module aren't evaluated in their
;; lexically-apparent order. Modify ex-v3.rkt to demonstrate the
;; evaluation reordering. Modify the language so that expressions are
;; evaluated in their lexically-apparent order.

;; ============================================================
;; Changelog
;;
;; v3: add `#:definitions` form
;; v2: replace `jazyk` macro with module-begin hook,
;;     now can use `#lang minijazyk/v2`
;; v1: add macro argument contracts
;; v0: basic `jazyk` expression macro
