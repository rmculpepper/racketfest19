#lang racket/base
(require (for-syntax racket/base racket/string syntax/parse)
         "run-time.rkt")
(provide (all-from-out "run-time.rkt")
         (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [module-begin #%module-begin])
         Verb
         Verb-Phrase)

;; SYNTAX:
;; The module body consists of a sequnces of Blocks
;; where  Block = #:translate p:part-of-speech-expr Translation ...
;;              | #:entries entry-expr ...
;;              | #:definitions form ...
;;  Translation = lhs:word rhs:word
;;         Word = Identifier | String

;; MEANING:
;; A `#lang minijazyk/v4` module has a `phrasebook` export bound to a
;; (run time) Phrasebook, a list of structs representing all of the
;; entries and translations in the module (see Phrasebook in
;; minijazyk/v4-run-time).
;;
;; The following constraints on the entries and translations are
;; checked at *compile time*:
;;
;; 1. each declared verb has an infinitive of a regular form
;;    (that is, the `verb` function would not raise an error)
;; 2. each verb phrase starts with the infinitive form of a verb
;;    declared elsewhere in the module
;;
;; Consequently: a part-of-speech has both a static behavior and
;; dynamic behavior. The dynamic behavior is to compute an Entry (as
;; in previous versions of minijazyk). The static behavior is report
;; any declared verbs and check referenced verbs are defined, but
;; these must be done in two passes.
;;
;; The module also has a `main` submodule that calls `run-phrasebook`
;; on the module's phrasebook.

(begin-for-syntax

  ;; I like to bind `$` as an alias to `attribute`, for conciseness.
  (require (only-in syntax/parse [attribute $]))

  ;; We re-use `verb` at *compile-time* to implement the first check
  ;; (infinitive has regular form).
  (require (only-in "run-time.rkt" verb))

  ;; A PartOfSpeech is an identifier bound as syntax to a
  ;; PartOfSpeechRecord. A PartOfSpeechRecord is
  ;; (part-of-speech (Syntax -> (Listof String))
  ;;                 (Syntax -> (Listof String) -> Void)
  ;;                 Expr[(String -> Entry)])
  ;; The ct functions are called with the syntax of the (single)
  ;; argument.
  (struct part-of-speech (ct-verbs ct-check rt))

  (define-syntax-class part-of-speech-name
    (pattern (~var x (static part-of-speech? "part of speech name"))
             #:attr ct-verbs (part-of-speech-ct-verbs ($ x.value))
             #:attr ct-check (part-of-speech-ct-check ($ x.value))
             #:attr rt (part-of-speech-rt ($ x.value))))

  ;; ----------------------------------------

  (define-splicing-syntax-class block
    #:attributes (code verbs check) ;; code : Expr[Phrasebook]
    (pattern (~seq #:translate p:part-of-speech-name t ...)
             #:declare t (translation ($ p.ct-verbs) ($ p.ct-check) ($ p.rt))
             #:attr verbs (apply append ($ t.verbs))
             #:attr check ($ t.check)
             #:with code #'(list t.rt ...))
    #| (pattern (~seq #:entries e ...) ....) |#)

  (define-splicing-syntax-class (translation get-verbs mk-check rt-fun)
    #:attributes (verbs  ;; (Listof String)
                  check  ;; (Listof String) -> Void
                  rt)    ;; Expr[Translation]
    (pattern (~seq lhs:id-or-string rhs:id-or-string)
             #:attr verbs (get-verbs #'lhs.converted)
             #:attr check (mk-check #'lhs.converted)
             #:attr rt #`(translation (#,rt-fun (quote lhs.converted))
                                      (quote rhs.converted))))

  (define-syntax-class id-or-string
    #:attributes (converted) ;; converted : (Syntaxof String)
    (pattern x:id
             #:with converted (datum->syntax #'x (symbol->string (syntax-e #'x)) #'x))
    (pattern x:string
             #:with converted #'x))
  )

(define-syntax Verb
  (part-of-speech
   (lambda (arg) (list (syntax-e arg)))
   (lambda (arg) (lambda (all-verbs) (verb (syntax-e arg)) (void)))
   #'verb))

(define-syntax Verb-Phrase
  (part-of-speech
   (lambda (arg) null)
   (lambda (arg)
     (define want-verb (car (string-split (syntax-e arg))))
     (lambda (all-verbs)
       (unless (member want-verb all-verbs)
         (raise-syntax-error 'Verb-Phrase "verb not declared" arg))))
   #'verb-phrase))

(define-syntax module-begin
  (syntax-parser
    [(_ b:block ...)
     (define all-verbs (apply append ($ b.verbs)))
     (for ([checker (in-list (apply append ($ b.check)))])
       (checker all-verbs))
     #'(#%module-begin
        (define phrasebook
          (append b.code ...))
        (provide phrasebook)

        (module* main #f
          (#%module-begin
           (run-phrasebook phrasebook)))
        )]))

;; Magic incantation to make `#lang minijazyk/v4` work
(module reader syntax/module-reader minijazyk/v4)

;; ============================================================
;; CHALLENGES

;; 4.1 Add `#:entries` and `#:definitions` back to the language.

;; ============================================================
;; Changelog
;;
;; v4: Static Jazyk: check certain properties at compile time
;;     dropped `#:definitions` and `#:entries` for simplicity
;; v3: add `#:definitions` form
;; v2: replace `jazyk` macro with module-begin hook,
;;     now can use `#lang minijazyk/v2`
;; v1: add macro argument contracts
;; v0: basic `jazyk` expression macro
