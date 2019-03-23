#lang racket/base
(require racket/contract
         racket/match
         racket/pretty)

(provide
 (contract-out
  [struct cz-verb ([inf string?] [stem string?] [group (or/c 'a 'i 'uj)])]
  [struct cz-vphrase ([words string?])]
  [struct translation ([cz entry/c] [en string?])]

  [entry/c contract?]
  [phrasebook/c contract?]
  [part-of-speech/c contract?]

  [verb (-> string? entry/c)]
  [verb-phrase (-> string? entry/c)]

  [lookup-verb (-> phrasebook/c string? (or/c entry/c #f))]
  [run-phrasebook (-> phrasebook/c void?)]))

;; ----------------------------------------
;; Data definitions

;; A Phrasebook is (Listof (U Entry Translation))

;; An Entry is one of
;; - (cz-verb String String (U 'a 'i 'uj)) -- verb w/ regular conjugation
;; - (cz-vphrase String)                   -- verb phrase (w/ verb first)
;; eg: (cz-verb "učit" "uč" 'i) = teach, (cz-verb "mít" "m" 'a) = have,
;;     (cz-vphrase "učit se") = learn.
(struct cz-verb (inf stem group) #:prefab)
(struct cz-vphrase (words) #:prefab)

;; A Translation is (Translation Entry String)
;; eg: (translation (cz-verb "učit" "uč" 'i) "teach")
(struct translation (cz en) #:prefab)

;; contracts:
(define entry/c (or/c cz-verb? cz-vphrase?))
(define phrasebook/c (listof (or/c entry/c translation?)))

;; elem->entry : (U Translation Entry) -> Entry
(define (elem->entry elem)
  (if (translation? elem) (translation-cz elem) elem))

;; ----------------------------------------
;; Parts of speech

;; A PartOfSpeech is a function (String -> Entry).
(define part-of-speech/c (-> string? entry/c))

;; verb : String -> Entry
;; Given a verb's infinitive form, and assuming it is regular,
;; compute a verb Entry with the verb's stem and conjugation group.
(define (verb inf)
  (cond [(regexp-match #rx"^(.*)ovat$" inf)
         => (match-lambda [(list _ stem) (cz-verb inf stem 'uj)])]
        [(regexp-match #rx"^(.*)at$" inf)
         => (match-lambda [(list _ stem) (cz-verb inf stem 'a)])]
        [(regexp-match #rx"^(.*)[eěi]t$" inf)
         => (match-lambda [(list _ stem) (cz-verb inf stem 'i)])]
        [else (error 'verb "bad verb: ~e" inf)]))

;; verb-phrase : String -> Entry
(define (verb-phrase words) (cz-vphrase words))

;; lookup-verb : Phrasebook String -> (U Entry #f)
(define (lookup-verb pb want-inf)
  (for/or ([elem (in-list pb)])
    (match (elem->entry elem)
      [(and entry (cz-verb inf _ _))
       (and (equal? inf want-inf) entry)]
      [_ #f])))

;; ----------------------------------------
;; Running a phrasebook

(define (run-phrasebook pb)
  ;; TODO: conjugate verbs and phrases, interactive gui, etc
  (printf "Here is the phrasebook:\n")
  (pretty-print pb))
