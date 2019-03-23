#lang racket
(require minijazyk/v1)

(run-phrasebook
 (jazyk

  #:translate verb
  dělat                 do
  pracovat              work

  #:translate verb-phrase
  "učit se"             learn

  ;; #:translate string-split ;; BAD
  ;; "Jak se máte?"        "How are you doing?"

  #:entries
  (verb "učit")

  ))
