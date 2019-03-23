#lang racket/base
(require minijazyk/v0)

(run-phrasebook
 (jazyk

  #:translate verb
  dělat                 do
  pracovat              work

  #:translate verb-phrase
  "učit se"             learn

  #:entries
  (verb "učit")

  ))
