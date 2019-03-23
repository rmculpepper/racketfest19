#lang minijazyk/v3

#:translate verb
dělat                   do
pracovat                work

#:translate verb-phrase
"učit se"               learn

#:entries
(verb "učit")
(cz-verb "mít" "m" 'a)

#:definitions

(define (mít-idiom cz-object en-phrase)
  (translation
   (cz-vphrase (format "mít ~a" cz-object))
   en-phrase))

#:entries
(mít-idiom "čas"    "have time")
(mít-idiom "rýmu"   "have a cold")
(mít-idiom "hlad"   "be hungry")
(mít-idiom "vztek"  "be angry")
