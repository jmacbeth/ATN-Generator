     ;; options are voice: passive or active, form: progressive or infinitive or "none", aspect: perfect or "none",
     ;; tense: future, present or past, mood: indic, interrog, neg t (not required).  Not all of it works yet though
     ;; note that usually infinitive is considered a "mood", I've grouped it with form because both infinitive and progressive
     ;; are used when a verb phrase is a dependent clause or object
     ;; here's an example with bells and whistles
     ;; (c1 (tok l1 voice active form none aspect none auxverb l-might tense
     ;;  past mood indic aux none neg t time 1 pron none agt c2 obj c3 vs () )) ;; see

     ((c1 (tok l1 voice active form none aspect none auxverb none tense
          past mood indic aux none time 1 pron none agt c2 obj c3 vs () )) ;; see
     ;; removed the obj and tried to add while clause
     (c1-test (tok l1 voice active form none aspect none auxverb none tense
               past mood indic aux none time 1 pron none agt c2 obj c3 vs () )) ;; see
     (conj-test (conj-word l-and comma c node-list (c1 c3 c14) lab conj-gen0))
     (embed-test1 (conj-word l-and node-list (c1 c3) ))
     (embed-test2 (conj-word l-and node-list (c10 c14) ))
     (c2 (tok l2 nbr sing det none pron none agt* (c1 c10 c14 c17) ns ()) ) ;; John
     (c3 (tok l3 voice active form progressive aspect none auxverb none tense past mood indic aux prog pron none time 1 agt c4 inst c5 loc c6 vs ()) ) ;; wrestle 
     (c4 (tok l4 nbr sing det none agt* (c3 c17) pron none ns ()) ) ;; Mary
     (c5 (tok l5 obj c7 pobj p ) ) ;; with .  pobj is needed here to actually generate the print image for "with"
     (c6 (tok l6 obj c8 pobj p ) ) ;; at
     (c7 (tok l7 det indef nbr sing pron none obj* (c17) ns () ) ) ;; bottle
     (c8 (tok l8 nbr sing det def pron none modifier c9 loc* (c3 c10 c14 c17) ns () ) ) ;; bar
     (c9 (tok l9 nbr sing det none ns () ) ) ;; liquor
     (c10 (tok l10 voice active form none aspect none auxverb none tense past mood
	   indic time 2 aux none agt c2 obj c13 pron none vs ()) ) ;; go
     (c11 (tok l11 obj c4 pobj p )) ;; to
     (c12 (tok l12 nbr sing det none ns () ) ) ;; over
     (c13 (tok l-help voice active form infinitive aspect none auxverb none tense past mood indic
	   aux prog pron none time 3 obj c4 inst c5 vs ()) ) ;; help
     (c14 (tok l-draw voice active form none aspect none auxverb none tense past mood indic
	   aux prog pron none time 4 agt c2 obj c15 vs ()) ) ;; draw
     (c15 (tok l-cork det def nbr sing modifier c7 ns () ) ) ;; cork
     (c16 (conjunction l-and firn c4 secn c2 pron none agt* (c17) ) ) ;; john and mary
     (c17 (tok l-drink voice active form none aspect none auxverb none tense past mood indic
	   aux prog pron none time 5 agt c16 obj c18 man l-together vs ()) ) ;; drink
     (c18 (tok l-champagne nbr sing det def obj* (c17) ns () ) ) ;; champagne
     (c19 (tok l-who nbr sing det none pron none ns ()) )) ;; who