;; This code is based on Simmons and Slocum 1972
;; This one explores tenses, aspects, moods, etc.
;; see atn_semantic_net.lisp for more notes.


(defun reset-property-list ()
  (mapcar

   #'(lambda (x) (setf (symbol-plist (first x)) (copy-list (second x))))

   '(
     ;; options are voice: passive or active, form: progressive or infinitive or "none", aspect: perfect or "none",
     ;; tense: future, present or past, mood: indic, interrog, neg t (not required).  Not all of it works yet though
     ;; note that usually infinitive is considered a "mood", I've grouped it with form because both infinitive and progressive
     ;; are used when a verb phrase is a dependent clause or object
     ;; here's an example with bells and whistles
     ;; (c1 (tok l1 voice active form none aspect none auxverb l-might tense
     ;;  past mood indic aux none neg t time 1 pron none agt c2 obj c3 vs () )) ;; see

     (c1 (tok l1 voice active form none aspect none auxverb none tense
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
     (c19 (tok l-who nbr sing det none pron none ns ()) ) ;; who
     (l1 (pi "see" present "sees" past "saw" past-part "seen" prog "seeing" terminal t) )
     (l2 (pi "John" pron-agt "he" pron-pred "him" terminal t) )
     (l3 (pi "wrestle" present "wrestle" past "wrestled" past-part "wrestled" prog "wrestling" terminal t) )
     (l4 (pi "Mary" pron-agt "she" pron-pred "her" terminal t) )
     (l5 (pi "with" terminal t) )
     (l6 (pi "at" terminal t) )
     (l7 (pi "bottle" pron-agt "it" pron-pred "it" terminal t) )
     (l8 (pi "bar" pron-agt "it" pron-pred "there" terminal t) )
     (l9 (pi "liquor" pron-agt "it" pron-pred "it" terminal t) )
     (l10 (pi "go" present "goes" past "went" past-part "gone" prog "going" terminal t))
     (l11 (pi "to"))
     (l12 (pi "over" pron-agt "there" pron-pred "there" terminal t) )
     (l-cork (pi "cork" pron-agt "it" pron-pred "it" terminal t) )
     (l-champagne (pi "champagne" pron-agt "it" pron-pred "it" terminal t) )
     (l-be (pi "be" present "is" past "was" past-part "been" prog "being" terminal t))
     (l-have (pi "have" present "have" past "had" past-part "had" prog "having" terminal t))
     (l-do (pi "do" present "does" past "did" past-part "done" prog "doing" terminal t))
     (l-help (pi "help" present "helps" past "helped" past-part "helped" prog "helping" terminal t))
     (l-draw (pi "draw" present "draws" past "drew" past-part "drawn" prog "drawing" terminal t))
     (l-drink (pi "drink" present "drinks" past "drank" past-part "drank" prog "drinking" terminal t))
     (l-and (pi "and" terminal t))
     (l-together (pi "gleefully" terminal t))
     (l-will (pi "will" present "will" past "will" past-part "will" prog "will" terminal t))
     (l-might (pi "might" present "might" past "might" past-part "might" prog "might" terminal t))
     (l-who (pi "who" pron-agt "who" pron-pred "who" terminal t) )
     (l-while (pi "while" terminal t) )
     
     ;; GRAMMAR NETWORK

    
     ;; New in summer 2018
     ;; compound sentences, e.g. "x while y", need a special semantic node that points to both clauses
     (cmp1 (paths ((s cmp2) (conj-gen0 cmp2)))) ;; generate main clause
     (cmp2 (paths ((subord-cl cmp3)))) ;; p-subord is the subordinate clause preposition e.g. "while"
     (cmp3 (paths ((t))))

     ;; while y, x.  Need to have the pair "comma p" in the structure
     (cmp4 (paths ((subord-cl cmp5))))
     (cmp5 (paths ((comma cmp6)))) 
     (cmp6 (paths ((s cmp7) (conj-gen0 cmp7))))
     (cmp7 (paths ((t))))

     (subord-cl (paths ((p-subord subord-cl1))))
     (subord-cl1 (paths ((s subord-cl2) (conj-gen0 subord-cl2))))
     (subord-cl2 (paths ((t))))

     ;; conjunction
     (conj-gen0 (paths ((s conj-gen1))))  ;; add others to this list, NP, ADJ...
     ;; or change s to conj-item and have the conj-item function generate whatever is needed.
     (conj-gen1 (paths ((comma conj-gen2) (conj-gen2)))) ;; or go straight to conj-gen2 if comma isn't set?  nice!
     (conj-gen2 (paths ((conj conj-gen3) (conj-gen0))))
     (conj-gen3 (paths ((s t))))
     
     
     ;; now voice happens first, in case we need to switch subject and object
     ;; still have trouble with passive voice ...
     (s (paths ((voice s1))))
     
     (s1 (paths ((subj pred))))
     (pred (paths ((voice tfm))))
     (tfm (paths ((form asp))))
     (asp (paths ((aspect auxv))))
     (auxv (paths ((auxverb tns)))) ;; there's another symbol in grammar called aux... would like to get rid of that one
     (tns (paths ((tense prop))))
     (prop (paths ((mood indic)))) ;; the paper has t here instead of indic.  assume indic
     (indic (paths ((s2))))
     (interrog (paths ((prevb s2))))

     ;; here's the rest of the grammar is slightly different from the
     ;; earlier one, since the earlier one uses AGT and DAT instead of
     ;; SUBJ?  No, actually the "voice" function expects AGT and
     ;; assigns AGT to SUBJ, changes/swaps SUBJ and OBJ if needed for
     ;; passive voice.
     (subj (paths ((pron pron-agt) )))  ;; added extra step for pronoun labeling
     (agt (paths ((pron pron-agt) ))) ;; added extra step for pronoun labeling need to merge these.
     (pron-agt (paths ((obj)))) ;; eliminated np0. in favor of obj, which can generate a verb phrase as object

     (np1 (paths ((nbr np2) (firn np-conj1)))) ; only np1.  optional POBJ disappeared and np0 was deprecated, 
     (np2 (paths ((modifier np3) (np3))))
     (np3 (paths ((det np4))))
     (np4 (paths ((ns np5) )))
     ; generating pronomial embeddings     
     (np5 (paths ((embed np6) (t))))

     ;; paths for conjunctive noun phrase
     (np-conj1 (paths ((conjunction np-conj2))))
     (np-conj2 (paths ((secn np-conj3))))
     (np-conj3 (paths ((t))))
     (firn (paths ((subj))))  ;; make sure to use subj and not np1, so that we generate "she and he"
     (secn (paths ((subj))))

     ;; embedding for agent
     ;; 4. depth limits, and when you don't recursively generate because of depth, you need to put the
     ;; node that you skipped on a queue so that it can be generated later

     (s2 (paths ((pron pron-pred)))) ;; added extra step for pronoun labeling
     (pron-pred (paths ((man vp0) (vp0))))
     (vp0 (paths ((aux vp1)))) ;; needed to leave this aux in ,
                               ;; but now try to remove, now that i have fix for agt as part of object
     (vp1 (paths ((vs vp2))))
     (vp2 (paths ((obj vp3) (vp3))))
     (vp3 (paths ((inst vp4) (vp4))))
     (vp4 (paths ((loc vp5) (vp5)))) ;; now can have either or both inst and loc in a vp
     (vp5 (paths ((embed-2nd-pred vp6)))) 
     (vp6 (paths ((end-punct vp7) (vp7)))) ;; handle end-of-sentence punctuation? not currently handled this way
     (vp7 (paths ((t))))
     
     ;; OK, now there are special paths for verb phrases as objects (dependent clause)
     ;; created special obj-prog-agt ando bj-pred symbols to handle "john saw mary wrestling"
     ;; and "john saw her wrestling"
     ;; now the pronoun should be generated as a predicate type pronoun by default, since the pron-type
     ;; label will be 'pron-obj-prog-agt

     ;; would like to generate -ing clauses with or without subjects,
     ;; but would like to generate "to x" infinitive clauses only without subjects
     (obj (paths ((obj-prog-agt obj-pred) (obj-verb-no-agt obj-pred) (np1))))
     (obj-prog-agt (paths ((pron pron-obj-prog-agt))))
     (pron-obj-prog-agt (paths ((np1))))
     ;; and here I only want to account for form, nothing else
     (obj-pred (paths ((obj-form s2))))
  
     ;; generating prepositional phrases, removed the pobj from elsewhere.  going to generate these separately here
     (pp0 (paths ((pobj pp1))))
     (pp1 (paths ((obj pp2))))
     (pp2 (paths ((t))))
     ;; and these for generating locations, instruments
     (loc (paths ((pp0))))
     (inst (paths ((pp0))))
     (dat (paths ((pp0))))
     ;; terminal symbols in the grammar, maybe just having paths be nil is another option
     (pobj (terminal t)) (nbr (terminal t )) (det (terminal t)) (ns (terminal t))
     (man (terminal t)) (aux (terminal t)) (vs (terminal t)) (voice (terminal t))
     (form (terminal t)) (aspect (terminal t)) (auxverb (terminal t)) (tense (terminal t)) (mood (terminal t))
     (prevb (terminal t)) (pron (terminal t)) (end-punct (terminal t)) (obj-form (terminal t))
     (obj-verb-no-agt (terminal t)) (modifier (terminal t)) (conjunction (terminal t))  (man (terminal t))
     (embed (terminal t)) (embed-2nd-pred (terminal t)) (p-subord (terminal t)) (comma (terminal t)) (conj (terminal t))
     )
   )
  )

  
;; Need this for creating new structure nodes if needed.  Copied from Common LISP versions
;; of Mcdypar.
(defun newsym (sym)
  ;; create a new unique symbol in the sequence sym1 sym2 sym3 ...
  (let ((count (get sym 'usage-count)))
    (if (null count) (setf count 0))
    (setf (get sym 'usage-count) (1+ count))
    (intern (concatenate 'string (string sym)
                                      (prin1-to-string count)))))



;; GENERATION FUNCTIONS
;; voice form aspect tense mood

;; voice, or some other of these verb form functions, has to assign
;; subj, otherwise the rest of the grammar doesn't work.  May eventually want to
;; stick to either agt or subj, not both.
(defun voice (structure)
  ;; voice is now used off of "s" and off of "pred" as well
  (if (not (get structure 'subj)) ;; checks to make sure that voice hasn't already been applied
      (if (equal 'passive (get structure 'voice))
          (progn
	    (setf (get structure 'vs) ;; passive
	          (append '(l-be) (list (get (get structure 'tok) 'past-part)) '("by"))) ;; a real hack to add "by".
	    ;; real solution is to add a new semantic node for by and attach agent to it?  but this really mangles the semantic
	    ;; network.  (later: it's OK to mangle the semantic network
            ;; much of the code does that.
	    (setf (get structure 'subj) (get structure 'obj))
	    (setf (get structure 'obj) (get structure 'agt)) 
	    (setf (get structure 'agt) nil)
	    )
          (progn
	    (setf (get structure 'vs) ;; active
	          (list (get structure 'tok)))
	    (setf (get structure 'subj) (get structure 'agt))
	    ;; leave obj alone
	    )
          )
      )
  nil
  )

(defun form (structure)
  (if (equal 'progressive (get structure 'form))
      (setf (get structure 'vs) 
	    (append '(l-be) (list (get (first (get structure 'vs)) 'prog))  (rest (get structure 'vs))))
      ;; otherwise do nothing
      )
  nil
  )

;; special version for certain kinds of verb phrases as objects
(defun obj-form (structure)
  (setf (get structure 'vs)
	(cond ((equal 'progressive (get structure 'form))
		(list (get (get structure 'tok) 'prog)))
	       ((equal 'infinitive (get structure 'form))
		(list "to" (get (get structure 'tok) 'pi)))
	       )
	)
  nil
  )

(defun aspect (structure)
  (if (equal 'perfect (get structure 'aspect))
      (setf (get structure 'vs) 
	    (append '(l-have) (list (get (first (get structure 'vs)) 'past-part))  (rest (get structure 'vs))))
      ;; otherwise do nothing
      )
  nil
  )


(defun auxverb (structure)
  (let ((aux 
	 (cond ((equal 'future (get structure 'tense)) 'l-will) ;; always will when future tense
	       ((not (equal 'none (get structure 'auxverb))) (get structure 'auxverb)) ;; specified in structure node
	       ((equal 'interrog (get structure 'mood)) 'l-do) ;; must have auxiliary verb for interrogatives
	       ((get structure 'neg) 'l-do) ;; must have auxiliary verb for negative
	       (T nil) ;; otherwise no aux verb at all
	       )))
    (if aux (setf (get structure 'vs) ;; append aux verb to 'vs but put the print image of whatever was first
		  (append (list aux)
			  (if (get structure 'neg) (list 'not)) ;; handle negation here
			  (list (get (first (get structure 'vs)) 'pi))
			  (rest (get structure 'vs)))))
    ;; otherwise do nothing
    )
  nil ;; don't generate any symbols on output
  )

(defun tense (structure)
  ;; Fix.  Refactor
  (cond ((equal 'past (get structure 'tense))
	 (setf (get structure 'vs)
	       (append (list (get (first (get structure 'vs)) 'past))  (rest (get structure 'vs)))))
	(T (setf (get structure 'vs) ;; present and future tense.  auxverb will take care of adding "will" for future
		 ;; Need to match with subject!
		 (append (list (get (first (get structure 'vs)) 'present))  (rest (get structure 'vs)))))
	)
  nil
  )

(defun mood (structure)
  (if (equal 'interrog (get structure 'mood))
      (progn (setf (get structure 'lab) 'interrog)
	     (setf (get structure 'prevb) (first (get structure 'vs)))
	     (setf (get structure 'vs) (rest (get structure 'vs)))
	     )
      )
  nil
  )

(defun prevb (structure) (list (get structure 'prevb)))
  

(defun nbr (structure)
  (setf (get structure 'ns) (append  (get structure 'ns) (list (get (get structure 'tok) 'pi)) ))
  nil
  )

;; OK, ns is going to do a lot of decision making about pronouns and embedding.
(defun ns (structure)
  (prog1 ;; prog1 means that the function evals to the first form, but it does everything after too.
      (cond ((and do-embed (> embed-count 0) (= 0 (random 2)))
             (decf embed-count) (setf (get structure 'embed) t) (get structure 'ns)) ;; try to do the embedding. no pronoun
	    ((equal 'tok (get structure 'pron-tok)) (pron-gen structure)) ;; do the pronoun
	    (T (setf (get structure 'pron-tok) 'tok) (get structure 'ns)) ;; no pronoun
	    )
    (setf (get structure 'ns) '()) ;; need to clear 'ns, otherwise get "Mary Mary Mary ..."
    (if (= 0 (random 4)) (setf (get structure 'pron-tok) nil)) ;; for more variety, occasionally kill the pronoun generation
    (setf (get structure 'pron-type) nil) ;; always reset the pron-type back to nil
    )
  )

;; This is a terminal grammar symbol which saves the structure label
;; Which will allow generation of an agt or pred pronoun in the right
;; circumstances
(defun pron (structure)
  (setf (get structure 'pron-type) (get structure 'lab ))
  nil
  )

;; This is called by the ns terminal grammar symbol to actually generate the pronoun
;; But in the rare case where the pronoun was already generated (and 'pron-tok was set
;; on the structure node) but pron-type was not set, it still generates properly
(defun pron-gen (structure)
  (cond ;; ((null (get structure 'pron-type)) (get structure 'ns))
	((equal 'pron-agt (get structure 'pron-type)) (list (get (get structure 'tok) 'pron-agt)))
	(T (list (get (get structure 'tok) 'pron-pred)))
      )
  )

  ;; I still need aux to take care of "wrestling" in "john saw mary wrestling"
  ;; If I try to fall back on the other functions, I get "john saw mary be wrestling"
(defun aux (structure)
  (if (null (get structure 'vs)) ;; if there is already a 'vs, then do nothing 
      (setf
       (get structure 'vs)
       (append (list (get (get structure 'tok)
			  ;; grab the past or progressive form
			  ;; if required.
			  (cond ((equal (get structure 'aux) 'past) 'past)
				((equal (get structure 'aux) 'prog) 'prog)
				(T 'pi))))
	       (get structure 'vs)
	       )
       )
      )
  nil
  )
  

(defun vs (structure)
  (setf (get structure 'embed-2nd-pred) t) ;; experimenting with embedding second predicate
  (get structure 'vs) ;; already a list
  )

;; before I had this so that it was putting the print image of the preposition
;; into the 'ns for the structure.  and this created a problem "A WITH BOTTLE"
;; when interacting with det and ns, the other terminal functions. Now the code just generates the
;; print image of the preposition as part of gen, which correctly creates "with a bottle"

(defun pobj (structure)
  (list (get (get structure 'tok) 'pi))  ;; now this is happening at the pobj node
  )

;; doing the same for prepositions for subordinate clauses
(defun p-subord (structure)
  (list (get (get structure 'tok) 'pi))
  )

(defun comma (structure)
  (list '*comma*)
  )

;; JCM 2018 need to reconcile this with my new conjunction stuff...
(defun conjunction (structure)
  (list (get (get structure 'conjunction) 'pi))
  )

(defun man (structure)
  (list (get (get structure 'man) 'pi))
  )

;; this still doesn't work
(defun modifier (structure)
  (setf (get structure 'ns)
	(append (list (get (get (get structure 'modifier) 'tok) 'pi)) (get structure 'ns))
	)
    nil
    )

(defun det (structure)
  (setf (get structure 'ns)
	(append
	 (case (get structure 'det)
	   ('indef '("a"))
	   ('def '("the"))
	   ('none nil))
	 (get structure 'ns)
	 )
	)
  nil
  )

;; deals with verb phrases as objects
(defun obj (structure)
  (cond ((equal 'infinitive (get structure 'form))
	 (setf (get structure 'obj-form) (get structure 'form))
	 (setf (get structure 'obj-verb-no-agt) 't ))
	((get structure 'agt) ;; assume progressive
	 (setf (get structure 'obj-prog-agt) (get structure 'agt))
	 (setf (get structure 'obj-form) (get structure 'form)))
	((equal 'progressive (get structure 'form)) ;; progressive no agent
	 (setf (get structure 'obj-form) (get structure 'form))
	 (setf (get structure 'obj-verb-no-agt) 't )))
  nil
  )


(defun end-punct (structure)
  (list (get structure 'end-punct))
  )

;; This is a test to see that functions corresponding to all grammar symbols
;; are called when any structure becomes labeled with that grammar symbol.
(defun auxv (structure)
   (format t "hello from auxv!!!!~%")
   )

;; fixes structure so that s is first item from node-list
;; and replaces node-list with rest from structure
(defun conj-gen0 (structure)
  (let* ((node-list (get structure 'node-list))
         (next-item (first node-list)))
    (setf (get structure 's) next-item) 
    (setf (get structure 'node-list) (rest node-list)) ;; replace node-list
    )
  )

;; checks to see if the node-list only has one element
;; if so, this is the last one, sticks 'conj on the structure
;; so that the 'conj path in the grammar is followed
(defun conj-gen2 (structure)
  (if (= 1 (length (get structure 'node-list)))
      (setf (get structure 'conj) 'c))
  )

;; you reach conj-gen3 when you only have the last node in the node-list,
;; but conj-gen3 just does the same thing as conj-gen0 - it sticks
;; the only node on the node-list into the structure as 's
;; replaces node-list with (rest node-list) which is nil.
(defun conj-gen3 (structure)
  (conj-gen0 structure))

;; similar to pobj or p-subord
;; grab whatever l symbol is the conj-word and print it.
(defun conj (structure)
  (list (get (get structure 'conj-word) 'pi))
  )

;; working on generated embeddings here 
(defun get-random-ungenerated-structure-from-list (x)
  (if (null x) nil
      (let ((random-member (get-random-nth x)))
	(cond ((null (get random-member 'lab)) random-member) ;; not generated yet.  Good.
	      (T (get-random-ungenerated-structure-from-list (remove random-member x)))))
      )
  )

;; constraint-structure has time constraints (and maybe other constraints?) for the
;; new structure that you want to embed.
(defun get-next-ungenerated-structure-from-list (x)
  (if (null x) nil
      (let ((next-member (car x)))
	(cond ((null (get next-member 'lab)) next-member) ;; not generated yet.  Good.
	      (T (get-next-ungenerated-structure-from-list (remove next-member x)))))
      )
  )

;; 
(defun embed (structure)
  (let ((node-to-embed (get-random-ungenerated-structure-from-list (get structure do-embed))))
    (if node-to-embed (case do-embed  ;; I should probably get this into the grammar
                        ('agt* (embed-agt node-to-embed))
                        ('obj* (embed-obj node-to-embed))
                        ('loc* (embed-loc node-to-embed)))
        )
    )
  )


;; What this does is to replace the agt (agent) for the clause with the token for "who"
;; so that what would have been generated as "John went to help" becomes "who went to help"
;; I think this is because it's hard to generate pred on its own.
;; somewhat different from the "which" and "where" embeddings.
(defun embed-agt (node-to-embed)
  (if (= 0 (random 2)) ;; don't always embed for agt
      (progn
        (setq do-embed nil)
        (setf (get node-to-embed 'lab) 's) ;; label as "s" so that we can generate clause
        (setf (get node-to-embed 'agt) 'c19) ;; c19 is "who"
        (append '(*comma*) (gen node-to-embed) '(*comma*))
        )
      )
  )

(defun embed-obj (node-to-embed)
  (setq do-embed nil)
  (setf (get node-to-embed 'lab) 's)
  (setf (get node-to-embed 'obj) nil)
  (append '(*comma*) '("which") (gen node-to-embed) '(*comma*))
  )

(defun embed-loc (node-to-embed)
  (setq do-embed nil)
  (setf (get node-to-embed 'lab) 's)
  (setf (get node-to-embed 'loc) nil)
  (append '(*comma*) '("where") (gen node-to-embed) '(*comma*))
  )

(defun embed-2nd-pred (structure)
  (let ((node-to-embed (get-next-ungenerated-structure-from-list
                        (get (get structure 'subj) 'agt*)
                        )))
    (if (and node-to-embed (> embed-count 0))
        (progn
          (decf embed-count)
          (setf (get node-to-embed 'lab) 'pred)
          (append '("and") (gen node-to-embed) )
          )
        )
    )
  )


;;;;; SEMANTIC / GRAMMAR MATCHING AND GENERATION FUNCTIONS 

(defun generate-unary-rule (structure rule)
  (setf (get structure 'lab) (car rule))
  (format t "Match was a unary rule, so relabeling structure")
  (format t " ~a to ~a and calling ~a on it (if bound) and then calling gen on it recursively.~%" structure (car rule) (car rule))
  ;; needed to add this... this code to call the function and then generate is in many places... should we
  ;; move the function call to be at the top of generate?
  ;; JCM 2018 (if (fboundp (car rule))  (funcall (car rule) structure) )
  (gen structure)
  )
  
(defun visit-terminal (structure rule)

  ;; If this new semantic structure node is a terminal node in the
  ;; grammar, the simmons paper recursively calls gen but first
  ;; applies the function corresponding to the structure label to the
  ;; structure if it's a terminal node, and I apply the function
  ;; corresponding to the structure label?  Do I really need to append
  ;; a call gen to that?  Yes, because ... it's possible to have more
  ;; than one terminal node in the grammar act on a semantic node.
  ;; For example, POBJ, NBR, DET, and NS are a sequence of terminal
  ;; nodes that will all need to act on the semantic node to produce a
  ;; string of the noun with the right determiner, singular/plural
  
  (format t "~a is a terminal node in the grammar so we're printing it but calling gen recursively on structure ~a.~%" (car rule) structure)
  ;; leave in funcall for terminal node symbol.
  (append (if (fboundp (car rule)) (funcall (car rule) structure)) (gen structure))
  )


(defun generate-binary-rule (structure rule)
  (format t "Generate based on matching between structure ~a and grammar path ~a.~%"
	       structure rule)
  ;; set k to the child semantic structure node that we're connecting to
  ;; The parent semantic structure node has an association corresponding to the grammar that points to
  ;; the appropriate child.  (e.g. if the grammar transition is "pobj np1" then our semantic node should have
  ;; a "pobj" association to another semantic node
  (setq k (get structure (car rule)))
  (format t "k was set to ~a. ~%" k)
  ;; relabel the semantic structure with the grammar node symbol at the end of the
  ;; transition.  the relabel happens first ... maybe it should happen after?
  (setf (get structure 'lab) (cadr rule))
  (format t "structure node ~a was relabeled to ~a.~%" structure (cadr rule)) 
 
  (cond
    ((get (car rule) 'terminal) (visit-terminal structure rule))

    ;; Not a terminal, so we're actually going to connect to a semantic structure node.
    ;; Label the new structure that we're visiting after the grammar non-terminal.
    ;; What I should probably do is decomp this code and build a couple of "visitor" functions
    (T (format t "Not a terminal, but still calling function ~a if it is bound.~%" (car rule) )
       ;; JCM 2018 (if (fboundp (car rule))  (funcall (car rule) k) )
       (setf (get k 'lab) (car rule))
       ;; calling the grammar node function on the structure node that we're about to generate on.
       ;; it can make important preparations before generating (for example, see the obj function)
       (format t "labeled structure node ~a to ~a.~%" k (car rule))
       ;; normally the simmons version of the code deletes the connection from the
       ;; semantic network, and calls apply with the name of the relation as a function
       ;; i won't do this for now, but may later.
       (format t "now calling gen again with structure nodes ~a and ~a.~%" k structure)
       (append (gen k) (gen structure))
       )
    )
  )


(defun gen (structure)
  ;; first thing I want to do is to try to call the function corresponding to the current grammar label
  ;; of the structure.  Previously there was code to do something like this in various places,
  ;; but I centralized it here.
  (if (fboundp (get structure 'lab))  (funcall (get structure 'lab) structure) )
  (cond
    ;; don't need this first line.  There have never been any structures
    ;; that reach a label called 'terminal. also checked the logs, and this log message never gets printed
    ;; JCM 2018 ((setq j (get structure 'terminal)) (print "terminal node. exiting.") (list j))

    ;; If this node in the semantic network isn't labeled, then exit.
    ;; but also set j to be the label of this node
    ;; do I really need this?  In what situation would I visit a structure node that isn't labeled?
    ((null (setq j (get structure 'lab))) (print "no structure label. exiting.") nil)
	
    ;; In this hacked version, won't check to see if label is in the grammar
    ;; if you get this far, you have a match between grammar and structure!
    ;; now i need to check all rules until I have a match
    ;; j is the grammar label on the semantic structure.  Get the paths from that
    ;; node in the grammar and see if any match the semantic structure
    ;; if there is no match, exit and return nil
    ((null (setq rule (random-get-matching-rule structure (get j 'paths))))  (format t "the structure ~a had nothing matching paths in the grammar state ~a.  exiting.~%" structure j) nil)

    ;; if there is a match now rule is set to the grammar rule
    ;; if it's a unary rule, just relabel the semantic structure to that node in the grammar
    ;; and call gen recursively
    ;; unary rules don't spawn new generations, just a recursive call to generate on the
    ;; same node.
    
    ((= 1 (length rule)) (generate-unary-rule structure rule))

    ;; if it's not a unary rule, then the grammar state symbol is the second element of rule
    
    (T (generate-binary-rule structure rule))
    )
  )

(defun get-random-nth (x)
  (if (> (length x) 0)
      (nth (random (length x)) x)
      nil)
  )

(defun get-random-nth-by-percentage (percent x) ;; gets the first member with probability percent
  (cond ((null x) nil)
	((< (random 100) percent) (first x))
	(T (get-random-nth x))
	)
  )

(defun get-c-nodes-matching-time (node node-list)
  (cond ((null node-list) nil)
        ((in-time-range (car node-list) node)
         (append (list (first node-list)) (get-c-nodes-matching-time node (rest node-list)))) 
	(T (get-c-nodes-matching-time node (rest node-list))))
  )

;; Non-nil if time is within the time span specified in the structure
;; (defun in-time-range (node other-node)
;;   ;; structure time is assumed to be a pair, e.g. (2 3)
;;   (let ((node-time (get structure 'time))
;;         (other-node-))
;;     (and (>= time (first structure-time)) (<= time (second structure-time))))
;;     )
;;   )

;; return a matching rule or nil if there was none
;; we should be able to deprecate this because we now generate
;; semi-randomly.
(defun get-matching-rule (structure rule-list)
  (cond ((null rule-list) nil)
	;; if the first rule in the list is a list with a single element, then it's a
	;; "unary" rule.  It always matches.  Return it.
	((null (cdar rule-list)) (car rule-list))
	;; not a unary rule, so see if the syntactic arc matches a relation in the semantic structure
	;; if it does, it's your match so return it.
	((get structure (caar rule-list)) (car rule-list))
	;; the car isn't a match.  Recursively call
	(T (get-matching-rule structure (cdr rule-list)))
	)
  )

;; new method can be mapcar-ed to the whole list.  If there is a match, returns the element.
;; If not, returns nil.  Assume rule is a list. 
(defun does-rule-match (structure rule)
  (cond ((null rule) nil)
	((null (second rule)) rule) ;; unary rule always matches
        ;; skip obj inst loc that have already been generated... cheap hack... should probably be part of gen function 
	((and (equal (first rule) 'obj) (get (get structure 'obj) 'lab)) nil)
        ((and (equal (first rule) 'inst) (get (get structure 'inst) 'lab)) nil)
        ((and (equal (first rule) 'loc) (get (get structure 'loc) 'lab)) nil)
        ((get structure (first rule)) rule) ;; for a binary rule, just check that the structure matches
	(T nil)
	)
  )

(defun random-get-matching-rule (structure rule-list)
  (get-random-nth-by-percentage (setq clause-generate-probability (+ clause-generate-probability 1))
   (remove nil (mapcar
		#'(lambda (rule) (does-rule-match structure rule))
		rule-list)
	   )
   )
  )

(defun shuffle-list (x)
  (cond ((null x) nil)
	(T (let ((chosen-element (get-random-nth x)))
	     (append (list chosen-element) (shuffle-list (remove chosen-element x))))))
  )



(defun gen-from-list (node-list) ;; takes a list of structure nodes and generates from them
  (mapcar
   #'(lambda (node) (post-process-sentence (gen-with-embedding node)))
   node-list)
  )

;;(defun gen-from-list-matching-time (node-list)
  ;;(mapcar
   ;;#'(lambda (node) (get-c-nodes-matching-time 


(defun gen-random-from-list (node-list)
  (gen-from-list (shuffle-list node-list))
  )

(defun post-process-sentence (x)
  (if x
      (string-capitalize
       (make-list-to-string
        (remove-spaces-before-punctuation
         (remove-extra-commas
          (append x '(*period*))))) :end 2 ;; only capitalize the first word in sentence
          )
      )
  )

(defun make-list-to-string (x)
  (string-trim " "
               (format nil "~{ ~A~}" x)
               )
  )
  
(defun remove-extra-commas (x)
  (cond ((null x) nil)
        ((and (null (rest x)) (equal '*comma* (first x))) nil) ; sentences don't end with commas
        ((and (equal '*comma* (first x)) (equal '*comma* (second x))) (remove-extra-commas (rest x)))
        ((and (equal '*comma* (first x)) (equal '*period* (second x))) (remove-extra-commas (rest x)))
        (T (cons (first x) (remove-extra-commas (rest x))))
        )
  )

(defun remove-spaces-before-punctuation (x)
  (cond ((null x) nil)
        ((null (rest x)) (first x))
        ((equal '*comma* (second x))
         (cons (concatenate 'string (first x) ",") (remove-spaces-before-punctuation (cddr x))))
        ((equal '*period* (second x))
          (cons (concatenate 'string (first x) ".") (remove-spaces-before-punctuation (cddr x))))
        (T (cons (first x) (remove-spaces-before-punctuation (rest x))))
        )
  )

(defun post-process-paragraph (x)
  (string-trim " " (concatenate 'string (format nil "~{ ~A~}" x)))
  )



(defun setup-ns-embedding ()
  (case (random 3)
    (0 (setq do-embed 'agt*) )
    (1 (setq do-embed 'obj*) )
    (2 (setq do-embed 'loc*) )
    )
  )

;; This could use simplification and decomp
(defun gen-with-embedding (node)
  (if (null (get node 'lab)) ;; only generate if we haven't already
      (progn
        ;; Need to label the semantic network with the node in the grammar network where
        ;; you want generation to start.  In this case, "s"
	(setf (get node 'lab) 's)
        (setq embed-count 3)
        (setup-ns-embedding)  ;; sets the global symbol for ns embedding
	(let* ((node-to-embed (get-random-ungenerated-structure-from-list (get (get node 'agt) 'agt*)))
	       (time-preposition (cond ((null node-to-embed) nil)
				       ((= (get node 'time) (get node-to-embed 'time)) "while")
				       ((= (1- (get node 'time)) (get node-to-embed 'time)) "when")
                                       ;; also set 'before-time in the main clause node
                                       ((< (get node 'time) (get node-to-embed 'time))
                                        (setf (get node 'before-time) (get node-to-embed 'time)) "before")
                                       ;; also set 'after-time in the main clause node
				       (T (setf (get node 'after-time) (get node-to-embed 'time)) "after"))))
	  (cond ((null node-to-embed) (gen node))
		(T (gen-time-preposition node node-to-embed time-preposition)))
          )
        )
      )
  )

(defun gen-time-preposition (node node-to-embed time-preposition)
  (format t "embedding node ~d into node ~d~%" node-to-embed node)
  (case (random 3)
    (0 (decf embed-count) (setf (get node-to-embed 'lab) 's)  (append (list time-preposition) (gen node-to-embed) '(*comma*) (gen node))) ;; before
    (1 (decf embed-count) (setf (get node-to-embed 'lab) 's) (append (gen node) (list time-preposition) (gen node-to-embed)) ) ;; after
    (2 (gen node))) ;; 1/3 of the time, don't embed at all
  )


;; assume that we're given a node list to generate
(defun gen-with-embedding-new (node-list)
  (cond ((nodes-all-same-time node-list)) ;; then split anywhere and use while
	((nodes-find-time-split node-list))
	)
  )

(defun nodes-find-split-points (node-list)
  (cond ((null (rest node-list)) nil) ;; last node in the list, so return nil
	((= (get (first node-list) 'time) (get (second node-list) 'time))
	 (nodes-find-split-points (rest node-list)))
	(t (append
	    (list (second node-list))
	    (nodes-find-split-points (rest node-list))))  ;; times weren't the same
	)
  )

;; test
;;(nodes-find-split-points '(c1 c3 c10 c14))


(let* ((node-list '(c1 c3 c10 c14))
       (split-points (nodes-find-split-points node-list))
       (split-point (if split-points (get-random-nth (nodes-find-split-points node-list))))
       (split-index (if split-points (1- (position split-point node-list)))))
  (cond ((null split-points)
	 (gen (make-subord-clause-semantic-node node node-to-embed 'l-while)))
	(list (subseq node-list 0 split-index)
	      (subseq node-list split-index))
	)
  )

	  
;; makes a new semantic node for a compound sentence with a subordinate clause
;; ready to generate ... right now set to generate on 'cmp4 from the grammar
(defun make-subord-clause-semantic-node (node node-to-embed prep-l-node)
  (let ((new-compound-sym (newsym 'compound-sent))
        (new-subord-sym (newsym 'subord-clause)))
    (setf (symbol-plist new-compound-sym) (list 's node 'subord-cl new-subord-sym 'lab 'cmp4 'comma 'c))
    (setf (symbol-plist new-subord-sym) (list 'tok prep-l-node 's node-to-embed 'p-subord 'p))
    new-compound-sym)
  )

  
;; makes a new semantic node for a compound sentence with a subordinate clause
;; ready to generate ... right now set to generate on 'cmp4 from the grammar
(defun make-subord-clause-semantic-node2 (conj-node1 conj-node2 prep-l-node)
  (let ((new-compound-sym (newsym 'compound-sent))
        (new-subord-sym (newsym 'subord-clause)))
    (setf (symbol-plist new-compound-sym) (list 'conj-gen0 conj-node1 'subord-cl new-subord-sym 'lab 'cmp4 'comma 'c))
    (setf (symbol-plist new-subord-sym) (list 'tok prep-l-node 'conj-gen0 conj-node2 'p-subord 'p))
    new-compound-sym)
  )




;; eventually you would like to swap these half of the time
(defun do-embed-time-relation (node node-to-embed)
  ;; uses the grammar!  and creates semantic nodes.
  (cond ((null node-to-embed) nil)
	((= (get node 'time) (get node-to-embed 'time))
         (gen (make-subord-clause-semantic-node node node-to-embed 'l-while)))
        ((= (1- (get node 'time)) (get node-to-embed 'time)) 
         (gen (make-subord-clause-semantic-node node node-to-embed 'l-while)))
        )
  )

;; test 
(progn (reset-property-list)
      (gen (make-subord-clause-semantic-node2 'embed-test1 'embed-test2 'l-while)))

;; test of conjunction stuff
;;(progn (reset-property-list)
;;      (gen 'conj-test))



(defun reset-and-gen () 
  (reset-property-list)
  (setq clause-generate-probability 50 do-embed nil generated-nodes '() gen-time 1)
  (post-process-paragraph (remove nil (gen-random-from-list '(c17 c3 c10 c14 c1))))
  )

(reset-and-gen)


;; MORE NOTES

;; in the original paper, I think john is really an agent, not a
;; dative.  and why do they have a dative at the start of a sentence?
;; in the paper it says that dative is supposed to be a "deep case
;; relation". maybe this is meant for sentences like "it looks like"
;; or "it seems?"  but I noticed that in Table VI, the larger semantic
;; structure, they have john in the AGT relation with "see" instead of
;; as a dative.  so, the table III dative must be a bug.  going to
;; just use AGT for john, so below, notice for C1 it says (agt c2)
;; instead of (dat c2).

;; to test:
;; (get 'vp0 'paths)

;; Can view the property list for a symbol s with (symbol-plist 's)
;; Can clear the property list for a symbol with (setf (symbol-plist 's) nil)
;; can also set the property list using this too!

;; Some tests of functions
;; (apply 'nbr (list 'c3))
;; (get-matching-rule 'c3  '((dat pred) (agt pred) ) )

;; (progn (reset-property-list) (voice 'e1) (form 'e1) (aspect 'e1) (tense 'e1) (get 'e1 'agt))
;; (progn (reset-property-list) (voice 'c1) (form 'c1) (aspect 'c1) (tense 'c1) (get 'c1 'subj))


;; test of remove extra commas
;(remove-extra-commas '(*comma* *comma* b c))
  
; 10-7-2017 delete me 
;(defun post-process-clause (x)
;  (list (string-trim " " (concatenate 'string (format nil "~{ ~A~}" x) *comma*)))
                                        ;  )

; does some post-processing on the output
;; (defun post-process-sentence (x)
;;   (if x
;;       (concatenate 'string (string-capitalize (string-trim " " (format nil "~{ ~A~}" x)) :end 2) ".")
;;       )
;;   )
