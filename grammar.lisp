     ;; GRAMMAR NETWORK

     ;; Here are more grammar states from pg 899 of the paper (top right)
     ;; to handle tenses.  Still have trouble dealing with passive voice
     ;; and sentence where the object is another act.  and still am not putting
     ;; "by" into the passive.
     ((s (paths ((voice tfm))))
     (tfm (paths ((form asp))))
     (asp (paths ((aspect auxv))))
     (auxv (paths ((auxverb tns)))) ;; there's another symbol in grammar called aux... would like to get rid of that one
     (tns (paths ((tense prop))))
     (prop (paths ((mood indic)))) ;; the paper has t here instead of indic.  assume indic
     (indic (paths ((s1))))
     (interrog (paths ((prevb s1))))
     (s1 (paths ((subj pred))))

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
     
     (pred (paths ((pron pron-pred)))) ;; added extra step for pronoun labeling
     (pron-pred (paths ((man vp0) (vp0))))
     (vp0 (paths ((aux vp1)))) ;; needed to leave this aux in ,
                               ;; but now try to remove, now that i have fix for agt as part of object
     (vp1 (paths ((vs vp2))))
     (vp2 (paths ((obj vp3) (vp3))))
     (vp3 (paths ((iobj vp4) (vp4)))) ;; added April 2019 for Mary received a book from John
     (vp4 (paths ((inst vp5) (vp5))))
     (vp5 (paths ((loc vp6) (vp6)))) ;; now can have either or both inst and loc in a vp
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
     (obj-pred (paths ((obj-form pred))))
  
     ;; generating prepositional phrases, removed the pobj from elsewhere.  going to generate these separately here
     (pp0 (paths ((pobj pp1))))
     (pp1 (paths ((obj pp2))))
     (pp2 (paths ((t))))
     ;; and these for generating locations, instruments
     (loc (paths ((pp0))))
     (iobj  (paths ((pp0))))
     (inst (paths ((pp0))))
     (dat (paths ((pp0))))
     ;; terminal symbols in the grammar, maybe just having paths be nil is another option
     (pobj (terminal t)) (nbr (terminal t )) (det (terminal t)) (ns (terminal t))
     (man (terminal t)) (aux (terminal t)) (vs (terminal t)) (voice (terminal t))
     (form (terminal t)) (aspect (terminal t)) (auxverb (terminal t)) (tense (terminal t)) (mood (terminal t))
     (prevb (terminal t)) (pron (terminal t)) (end-punct (terminal t)) (obj-form (terminal t))
     (obj-verb-no-agt (terminal t)) (modifier (terminal t)) (conjunction (terminal t))  (man (terminal t))
     (embed (terminal t)))
