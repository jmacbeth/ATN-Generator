;; This code is based on Simmons and Slocum 1972
;; This one explores tenses, aspects, moods, etc.
;; see atn_semantic_net.lisp for more notes.

(defun generate (cd frame) 
  (setq agtsym (newsym 'g)) ;;g0
  (setq objsym (newsym 'g)) ;;g1
  (setq lexptrsym (newsym 'g)) ;;g2
  (setq iobjsym (newsym 'g)) ;;g3
  (setq prepsym (newsym 'g)) ;;g4
  (apply 'append  (mapcar  #'(lambda (x)  
  (case (car x)
   ((agt)
   `((,agtsym (tok ,(caadr (assoc (cadr x) (cdr cd))) nbr sing det none pron none agt* (,lexptrsym) ns ()))) )
   ((obj) 
   `((,objsym (tok ,(caadr (assoc (cadr x) (cdr cd))) det indef nbr sing pron none obj* (,lexptrsym) ns ()))) )
   ((lexptr) ;; used mapcar to deal with the verb as well
    `((,lexptrsym (tok ,(cadr(assoc 'lexptr frame)) voice active form none aspect none auxverb none tense past mood indic aux none time 1 pron none agt (,agtsym) obj (,objsym) vs () )))
    )
  ((iobj) 
    (if (eq (third(assoc 'iobj frame)) nil)
         `((,iobjsym (tok ,(caadr (assoc (cadr x) (cdr cd))) nbr sing det none pron none obj* (,lexptrsym) ns ())) )
         `((,iobjsym (tok ,(caadr (assoc (cadr x) (cdr cd))) nbr sing det none pron none obj* (,lexptrsym) ns ()))
         (,prepsym (tok ,(second(third(assoc 'iobj frame))) obj ,iobjsym pobj p )))
      ))
    )
	       )
  frame
)))
 
(defparameter *cd1* '(atrans (actor (l-john)) (object (l-book)) (from (l-john)) (to (l-mary)) ))
;; changed the format of the "framework" expression to make code more elegant
(defparameter *frame1* '((agt actor) (obj object) (lexptr l-give)))

(defparameter *frame2* '((agt to) (obj object) (lexptr l-receive) (iobj actor (prep l-from)) )) ;; mary receive book from john


(defun newsym (sym)
  ;; create a new unique symbol in the sequence sym1 sym2 sym3 ...
  (let ((count (get sym 'usage-count)))
    (if (null count) (setf count 0))
    (setf (get sym 'usage-count) (1+ count))
    (intern (concatenate 'string (string sym)
			 (prin1-to-string count)))))

(setq plist (append (generate *cd1* *frame1*)
     '((l-give (pi "give" present "gives" past "gave" past-part "given" prog "giving" terminal t) )
        (l1 (pi "see" present "sees" past "saw" past-part "seen" prog "seeing" terminal t) )
       (l-book (pi "book" pron-agt "it" pron-pred "it" terminal t) )
       (l-john (pi "John" pron-agt "he" pron-pred "him" terminal t) )
     (l3 (pi "wrestle" present "wrestle" past "wrestled" past-part "wrestled" prog "wrestling" terminal t) )
     (l-mary (pi "Mary" pron-agt "she" pron-pred "her" terminal t) )
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
     (l-by (pi "by John" terminal t) )
     ;; GRAMMAR NETWORK

     ;; Here are more grammar states from pg 899 of the paper (top right)
     ;; to handle tenses.  Still have trouble dealing with passive voice
     ;; and sentence where the object is another act.  and still am not putting
     ;; "by" into the passive.
     (s (paths ((voice tfm))))
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
     (embed (terminal t)) )
    
     
     ))
     
     


;;(print plist)
(defun reset-property-list () (mapcar 
#'(lambda (x) 
(setf (symbol-plist (first x)) (copy-list (second x))))
    plist
)
 )
 


;; GENERATION FUNCTIONS
;; voice form aspect tense mood

;; voice, or some other of these verb form functions, has to assign
;; subj, otherwise the rest of the grammar doesn't work.  May eventually want to
;; stick to either agt or subj, not both.
(defun voice (structure)
  (if (equal 'passive (get structure 'voice))
      (progn
	(setf (get structure 'vs) ;; passive
	      (append '(l-be) (list (get (get structure 'tok) 'past-part) ) )) 
	;; real solution is to add a new semantic node for by and attach agent to it?  but this really mangles the semantic
	;; network.  (later: it's OK to mangle the semantic network
        ;; much of the code does that.
	(setf (get structure 'subj) (get structure 'obj))
        (setf (get structure 'agt) 'c20) ;; we should create cnode dynamically instead
	(setf (get structure 'obj) (get structure 'agt)) ;; replace the obj with by + agent

	)
      (progn
	(setf (get structure 'vs) ;; active
	      (list (get structure 'tok)))
	(setf (get structure 'subj) (get structure 'agt))
	;; leave obj alone
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

;; no longer need tense to stick 'will on first
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
  (prog1
      (cond ((and do-embed (= 0 (random 2)))
             (setf (get structure 'embed) t) (get structure 'ns)) ;; try to do the embedding. no pronoun
	    ((equal 'tok (get structure 'pron-tok)) (pron-gen structure)) ;; do the pronoun
	    (T (setf (get structure 'pron-tok) 'tok) (get structure 'ns)) ;; no pronoun
	    )
    (setf (get structure 'ns) '()) ;; need to clear 'ns, otherwise get "Mary Mary Mary ..."
    (if (= 0 (random 4)) (setf (get structure 'pron-tok) nil)) ;; for more variety, occasionally kill the pronoun generation
    (setf (get structure 'pron-type) nil) ;; always reset the pron-type back to nil, otherwise
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
  (get structure 'vs) ;; already a list
  )

;; before I had this so that it was putting the print image of the preposition
;; into the 'ns for the structure.  and this created a problem "A WITH BOTTLE"
;; when interacting with det and ns, the other terminal functions. Now the code just generates the
;; print image of the preposition as part of gen, which correctly creates "with a bottle"

(defun pobj (structure)
  (list (get (get structure 'tok) 'pi))  ;; now this is happening at the pobj node
  )

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

;; deals with verb phrases as object
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

;; working on generated embeddings here 
(defun get-random-ungenerated-structure-from-list (x)
  (if (null x) nil
      (let ((random-member (get-random-nth x)))
	(cond ((null (get random-member 'lab)) random-member) ;; not generated yet.  Good.
	      (T (get-random-ungenerated-structure-from-list (remove random-member x)))))
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


;;;;; SEMANTIC / GRAMMAR MATCHING AND GENERATION FUNCTIONS 

(defun generate-unary-rule (structure rule)
  (setf (get structure 'lab) (car rule))
  (format t "Match was a unary rule, so relabeling structure")
  (format t " ~a to ~a and calling ~a on it (if bound) and then calling gen on it recursively.~%" structure (car rule) (car rule))
  ;; needed to add this... this code to call the function and then generate is in many places... should we
  ;; move the function call to be at the top of generate?
  (if (fboundp (car rule))  (funcall (car rule) structure) )
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
       (if (fboundp (car rule))  (funcall (car rule) k) )
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
  (cond
    ;; don't really need this first line at the moment, since i'm outputting a print image below
    ((setq j (get structure 'terminal)) (print "terminal node. exiting.") (list j))

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

(defun gen-random-from-list (node-list)
  (gen-from-list (shuffle-list node-list))
  )

; does some post-processing on the output
;; (defun post-process-sentence (x)
;;   (if x
;;       (concatenate 'string (string-capitalize (string-trim " " (format nil "~{ ~A~}" x)) :end 2) ".")
;;       )
;;   )

(defun post-process-sentence (x)
  (if x
      (string-capitalize
       (make-list-to-string
        (remove-spaces-before-punctuation
         (remove-extra-commas
          (append x '(*period*))))) :end 2
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


;(remove-extra-commas '("champagne" *period*))

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


  
  ;  test.  delete me
;(remove-extra-commas '(*comma* *comma* b c))
  
(defun post-process-paragraph (x)
  (string-trim " " (concatenate 'string (format nil "~{ ~A~}" x)))
  )

; 10-7-2017 delete me 
;(defun post-process-clause (x)
;  (list (string-trim " " (concatenate 'string (format nil "~{ ~A~}" x) *comma*)))
;  )

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
	(setf (get node 'lab) 's)
        (setup-ns-embedding)  ;; sets the global symbol for ns embedding
	(let* ((node-to-embed (get-random-ungenerated-structure-from-list (get (get node 'agt) 'agt*)))
	       (time-preposition (cond ((null node-to-embed) nil)
				       ((= (get node 'time) (get node-to-embed 'time)) "while")
				       ((= (1- (get node 'time)) (get node-to-embed 'time)) "when")
                                       ((< (get node 'time) (get node-to-embed 'time)) "before")
				       (T "after"))))
	  (cond ((null node-to-embed) (gen node))
		(T (format t "embedding node ~d into node ~d~%" node-to-embed node)
		   (case (random 3)
		     (0 (setf (get node-to-embed 'lab) 's) (append (list time-preposition) (gen node-to-embed) '(*comma*) (gen node))) ;; before
		     (1 (setf (get node-to-embed 'lab) 's) (append (gen node) (list time-preposition) (gen node-to-embed))) ;; after
		     (2 (gen node))))
		)
	  )
	)
      )
  )

(defun reset-and-gen () 
  (reset-property-list)
  (setq clause-generate-probability 50 do-embed nil generated-nodes '())
  ;; Need to label the semantic network with the node in the grammar network where
  ;; you want generation to start.
  ;; c17 c3 c10 c14 c1
  (setq story-output (post-process-paragraph (remove nil (gen-random-from-list '(g2)))))
  (format t "~%~%~%           ** STORY OUTPUT **~%~%")
  (format t story-output)
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
