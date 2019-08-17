;; This code generates a semantic network using Conceptual Dependency structure
;; It can only deal with atrans right now. 
;; Now it works for generating once. Still have bug matching g-nodes when it generates from multiple cds and frameworks
;; Changed the format of the "framework" expression to make code more elegant
;; Added the "special requirement" part of the framework

(defun generate (cd frame)
  (let (
        (agtsym (newsym 'g))
        (objsym (newsym 'g))
        (lexptrsym (newsym 'g))
        (iobjsym (newsym 'g))
        (prepsym (newsym 'g))
        )
    (apply
     'append
     (mapcar
      #'(lambda (x)  
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
      ))
    
    )
  )
 
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

(generate *cd1* *frame2*)
