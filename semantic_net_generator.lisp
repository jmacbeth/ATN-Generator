;; This code generates a semantic network using Conceptual Dependency structure
;; It can only deal with atrans right now. 
;; Now it works for generating once. Still have bug matching g-nodes when it generates from multiple cds and frameworks
;; Changed the format of the "framework" expression to make code more elegant

(defun generate (cd frame) 
  (setq agtsym (newsym 'g)) ;;g0
  (setq objsym (newsym 'g)) ;;g1
  (setq lexptrsym (newsym 'g)) ;;g2
  (mapcar  #'(lambda (x)  
  (case (car x)
   ((agt)
   (append `(agtsym (tok ,(caadr (assoc (cadr x) (cdr cd))) nbr sing det none pron none agt* (,lexptrsym) ns ())) ))
   ((obj) 
   (append `(objsym (tok ,(caadr (assoc (cadr x) (cdr cd))) det indef nbr sing pron none obj* (,lexptrsym) ns ())) ))
   ((lexptr) ;; used mapcar to deal with the verb as well
   (append `(lexptrsym (tok ,(second frame) voice active form none aspect none auxverb none tense past mood indic aux none time 1 pron none agt (,agtsym) obj (,objsym) vs () )))
 )
 ))
  frame
))

 
(defparameter *cd1* '(atrans (actor (l-john)) (object (l-book))))
;; changed the format of the "framework" expression to make code more elegant
(defparameter *frame1* '((agt actor) (obj object) (lexptr l-give)))

(defparameter *cd2* '(atrans (actor (l-mary)) (object (l-beer))))
(defparameter *frame2* '((agt actor) (obj object) (lexptr l-drink)))


(defun newsym (sym)
  ;; create a new unique symbol in the sequence sym1 sym2 sym3 ...
  (let ((count (get sym 'usage-count)))
    (if (null count) (setf count 0))
    (setf (get sym 'usage-count) (1+ count))
    (intern (concatenate 'string (string sym)
       (prin1-to-string count)))))

(generate *cd1* *frame1*)
