;;This code generates a semantic network using Conceptual Dependency structure
;;It can only deal with atrans right now. 

(defun generate (cd frame) 
  (newsym 'g)
  (newsym 'g)
  (newsym 'g) 
  (mapcar  #'(lambda (x)  
  (case (car x)
   ((agt)
   (append `(g1 (tok ,(caadr (assoc (cadr x) (cdr cd))) nbr sing det none pron none agt* (g0) ns ())) ))
   ((obj) 
   (append `(g2 (tok ,(caadr (assoc (cadr x) (cdr cd))) det indef nbr sing pron none obj* (g0) ns ())) ))
   ((lexptr) ;; used mapcar to deal with the verb as well
   (append `(g0 (tok ,(second frame) voice active form none aspect none auxverb none tense past mood indic aux none time 1 pron none agt (g1) obj (g2) vs () )))
 )
 ))
;; added (lexptr l-node) pair to the alist, in order to use mapcar to create the node for atrans along with other nodes
  (push `(,(car frame) ,(second frame)) (car (last frame))) 
)) 
 
(defparameter *cd1* '(atrans (actor (l-john)) (object (l-book))))
(defparameter *frame1* '(lexptr l-give framework ((agt actor) (obj object))))

(defparameter *cd2* '(atrans (actor (l-mary)) (object (l-beer))))
(defparameter *frame2* '(lexptr l-drink framework ((agt actor) (obj object))))


(defun newsym (sym)
  ;; create a new unique symbol in the sequence sym1 sym2 sym3 ...
  (let ((count (get sym 'usage-count)))
    (if (null count) (setf count 0))
    (setf (get sym 'usage-count) (1+ count))
    (intern (concatenate 'string (string sym)
			 (prin1-to-string count)))))

(print(generate *cd1* *frame1*))
(print(generate *cd2* *frame2*))

