;;This code generates a semantic network using Conceptual Dependency structure
;;It can only deal with atrans right now. 

(defun generate (cd frame) 
    (defparameter *cdlist* `((,(cadr cd) . ,(caaddr cd))
                          (,(cadddr cd) . ,(caar(last cd)))
                          ))
    (defparameter *framelist* `((,(car frame) . ,(cadr frame))
                          (,(caaar (last frame)) . ,(cadaar(last frame)))
                          (,(caar(last(car(last frame)))) . ,(cadar(last(car(last frame))))))
                          )
  (append `((,(newsym 'g) (tok ,(cdr(assoc 'lexptr *framelist*)) voice active form none aspect none auxverb none tense past mood indic aux none time 1 pron none agt ,(cdr(assoc (cdr(assoc 'agt *framelist*))*cdlist*)) obj ,(cdr(assoc (cdr(assoc 'obj *framelist*)) *cdlist*)) vs () ))) ;;atrans
       
 `((,(newsym 'g)  (tok ,(cdr(assoc 'actor *cdlist*)) nbr sing det none pron none agt* (g0) ns ()))) ;;actor
  `((,(newsym 'g)  (tok ,(cdr(assoc 'object *cdlist*)) det indef nbr sing pron none obj* (g0) ns ())) ;; object
   )))
 
(defparameter *cd1* '(atrans actor (l-john) object (l-book)))
(defparameter *frame1* '(lexptr l-give framework ((agt actor) (obj object))))

(defparameter *cd2* '(atrans actor (l-mary) object (l-beer)))
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
