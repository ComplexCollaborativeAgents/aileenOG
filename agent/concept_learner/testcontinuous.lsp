;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: testgeneralization.lsp
;;;;    System: 
;;;;    Author: Will Hancock
;;;;   Created: July 13, 2021 13:04:34
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Tuesday, July 13, 2021 at 11:27:28 by Hancock
;;;; ----------------------------------------------------------------------------

(in-package :cl-user)

; (require :asdf)


; (load "server.lsp") ;;;Should probably move this to asdf at some point

(in-package :aileen)


(defparameter *threshold* .6)

; mt ids for concept
(defparameter *r-near* '(11 12 13))
(defparameter *concept-symbol* 'd::rNear)
(defparameter *concept-gpool* (get-concept-gpool *concept-symbol*))
(defparameter *probe-mt* 'd::query-facts)
(defparameter *probe-dist* 5.0)

(defparameter *filter-expression* (list *concept-symbol* 'd::?one 'd::?two))



;;; positive example
(defparameter *test-case* 'd::( 

  (isa Obj11 CVCube)
  (isa Obj11 CVGreen)
  (isa Obj11 RCube)
  (isa Obj11 RGreen)

  (isa Obj22 CVCylinder)
  (isa Obj22 CVGreen)
  (isa Obj22 RCylinder)
  (isa Obj22 RGreen)

  (distanceBetween Obj11 Obj22 4.0)


  (dc Obj11 Obj22)
  (e Obj11 Obj22)


  ))

;;; negative nearness test case
(defparameter *negative-test-case* 'd::( 

  (isa Obj13 CVCube)
  (isa Obj13 CVYellow)
  (isa Obj13 RCube)
  (isa Obj13 RYellow)

  (isa Obj23 CVCylinder)
  (isa Obj23 CVGreen)
  (isa Obj23 RCylinder)
  (isa Obj23 RGreen)

  (distanceBetween Obj13 Obj23 99999.0)

  (dc Obj13 Obj23)
  (s Obj13 Obj23)

  ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Testing code



(defun qlearning-main ()

  (load-test-flat-files)
  (create-test-generalizations *concept-symbol*)
  (run-query *test-case* *concept-gpool*)
  )

(defun load-test-flat-files ()
  (fire:kr-file->kb (qrg:make-qrg-file-name
			    (qrg:make-qrg-path ".." "data")
			    "aileen-mt.krf")
	       :error-on-bad-exps? t :kb fire::*kb*)
  (cl-user::load-flatfiles-in-dir (qrg:make-qrg-path ".." "data" "continuous-q-learning")))

(defun create-test-generalizations (concept-symbol)
  ; (create-reasoning-symbol concept-symbol)
  (create-reasoning-predicate concept-symbol 2)
  (multiple-value-bind (gens examples) ;;do I need to add reasoning symbols?
    (create-gpool *r-near* (get-concept-gpool concept-symbol))
    (declare (ignore gens examples))
    )
  )


;;; first assume global distribution
;;; this will be called by a query
(defun test-probe (probe-dist &key (cheating-dists (list 3 4 5)))
  (let* ((mean (mean cheating-dists))
         (stddev (stddev cheating-dists)))
    (confidence probe-dist mean stddev)))





(defun run-query (facts gpool)

  (remove-facts-from-case *probe-mt*)

  (when (test-probe *probe-dist*)
    (format t "found to be near, appending near fact~%")
    (setf facts (append facts `(d::(near Obj11 Obj22))))
    )

  ; (format t "Filtering scene with facts ~a~%." facts)
  (filter-scene-by-expression facts *probe-mt* gpool nil *filter-expression*)

  )



(defun make-random-mt ()
  (intern (gensym "AlieenMT") :d))
  

(defun microtheory-by-id (id)
  (intern (format nil "AileenExp~D" id) :d))


(defun create-gpool (ids gpool)
  (cl-user::nuke-gpool gpool)
  (cl-user::setup-gpool gpool :threshold *threshold* :strategy :gel)  
  (dolist (id ids)
    (format t "generalizing ~s~%" id)
    (fire:tell-it `(d::sageSelectAndGeneralize
                    ,(microtheory-by-id id)
                    ,gpool)))
  (values (length (fire:ask-it `(d::gpoolGeneralization ,gpool ?num)))
          (length (fire:ask-it `(d::gpoolExample ,gpool ?num)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Quantity Reasoning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *test-mt* 'd::AileenExp11)

(defparameter *test-forms* 
  'd::((distanceBetween ?o1 ?o2 ?o3)))



(defun test-find-quantities ()
  (find-quantities *test-forms* *test-mt*))


(defun test-find-quantities-new ()

  (filter-scene-by-expression *test-case* *probe-mt* *concept-gpool* nil *filter-expression*)

  )


(defun process-sme ()

  (let ((sme sme::*sme*))




    )

  )


;;; return a list of facts
(defun find-quantities (forms mt)
  (mapcan (lambda (form)
    (mapcar 'car (fire::retrieve form :mt mt)))
    forms))


;;; this is making ugly assumptions; essentially
;;; it is ignoring structure mapping and assuming that
;;; we will ever only see one instantiation of a predicate
;;; e.g. (distanceBetween ... ... ...)
;;; to handle this more elegantly, we could invoke SME
(defun generate-quantity-symbols (forms mt gpool)

  (let* ((quantity-preds (find-quantities forms mt))
         (preds (remove-duplicates (mapcar 'car quantity-preds)))


        )

))


(defun get-quantity-preds-for-gpool (pred argfn gpool)
  (let ((preds (fire::retrieve pred :mt gpool)))
    (mapcar argfn preds)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prob Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun mean (points)
  (/ (reduce '+ points) (length points)))


(defun variance (points)
  (let ((mean (mean points)))
    (mean (mapcar (lambda (point)
                    (expt (- point mean) 2))
            points))))


(defun stddev (points)
  (sqrt (variance points)))


;;; given a point, what is the prob it belongs to this class?
;;;
(defun gaussian-density (point mean stddev)
  (* (/ 1 (* stddev (sqrt (* 2. pi))))
     (exp (* -.5 (expt (/ (- point mean) stddev) 2)))))




(defun confidence (point mean stddev &key (n-stddevs 2.))
  (let ((lower-bound (- mean (* n-stddevs stddev)))
        (upper-bound (+ mean (* n-stddevs stddev))))
    
    (format t "lower is ~s~%" lower-bound)
    (format t "upper is ~s~%" upper-bound)
    
    (and (>= point lower-bound)
         (<= point upper-bound))))



;;; one way to do this would be a two-step process.
;;; first, align using the distance function (args would be aligned)
;;; then, we need to answer the question, are we assuming we know possible
;;; classes (i.e. we know there is close and near), or are we just trying to learn
;;; a single concept.
;;;
;;; if we are just trying to learn a single concept, then we need to determine how
;;; to symbolize the distribution. For inform signals, we can be sure that
;;; the point supplied is in-distribution (barring an evil instructor)
;;; do we make the same assumption for probes? I think no; that would mean
;;; this problem is meaningless.
;;;
;;; I think for now we do something dumb. We just binarize the symbol problem,
;;; and say that either the probe is within sigma or it is not.
;;; we add this symbol to the case, and then re-probe. It is fair to make the assumption
;;; that inform will always be in-distribution, so we introduce the nearness symbol
;;; for each case in an inform lesson.
;;;
;;; in order to act in the world, we ground, and then sample from the distribution
;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
