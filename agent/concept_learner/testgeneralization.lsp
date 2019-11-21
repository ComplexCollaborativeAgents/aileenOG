;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: testgeneralization.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: November 18, 2019 13:04:34
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Tuesday, November 19, 2019 at 14:26:43 by klenk
;;;; ----------------------------------------------------------------------------

(in-package :cl-user)

(require :asdf)

(load "lisp-unit/lisp-unit.asd")
(asdf:load-system :lisp-unit)

(load "server.lsp") ;;;Should probably move this to asdf at some point

(in-package :aileen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Testing code

(defun run-tests ()
  (make-reasoner)
  (load-test-flat-files)
  (setq lisp-unit::*print-failures* t lisp-unit::*print-errors* t)
  (lisp-unit:write-tap-to-file (lisp-unit:run-tests :all :aileen) "concept_test.tap")
  (clean-tests)
  )

(defun load-test-flat-files ()
  (fire:kr-file->kb (qrg:make-qrg-file-name
			    (qrg:make-qrg-path ".." "data")
			    "aileen-mt.krf")
	       :error-on-bad-exps? t :kb fire::*kb*)
  (cl-user::load-flatfiles-in-dir (qrg:make-qrg-path ".." "data")))
  

(lisp-unit:define-test concept-learner-object-generalization
  (lisp-unit:assert-equal 5 5)
  (make-reasoner)
  (generalization-of-concepts-aileen)
  (test-object-filter)
  )

(lisp-unit:define-test concept-learner-rel-generalization
  (lisp-unit:assert-equal 5 5)
  (make-reasoner)
  (generalization-of-rel-concepts-aileen)
  ;(test-rel-filter)
  )

;;; Automated testing

(defparameter *r-cube* '(1 2))
(defparameter *r-cube-context* 'd::r-cube-gpool)
(defparameter *r-green* '(2 3))
(defparameter *r-green-context* 'd::r-green-gpool)
(defparameter *r-cylinder* '(3 4))
(defparameter *r-cylinder-context* 'd::r-cylinder-gpool)
(defparameter *r-left* '(5 6 7))
(defparameter *r-left-context* 'd::r-left-gpool)
(defparameter *r-on* '(8 9 10))
(defparameter *r-on-context* 'd::r-on-gpool)
    
(defun test-object-filter ()
  ;;;Find the object
  (multiple-value-bind (facts1 obj1)
      (make-random-object-facts :propositions 'd::((CVCube)(CVBlue)))
    (multiple-value-bind (facts2 obj2)
	(make-random-object-facts :propositions 'd::((CVSphere)(CVRed)))
      (let ((objs (filter-scene-by-expression
		   (append facts1 facts2) (make-random-mt) *r-cube-context* nil 'd::(isa ?obj RCube))))
	(lisp-unit:assert-equal 1 (length objs))
	(lisp-unit:assert-true (member obj1 objs))
	(lisp-unit:assert-false (member obj2 objs))
       ))))

(defun generalization-of-concepts-aileen ()
  (create-reasoning-symbol 'd::RCube)
  (multiple-value-bind (gens examples) ;;do I need to add reasoning symbols?
      (create-gpool *r-cube* *r-cube-context*)
    (lisp-unit:assert-equal 1 gens)
    (lisp-unit:assert-equal 0 examples))
  (multiple-value-bind (facts obj)
      (make-random-object-facts :propositions 'd::((CVCube)(CVBlue)))
    (lisp-unit:assert-true 
     (match-case-against-gpool facts (make-random-mt) *r-cube-context* `(d::isa ,obj d::RCube))))
  (multiple-value-bind (facts obj)
      (make-random-object-facts :propositions 'd::((CVSphere)(CVBlue)))
    (lisp-unit:assert-false
     (match-case-against-gpool facts (make-random-mt) *r-cube-context* `(d::isa ,obj d::RCube)))))


(defun generalization-of-rel-concepts-aileen ()
  (multiple-value-bind (num gpool)
      (create-reasoning-predicate 'd::rLeft 2)
    (lisp-unit:assert-equal 1 num)
    (multiple-value-bind (gens examples) ;;do I need to add reasoning symbols?
	(create-gpool *r-left* gpool)
    (lisp-unit:assert-equal 1 gens)
    (lisp-unit:assert-equal 0 examples))
    (multiple-value-bind (facts objs)
	(make-random-two-object-facts :relations 'd::((w)(dc)))
      (lisp-unit:assert-true 
       (match-case-against-gpool facts (make-random-mt) gpool `(d::rLeft ,(car objs) ,(second objs)))))
    (multiple-value-bind (facts objs)
	(make-random-two-object-facts :relations 'd::((n)(ec)))
      (lisp-unit:assert-false
       (match-case-against-gpool facts (make-random-mt) gpool `(d::rLeft ,(car objs) ,(second objs)))))))

;;; This is not necessary once we have a smoke-kb
(defun clean-tests ()
  (fire:kb-forget (car (fire:retrieve-references 'd::RCube)))
  (fire:kb-forget `d::(genls common-lisp-user::rLeft AileenReasoningPredicate) :mt 'd::BaseKB)
  (dolist (mt (fire::ask-it 'd:(isa ?x d::AileenCaseMt) :response 'd::?x))
    (dolist (fact  (fire:ask-it '?x :context mt :response '?x))
      (fire:forget fact :context mt)))
  (dolist (fact (fire:ask-it `d::(ist-Information (MinimalCaseFromMtFn ?x ?y) ?z)))
    (fire:forget fact)))

(defun make-random-object-facts (&key (propositions
				       'd::((CVCube CVCylinder CVSphere)
					    (CVBlue CVGreen CVRed))))
  (let ((obj (intern (gensym "Obj") :d)))
    (values
     (mapcar #'(lambda (props)
		 `(d::isa ,obj ,(nth (random (length props)) props)))
	     propositions)
     obj)))

(defun make-random-mt ()
  (intern (gensym "AlieenMT") :d))
  

(defun microtheory-by-id (id)
  (intern (format nil "AileenExp~D" id) :d))


(defun create-gpool (ids gpool)
  (cl-user::nuke-gpool gpool)
  (cl-user::setup-gpool gpool :threshold 0.2 :strategy :gel)  
  (dolist (id ids)
    (fire:tell-it `(d::sageSelectAndGeneralize
		    ,(microtheory-by-id id)
		    ,gpool)))
  (values (length (fire:ask-it `(d::gpoolGeneralization ,gpool ?num)))
	  (length (fire:ask-it `(d::gpoolExample ,gpool ?num)))))

(defun compare-random-object-with-gpools (gpools)
  (let ((exp (make-random-mt))
	(facts (make-random-object-facts)))
    (fire:tell-all facts fire:*reasoner* :assumption exp)
    (format t "~%Categorizing ~A object: ~A" exp facts)
    (dolist (gpool gpools)
      (format t "~% ~A : ~A"
	      gpool
	      (fire:ask-it `(d::and (d::sageSelect ,exp ,gpool ?ret ?mapping)
				 (d::structuralEvaluationScoreOf ?mapping ?score))
			   :response '(?score))))
    ))

(defun make-random-two-object-facts (&key (relations
					   'd::((w n )
						(ec dc))))
  (multiple-value-bind (o1-facts o1)
      (make-random-object-facts)
    (multiple-value-bind (o2-facts o2)
	(make-random-object-facts)
      (values
       (append o1-facts
	       o2-facts
	       (mapcar #'(lambda (rels)
			   (list
			    (nth (random (length rels)) rels)
			    o1 o2))
		       relations))
       (list o1 o2)))))

(defun compare-random-two-obj-with-gpools (gpools)
  (let ((exp (make-random-mt))
	(facts (make-random-two-object-facts)))
    (fire:tell-all facts fire:*reasoner* :assumption exp)
    (format t "~%Categorizing ~A object: ~A" exp facts)
    (dolist (gpool gpools)
      (format t "~% ~A : ~A"
	      gpool
	      (fire:ask-it `(d::and (d::sageSelect ,exp ,gpool ?ret ?mapping)
				 (d::structuralEvaluationScoreOf ?mapping ?score))
			   :response '(?score))))
    ))
    
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
