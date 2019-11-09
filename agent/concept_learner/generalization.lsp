;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: generalization.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: November  6, 2019 14:54:11
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Friday, November  8, 2019 at 16:04:14 by klenk
;;;; ----------------------------------------------------------------------------

(in-package :cl-user)

(load "analogystack/qrgsetup.lsp")
(require-module "fire" :fire)

(defun make-reasoner ()
  (fire:open-or-create-kb :kb-path (qrg::make-qrg-path "planb" "kbs" "nextkb"))
  (fire:in-reasoner (fire:make-reasoner 'concept)))

(defparameter *r-cube* '(1 2))
(defparameter *r-cube-context* 'r-cube-gpool)
(defparameter *r-green* '(2 3))
(defparameter *r-green-context* 'r-green-gpool)
(defparameter *r-cylinder* '(3 4))
(defparameter *r-cylinder-context* 'r-cylinder-gpool)
(defparameter *r-left* '(5 6 7))
(defparameter *r-left-context* 'r-left-gpool)
(defparameter *r-on* '(8 9 10))
(defparameter *r-on-context* 'r-on-gpool)

(defun generalization-of-concepts-of-aileen ()
  (make-reasoner)
  (format t "~%loading ~s" (qrg:make-qrg-file-name
			    (qrg:make-qrg-path ".." "data")
			    "aileen-mt.krf"))
;; Definitions must be in the kb, not just working memory.  
  (fire:kr-file->kb (qrg:make-qrg-file-name
			    (qrg:make-qrg-path ".." "data")
			    "aileen-mt.krf")
	       :error-on-bad-exps? t :kb fire::*kb*)
  (load-flatfiles-in-dir-into-wm (qrg:make-qrg-path ".." "data"))
  (create-gpool *r-cube* *r-cube-context*)
  (create-gpool *r-green* *r-green-context*)
  (create-gpool *r-cylinder* *r-cylinder-context*)  
  (dotimes (n 5)
    (compare-random-object-with-gpools
     (list *r-cube-context* *r-green-context* *r-cylinder-context*)))

  (format t "~%~%~% testing spatial relationships")
  (create-gpool *r-left* *r-left-context*)
  (create-gpool *r-on* *r-on-context*)
  (dotimes (n 15)
    (compare-random-two-obj-with-gpools
     (list *r-left-context* *r-on-context*)))
  )

(defun make-random-object-facts (&key (propositions
				       '((CVCube CVCylinder CVSphere)
					 (CVBlue CVGreen CVRed))))
  (let ((obj (intern (gensym "Obj") :d)))
    (values
     (mapcar #'(lambda (props)
		 `(isa ,obj ,(nth (random (length props)) props)))
	     propositions)
     obj)))

(defun make-random-mt ()
  (intern (gensym "AlieenMT") :d))
  

(defun microtheory-by-id (id)
  (intern (format nil "AileenExp~D" id) :d))
  
(defun create-gpool (ids gpool)
  (fire::create-gpool-if-needed fire:*reasoner* gpool)
  (fire:tell-it `(wmGpoolSelectStrategy ,gpool :macfac) )
  (fire:tell-it `(wmGpoolUseProbability ,gpool True) )
  (fire:tell-it `(wmGpoolProbabilityCutoff ,gpool 0.2)  )
  (fire:tell-it `(wmGpoolAssimilationThreshold ,gpool 0) )
  (fire:tell-it `(nukeGpool ,gpool))
  (dolist (id ids)
    (fire:tell-it `(sageWMSelectAndGeneralize
		    ,(microtheory-by-id id)
		    ,gpool)))
  (format t "~%Gpool ~A has ~d generalizations and ~d exemplars"
	  gpool
	  (length (fire:ask-it `(wmGpoolGeneralization ,gpool ?z ?num)))
	  (length (fire:ask-it `(wmGpoolExample ,gpool ?z ?num)))))


(defun compare-random-object-with-gpools (gpools)
  (let ((exp (make-random-mt))
	(facts (make-random-object-facts)))
    (fire:tell-all facts fire:*reasoner* :assumption exp)
    (format t "~%Categorizing ~A object: ~A" exp facts)
    (dolist (gpool gpools)
      (format t "~% ~A : ~A"
	      gpool
	      (fire:ask-it `(and (sageWMSelect ,exp ,gpool ?ret ?mapping)
				 (structuralEvaluationScoreOf ?mapping ?score))
			   :response '(?score))))
    ))

(defun make-random-two-object-facts (&key (relations
					   '((w n )
					     (ec dc))))
  (multiple-value-bind (o1-facts o1)
      (make-random-object-facts)
    (multiple-value-bind (o2-facts o2)
	(make-random-object-facts)
      (append o1-facts
	      o2-facts
	      (mapcar #'(lambda (rels)
			  (list
			   (nth (random (length rels)) rels)
			   o1 o2))
		      relations)))))

(defun compare-random-two-obj-with-gpools (gpools)
  (let ((exp (make-random-mt))
	(facts (make-random-two-object-facts)))
    (fire:tell-all facts fire:*reasoner* :assumption exp)
    (format t "~%Categorizing ~A object: ~A" exp facts)
    (dolist (gpool gpools)
      (format t "~% ~A : ~A"
	      gpool
	      (fire:ask-it `(and (sageWMSelect ,exp ,gpool ?ret ?mapping)
				 (structuralEvaluationScoreOf ?mapping ?score))
			   :response '(?score))))
    ))
    
  


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
