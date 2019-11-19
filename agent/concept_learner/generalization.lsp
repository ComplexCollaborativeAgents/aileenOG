;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: generalization.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: November  6, 2019 14:54:11
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Monday, November 18, 2019 at 20:55:49 by klenk
;;;; ----------------------------------------------------------------------------

(in-package :aileen)

;; (load "analogystack/qrgsetup.lsp")
;; (require-module "fire" :fire)

;; Need to talk to QRG folks about the best way to figure out settings here
(setq fire::*default-sagewm-threshold* 0)
(setq fire::*default-sagewm-prob-cutoff* .2)


(defun make-reasoner ()
  (fire:open-or-create-kb
   :kb-path
   (qrg::make-qrg-path "planb" "kbs" "nextkb"))
  (fire:in-reasoner (fire:make-reasoner 'concept)))


(defun create-reasoning-symbol (symbol)
  (let ((gpool (intern (format nil "~AMt" symbol) :d)))
    (cl-user::nuke-gpool gpool)
    (cl-user::setup-gpool gpool)  
    (fire:kb-store `d::(genls ,aileen::symbol AileenReasoningSymbol) :mt 'd::BaseKB)
    (fire:kb-store `d::(isa ,aileen::symbol Collection) :mt 'd::BaseKB)
    (values (length (fire:ask-it `d::(genls ?x AileenReasoningSymbol)))
	    gpool)))

(defun create-reasoning-predicate (pred arity)
  (let ((gpool (intern (format nil "~AMt" (symbol-name pred)) :d)))
    (cl-user::nuke-gpool gpool)
    (cl-user::setup-gpool gpool)
    (fire:kb-store `d::(isa ,aileen::pred AileenReasoningPredicate) :mt 'd::BaseKB)
    (fire:kb-store `d::(arity ,aileen::pred
			      ,aileen::arity) :mt 'd::BaseKB)
    (values (length (fire:ask-it `d::(isa ?x AileenReasoningPredicate)))
	    gpool)))


;;; Assumes gpool is aready created
(defun add-case-to-gpool (facts context gpool)
  (fire:tell-all facts fire:*reasoner* :assumption context)
  (fire:tell-it `(d::sageSelectAndGeneralize ,context ,gpool))
  (values (length (fire:ask-it `(d::gpoolGeneralization ,gpool ?z ?num) :context gpool))
	  (length (fire:ask-it `(d::gpoolExample ,gpool ?z ?num) :context gpool))) )

(defun store-facts-in-case (facts context)
  (dolist (fact facts)
;    (fire:tell-it fact :context context))
    (fire:kb-store fact :mt context)
    (fire:kb-store `(d::isa ,context d::AileenCaseMt) :mt context))
  )
  
;;; this may go away
(defun match-case-against-gpool (facts context gpool pattern)
  (store-facts-in-case facts context)
  (fire:ask-it `(d::reverseCIsAllowed
		 (d::and
		  (d::sageSelect  ,context ,gpool ?ret ?mapping)
		  (d::reverseCandidateInferenceOf ?ci ?mapping)
		  (d::candidateInferenceContent ?ci ,pattern)))
	       :context gpool :response pattern))


(defun filter-scene-by-expression (facts context gpool prevmatches pattern)
  (cond ((object-filter? pattern)
	 (filter-scene-by-expression-obj facts context gpool prevmatches pattern))
	((relation-filter? pattern)
	 (filter-scene-by-expression-rel facts context gpool prevmatches pattern))
	(t (assert nil))))

(defun object-filter? (pattern)
  (and (= (length pattern) 3) 
       (eql (car pattern) 'd::isa)
       (fire:ask-it `(d::genls ,(third pattern) d::AileenReasoningSymbol))))

(defun relation-filter? (pattern)
  (fire:ask-it `(d::isa ,(car pattern) d::AileenReasoningPredicate)))

(defun filter-scene-by-expression-obj (facts context gpool prevmatches pattern)
  (store-facts-in-case facts context)
  (let* ((collection (third pattern))
	 (objs
	  (remove-if-not
	   #'(lambda (obj)
	       (fire:tell-it `(d::constructCaseInWM (d::MinimalCaseFromMtFn ,obj ,context)))
	       (fire:tell-it `(d::copyWMCaseToKB (d::MinimalCaseFromMtFn ,obj ,context)
						 (d::MinimalCaseFromMtFn ,obj ,context)))
	       (fire:ask-it
		`(d::reverseCIsAllowed
		  (d::and
		   (d::sageSelect (d::MinimalCaseFromMtFn ,obj ,context)
					   ,gpool ?ret ?mapping)
			  (d::reverseCandidateInferenceOf ?ci ?mapping)
			  (d::candidateInferenceContent ?ci (d::isa ,obj ,collection))))
		:context gpool))
	   (if prevmatches
	       (fire:ask-it `d::(localOnly
				 (wmOnly ,aileen::prevmatches))
			    :context context :response (second pattern))
	       (reverse (objs-in-context context))))))
    (dolist (obj objs objs) ;;;the fact to working memory and return the list of objects
      (fire:tell-it `(d::isa ,obj (third pattern) :context context)))))

(defun filter-scene-by-expression-rel (facts context gpool prevmatches pattern)
  (assert nil)
  )


(defun objs-in-context (context)
  (fire::ask-it `d::(and (isa ?col AileenCVSymbol)
			 (ist-Information ,aileen::context
					  (isa ?x ?col)))
	         :response 'd::?x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Testing code




 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
