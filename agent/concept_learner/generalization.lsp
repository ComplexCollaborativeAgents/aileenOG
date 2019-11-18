;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: generalization.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: November  6, 2019 14:54:11
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Monday, November 18, 2019 at 14:30:20 by klenk
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
    (fire::create-gpool-if-needed fire:*reasoner* gpool)  ;;; Currently gpools are all in WM
    (fire:kb-store `d::(genls ,aileen::symbol AileenReasoningSymbol) :mt 'd::BaseKB)
    (fire:kb-store `d::(isa ,aileen::symbol Collection) :mt 'd::BaseKB)
    (values (length (fire:ask-it `d::(genls ?x AileenReasoningSymbol)))
	    gpool)))

(defun create-reasoning-predicate (pred)
  (let ((gpool (intern (format nil "~AMt" (symbol-name pred)) :d)))
    (fire::create-gpool-if-needed fire:*reasoner* gpool)  ;;; Currently gpools are all in WM
    (fire:kb-store `d::(genls ,aileen::pred AileenReasoningPredicate) :mt 'd::BaseKB)
    (values (length (fire:ask-it `d::(isa ?x AileenReasoningPredicate)))
	    gpool)))


;;; Assumes gpool is aready created
(defun add-case-to-gpool (facts context gpool)
  (fire:tell-all facts fire:*reasoner* :assumption context)
  (fire:tell-it `(d::sageWMSelectAndGeneralize ,context ,gpool))
  (values (length (fire:ask-it `(d::wmGpoolGeneralization ,gpool ?z ?num) :context gpool))
	  (length (fire:ask-it `(d::wmGpoolExample ,gpool ?z ?num) :context gpool))) )

;;; this may go away
(defun match-case-against-gpool (facts context gpool pattern)
  (fire:tell-all facts fire:*reasoner* :assumption context)
  (fire:ask-it `(d::reverseCIsAllowed
		 (d::and (d::sageWMSelect ,context ,gpool ?ret ?mapping)
			 (d::reverseCandidateInferenceOf ?ci ?mapping)
			 (d::candidateInferenceContent ?ci ,pattern)))
	       :context gpool :response pattern))


(defun filter-scene-by-expression (facts context gpool prevmatches pattern)
  (cond ((object-filter? pattern)
	 (filter-scene-by-expression-obj facts context gpool prevmatches pattern))
	((relation-filter? patterm)
	 (filter-scene-by-expression-rel facts context gpool prevmatches pattern))
	(t (assert nil))))

(defun object-filter? (pattern)
  (and (= (length pattern) 3) 
       (eql (car pattern) 'd::isa)
       (fire:ask-it `(d::genls ,(third pattern) d::AileenReasoningSymqbol))))

(defun relation-filter? (pattern)
  (fire:ask-it `(d::isa ,(car pattern) d::AileenReasoningPredicate)))

(defun filter-scene-by-expression-obj (facts context gpool prevmatches pattern)
  (fire:tell-all facts fire:*reasoner* :assumption context)
  (let ((objs
	 (remove-if-not
	  #'(lambda (obj)
	      (fire:ask-it
	       `(d::reverseCIsAllowed
		 (d::and (d::sageWMSelect (d::MinimalCaseFromMtFn ,obj , context)
					  ,gpool ?ret ?mapping)
			 (d::reverseCandidateInferenceOf ?ci ?mapping)
			 (d::candidateInferenceContent ?ci ,pattern)))
	       :context gpool :response pattern))
	  (if prevmatches
	      (fire:ask-it `d::(localOnly
				(wmOnly ,aileen::prevmatches))
			   :context context :response (second pattern))
	      (objs-in-context context)))))
  (dolist (obj objs objs) ;;;the fact to working memory and return the list of objects
    (fire:tell-it `(d::isa ,obj (third pattern) :context context)))))

(defun filter-scene-by-expression-rel (facts context gpool prevmatches pattern)
  (assert nil)
  )


(defun objs-in-context (context)
  (fire::ask-it 'd::(localOnly
		     (wmOnly (isa ?x ?y)))
		:context context :response 'd::(?x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Testing code




 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
