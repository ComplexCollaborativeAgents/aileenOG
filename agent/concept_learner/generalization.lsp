;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: generalization.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: November  6, 2019 14:54:11
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Saturday, December 14, 2019 at 17:52:43 by klenk
;;;; ----------------------------------------------------------------------------

(in-package :aileen)

;; (load "analogystack/qrgsetup.lsp")
;; (require-module "fire" :fire)



(defun make-reasoner (&key (kbdir "nextkb"))
  (fire:open-or-create-kb
   :kb-path
   (qrg::make-qrg-path "planb" "kbs" kbdir))
  (fire:kr-file->kb (qrg:make-qrg-file-name
			    (qrg:make-qrg-path ".." "data")
			    "aileen-mt.krf")
	       :error-on-bad-exps? t :kb fire::*kb*)
  (fire:in-reasoner (fire:make-reasoner 'concept)))


(defun create-reasoning-symbol (symbol)
  (let ((gpool (get-concept-gpool symbol)))
    (cl-user::nuke-gpool gpool)
    (cl-user::setup-gpool gpool :threshold 0.2 :strategy :gel)  
    (fire:kb-store `d::(genls ,aileen::symbol AileenReasoningSymbol) :mt 'd::BaseKB)
    (fire:kb-store `d::(isa ,aileen::symbol Collection) :mt 'd::BaseKB)
    (values (1- (length (fire:ask-it `d::(genls ?x AileenReasoningSymbol)))) ;;due to genls identity
	    gpool)))

(defun create-reasoning-predicate (pred arity)
  (let ((gpool (get-concept-gpool pred)))
    (cl-user::nuke-gpool gpool)
    (cl-user::setup-gpool gpool :threshold 0.2 :strategy :gel)  
    (fire:kb-store `d::(isa ,aileen::pred AileenReasoningPredicate) :mt 'd::BaseKB)
    (fire:kb-store `d::(arity ,aileen::pred
			      ,aileen::arity) :mt 'd::BaseKB)
    (values (length (fire:ask-it `d::(isa ?x AileenReasoningPredicate)))
	    gpool)))


;;; Assumes gpool is aready created
(defun add-case-to-gpool (facts context gpool)
  (store-facts-in-case facts context)
  (fire:tell-it `(d::sageSelectAndGeneralize ,context ,gpool) :context gpool)
  (values (length (fire:ask-it `(d::kbOnly (d::gpoolGeneralization ,gpool ?num))))
	  (length (fire:ask-it `(d::kbOnly (d::gpoolExample ,gpool ?num)) ))) )

(defun store-facts-in-case (facts context)
  (remove-facts-from-case context)
  (dolist (fact facts)
    (fire:kb-store fact :mt context)
    (fire:kb-store `(d::isa ,context d::AileenCaseMt) :mt context))
  )

(defun remove-facts-from-case (context)
  (format t "Removing ~A~%" context)
  (dolist (fact (fire:retrieve-it '?x :context context :response '?x))
    (format t "Forgetting ~A in ~A~%" fact context)
    (fire:kb-forget fact :mt context)))
  
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
  (fire:clear-wm)
  (fire:clear-dgroup-caches)
  (cond
   ((or (eq (car pattern) 'd::and) (eq (car pattern) 'd::or))
    ;; boolean operation
    (let (result sub-result)
      (loop for item in (cdr pattern) do
            (setf sub-result (filter-scene-by-expression facts context gpool prevmatches item))
            (cond
             ((not result)
              (setf result sub-result))
             ((eq (car pattern) 'd::and)
              (setf result (intersection result sub-result)))
             ((eq (car pattern) 'd::or)
              (setf result (union result sub-result))))
            (when (and (not result) (eq (car pattern) 'd::and))
              (return)))
      result))
   ((eq (car pattern) 'd::not)
    ;; negation
    (let (objects negated)
      (setf objects (objs-in-context context))
      (setf negated (filter-scene-by-expression facts context gpool prevmatches (nth 1 pattern)))
      (loop for item in negated do
            (setf objects (remove item objects)))
      objects))
   ((object-filter? pattern)
    (filter-scene-by-expression-obj facts context gpool prevmatches pattern))
   ((relation-filter? pattern)
    (filter-scene-by-expression-rel facts context gpool prevmatches pattern))
   (t (error "Unknown pattern: ~a" pattern))))

(defun object-filter? (pattern)
  (and (= (length pattern) 3) 
       (eql (car pattern) 'd::isa)
       (fire:ask-it `(d::genls ,(third pattern) d::AileenReasoningSymbol))))

(defun relation-filter? (pattern)
  (fire:ask-it `(d::isa ,(car pattern) d::AileenReasoningPredicate)))

(defun filter-scene-by-expression-obj (facts context gpool prevmatches pattern)
  (store-facts-in-case facts context)
  (when (not gpool)
    (setf gpool (get-concept-gpool (third pattern))))
  (let* ((collection (third pattern))
	 (objs
	  (remove-if-not
	   #'(lambda (obj)
	       (remove-facts-from-case `(d::MinimalCaseFromMtFn ,obj ,context))
	       (fire:clear-dgroup-caches)	       
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
      (fire:tell-it `(d::isa ,obj ,(third pattern)) :context context))))

(defun get-concept-gpool (concept)
  (intern (format nil "~AMt" (symbol-name concept)) :d))

(defun filter-scene-by-expression-rel (facts context gpool prevmatches pattern)
  (assert (null prevmatches))
  (assert (and (listp pattern) (every #'atom pattern))) ;; no nested lists in pattern.
  (store-facts-in-case facts context)
  (when (not gpool)
    (setf gpool (get-concept-gpool (car pattern))))
  (cond ((null (vars-in-expr pattern))
	 (if (match-query-against-gpool context gpool pattern)
	     (list pattern)
	     nil))
	((vars-in-expr pattern)
	 (remove-if-not
	  #'(lambda (bound-pattern)
	      (match-query-against-gpool context gpool bound-pattern))
	  (mapcar 
	   #'(lambda (blist)
	       (fire::substitute-bindings pattern blist))
	   (make-possible-blists
	    (vars-in-expr pattern)
	    (objs-in-context context)))))))


(defun make-case-term (context objs)
  ;;; Union of all the minimal case fns
  (cons 'd::CaseUnionFn
	(mapcar #'(lambda (obj)
		    `(d::MinimalCaseFromMtFn ,obj ,context))
		objs)))

(defun match-query-against-gpool (context gpool pattern)
  (let* ((objs (objs-in-context context))
	 (case-term (make-case-term context (remove-if-not
					     #'(lambda (e) (find e objs))
					     pattern) )))
    (remove-facts-from-case case-term)
    (fire:clear-dgroup-caches)	       
    (fire:tell-it `(d::constructCaseInWM ,case-term))
    (fire:tell-it `(d::copyWMCaseToKB ,case-term ,case-term)) ;;could have an explicit query context here for easier clean up?
    (fire:ask-it
     `(d::reverseCIsAllowed
       (d::and
	(d::sageSelect ,case-term ,gpool ?ret ?mapping)
	(d::reverseCandidateInferenceOf ?ci ?mapping)
	(d::candidateInferenceContent ?ci ,pattern)))
     :context gpool)))

(defun make-possible-blists (vars objs &optional blist)
  (cond ((null vars) (list blist))
	((null objs) (assert nil))
	(t
	 (mapcan #'(lambda (obj)
		     (make-possible-blists
		      (cdr vars)
		      (remove obj objs)
		      (cons (cons (car vars) obj) blist)))
		 objs))))
  
(defun vars-in-expr (expr)
  (remove-if-not #'fire:variable? expr))

(defun objs-in-context (context)
  (fire::ask-it `d::(and (isa ?col AileenCVSymbol)
			 (ist-Information ,aileen::context
					  (isa ?x ?col)))
	         :response 'd::?x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Testing code




 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
