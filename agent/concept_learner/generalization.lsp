;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: generalization.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: November  6, 2019 14:54:11
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Thursday, December 19, 2019 at 12:21:04 by klenk
;;;; ----------------------------------------------------------------------------

(in-package :aileen)

;; (load "analogystack/qrgsetup.lsp")
;; (require-module "fire" :fire)

(defparameter *assimilation-threshold* 0.2 "sage threshold for storing new cases. A low number means we want everything to be in one generalization and that we don't expect disjunctive concepts")
(defparameter *match-threshold* 0.2 "sage threshold for matching to generalizations. because we are doing our own scoring this is low")
(defparameter *normalized-threshold* 0.75 "mapping threshold normalized against the target without the inference fact")
;;; Klenk tried making this .999, but I ran into a problem with some mapping scores not being as high expected in the relational cases

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
    (dolist (fact (fire:retrieve-it '?x :context gpool :response '?x))
      (format t "Forgetting ~A in ~A~%" fact gpool)
      (fire:kb-forget fact :mt context))
    (cl-user::setup-gpool gpool  :strategy :gel )  
    (fire:kb-store `d::(genls ,aileen::symbol AileenReasoningSymbol) :mt 'd::BaseKB)
    (fire:kb-store `d::(isa ,aileen::symbol Collection) :mt 'd::BaseKB)
    (values (1- (length (fire:ask-it `d::(genls ?x AileenReasoningSymbol)))) ;;due to genls identity
	    gpool)))

(defun create-reasoning-predicate (pred arity)
  (let ((gpool (get-concept-gpool pred)))
    (cl-user::nuke-gpool gpool)
    (dolist (fact (fire:retrieve-it '?x :context gpool :response '?x))
      (format t "Forgetting ~A in ~A~%" fact gpool)
      (fire:kb-forget fact :mt gpool))
    (cl-user::setup-gpool gpool :strategy :gel )  
    (fire:kb-store `d::(isa ,aileen::pred AileenReasoningPredicate) :mt 'd::BaseKB)
    (fire:kb-store `d::(arity ,aileen::pred
			      ,aileen::arity) :mt 'd::BaseKB)
    (values (length (fire:ask-it `d::(isa ?x AileenReasoningPredicate)))
	    gpool)))


;;; Assumes gpool is aready created
(defun add-case-to-gpool (facts context gpool)
  (store-facts-in-case facts context)
  (fire:kb-forget `(d::gpoolAssimilationThreshold ,gpool ?x) :mt gpool)
  (fire:kb-store `(d::gpoolAssimilationThreshold ,gpool ,*assimilation-threshold*) :mt gpool)
  (fire:tell-it `(d::sageSelectAndGeneralize ,context ,gpool) :context gpool)
  (multiple-value-bind (rbrowse full url) (rbrowse::browse-sme sme::*sme*)
		   (format t "~% rbrowse-sme: ~A base:~A target: ~A url: ~A "
			   sme::*sme*
			   context
			   gpool
			   full))
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
  (fire:kb-forget `(d::gpoolAssimilationThreshold ,gpool ?x) :forget gpool)
  (fire:kb-store `(d::gpoolAssimilationThreshold ,gpool ,*match-threshold*) :mt gpool)
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
  (assert (null prevmatches))
  (store-facts-in-case facts context)
  (when (not gpool)
    (setf gpool (get-concept-gpool (third pattern))))
  (fire:kb-forget `(d::gpoolAssimilationThreshold ,gpool ?x) :mt gpool)
  (fire:kb-store `(d::gpoolAssimilationThreshold ,gpool ,*match-threshold*) :mt gpool)
  (let* ((collection (third pattern))
	 (objs
	  (remove-if-not
	   #'(lambda (obj)
	       (remove-facts-from-case `(d::MinimalCaseFromMtFn ,obj ,context))
	       (fire:clear-dgroup-caches)	       
	       (fire:tell-it `(d::constructCaseInWM (d::MinimalCaseFromMtFn ,obj ,context)))
	       (fire:tell-it `(d::copyWMCaseToKB (d::MinimalCaseFromMtFn ,obj ,context)
						 (d::MinimalCaseFromMtFn ,obj ,context)))
	       (when (fire:ask-it
		      `(d::reverseCIsAllowed
			(d::and
			 (d::sageSelect (d::MinimalCaseFromMtFn ,obj ,context)
					,gpool ?ret ?mapping)
			 (d::reverseCandidateInferenceOf ?ci ?mapping)
			 (d::candidateInferenceContent ?ci (d::isa ,obj ,collection))))
		      :context gpool)
		 (multiple-value-bind (rbrowse full url) (rbrowse::browse-sme sme::*sme*)
		   (format t "~% rbrowse-sme: ~A base:~A target: ~A url: ~A"
			   sme::*sme*
			   `(d::MinimalCaseFromMtFn ,obj ,context)
			   gpool
			   full))
		 (let ((score (sme::score (car (sme::mappings  sme::*sme*))))
		       (sme sme::*sme*))
		   (sme::clone-current-sme)
		   (format t "~% ~A" (sme::expressions (sme::target sme::*sme*)))
		   (remove-rel-from-dgroup (sme::target sme::*sme*) collection)
		   (format t "~% ~A" (sme::expressions (sme::target sme::*sme*)))
		   (let ((normalized-score (/ score
					      (sme::self-score-dgroup (sme::target sme::*sme*)
								      (sme::mapping-parameters sme::*sme*)))))
		     (format t "~% ~A score: ~A normalized score: ~A" sme score normalized-score)
		     (sme::in-sme sme)
		     (> normalized-score *normalized-threshold*) ;; Out of floating point concerns
		     ))))
	   (reverse (objs-in-context context)))))
    (dolist (obj objs objs) ;;;the fact to working memory and return the list of objects
      (fire:tell-it `(d::isa ,obj ,(third pattern)) :context context))))

(defun remove-rel-from-dgroup (dgroup rel)
  (let ((expr
	 (find rel (sme::expressions dgroup)
	       :key #'(lambda (expr) (sme:name (car expr))))))
    (format t "~% REMOVING: ~A" expr)
    (setf (sme::expressions dgroup)
	  (remove expr (sme::expressions dgroup) :test #'equalp))
    (dolist (entity (sme::entities dgroup))
      (setf (sme::parents entity)
	    (remove (cadr expr) (sme::parents entity))))))

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
	    (remove-if #'(lambda (obj)
			   (find obj pattern))
		       (objs-in-context context))))))))


(defun make-case-term (context objs)
  ;;; Union of all the minimal case fns
  (cons 'd::CaseUnionFn
	(mapcar #'(lambda (obj)
		    `(d::MinimalCaseFromMtFn ,obj ,context))
		objs)))

(defun aileen-symbols-in-pattern (pattern)
  (remove-if-not
   #'(lambda (entity)
       (fire:ask-it `d::(or (isa ,aileen::entity AileenReasoningSymbol)
			    (isa ,aileen::entity AileenReasoningPredicate))))
   pattern))

(defun match-query-against-gpool (context gpool pattern)
  (sme:with-sme-type
    ;  'sme::exhaustive-sme  ;;;DOESN'T WORK WITH FILTERS  ;;;annoyingly missing a greedy merge in a test case 
      'sme::sme
  (let* ((objs (remove-if-not
		#'(lambda (e) (find e (objs-in-context context)))
		pattern))
	 (case-term (make-case-term context objs)))
    (fire:kb-forget `(d::gpoolAssimilationThreshold ,gpool ?x):mt gpool)
    (fire:kb-store `(d::gpoolAssimilationThreshold ,gpool ,*match-threshold*) :mt gpool)
    (remove-facts-from-case case-term)
    (fire:clear-dgroup-caches)	       
    (fire:tell-it `(d::constructCaseInWM ,case-term))
    (fire:tell-it `(d::copyWMCaseToKB ,case-term ,case-term)) ;;could have an explicit query context here for easier clean up?
    (dolist (obj objs) ;; analogy control predicates
      (fire:kb-store `(d::sageRequireInMapping ,obj) :mt case-term))
    (when (fire:ask-it
	   `(d::reverseCIsAllowed
	     (d::and
	      (d::sageSelect ,case-term ,gpool ?ret ?mapping)
	      (d::reverseCandidateInferenceOf ?ci ?mapping)
	      (d::candidateInferenceContent ?ci ,pattern)))
	   :context gpool)
      (let ((score (sme::score (car (sme::mappings  sme::*sme*))))
	    (sme sme::*sme*))
	(multiple-value-bind (rbrowse full url) (rbrowse::browse-sme sme::*sme*)
		   (format t "~% rbrowse-sme: ~A base: ~A target: ~A url: ~A"
			   sme::*sme*
			   case-term
			   gpool
			   full))
	(sme::clone-current-sme)
	(format t "~% ~A" (sme::expressions (sme::target sme::*sme*)))
	(dolist (rel (aileen-symbols-in-pattern pattern))
	  (remove-rel-from-dgroup (sme::target sme::*sme*) rel))
	(format t "~% ~A" (sme::expressions (sme::target sme::*sme*)))
	(let ((normalized-score (/ score
				   (sme::self-score-dgroup (sme::target sme::*sme*)
							   (sme::mapping-parameters sme::*sme*)))))
	  (format t "~% ~A score: ~A normalized score: ~A" sme score normalized-score)
	  (sme::in-sme sme)
	  (> normalized-score *normalized-threshold*) ;; Out of floating point concerns
	  ))))))


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
