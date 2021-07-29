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

(in-package :aileen)


(defparameter *assimilation-threshold* .6)

; mt ids for concept
(defparameter *r-near-inform-ids* '(11 12 13))
(defparameter *near-concept-symbol* 'd::rNear)


(defparameter *leftof-inform-ids* '(11 12 13))
(defparameter *leftof-concept-symbol* 'd::r_leftOf)


(defparameter *near-filter-expression* (list *near-concept-symbol* 'd::?one 'd::?two))
(defparameter *probe-mt* 'd::query-facts)




;;; positive example
(defparameter *near-pos* 'd::( 

  (isa ObjNearPosA CVCube)
  (isa ObjNearPosA CVGreen)

  (isa ObjNearPosB CVCylinder)
  (isa ObjNearPosB CVGreen)

  (distanceBetween ObjNearPosA ObjNearPosB 4.0)

  (dc ObjNearPosA ObjNearPosB)
  (e ObjNearPosA ObjNearPosB)

  ))


;;; negative nearness test case
(defparameter *near-neg* 'd::( 

  (isa ObjNearNegA CVCube)
  (isa ObjNearNegA CVYellow)

  (isa ObjNearNegB CVCylinder)
  (isa ObjNearNegB CVGreen)

  (distanceBetween ObjNearNegA ObjNearNegB 99999.0)

  (dc ObjNearNegA ObjNearNegB)
  (s ObjNearNegA ObjNearNegB)

  ))




(defparameter *left-pos* 'd::( 

  (isa ObjLeftPosA CVCube)
  (isa ObjLeftPosA CVGreen)

  (isa ObjLeftPosB CVCylinder)
  (isa ObjLeftPosB CVGreen)

  (distanceBetween ObjLeftPosA ObjLeftPosB 4.0)

  (dc ObjLeftPosA ObjLeftPosB)
  (w ObjLeftPosA ObjLeftPosB)

  ))


(defparameter *left-neg* 'd::( 

  (isa ObjLeftNegA CVCube)
  (isa ObjLeftNegA CVYellow)

  (isa ObjLeftNegB CVCylinder)
  (isa ObjLeftNegB CVGreen)

  (distanceBetween ObjLeftNegA ObjLeftNegB 7.0)

  (dc ObjLeftNegA ObjLeftNegB)
  (s ObjLeftNegA ObjLeftNegB)

  ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Testing code


(defun qlearning-main ()

  (load-test-flat-files)

  ;;; nearness cases
  (create-test-generalizations *near-concept-symbol* *r-near-inform-ids*)



  ; (run-query *near-pos* *concept-gpool*)
  )



(defun load-test-flat-files ()
  (fire:kr-file->kb (qrg:make-qrg-file-name
                     (qrg:make-qrg-path ".." "data")
                     "aileen-mt.krf")
    :error-on-bad-exps? t :kb fire::*kb*)
  (cl-user::load-flatfiles-in-dir (qrg:make-qrg-path ".." "data" "continuous-q-learning")))


(defun create-test-generalizations (concept-symbol case-ids)
  ; (create-reasoning-symbol concept-symbol)
  (create-reasoning-predicate concept-symbol 2)
  
  ;;; need to mod training examples
  (let ((gpool (get-concept-gpool concept-symbol)))
    
    (create-gpool gpool)
    
    (dolist (id case-ids)
      
      ;;; see if we need to add quantity preds
      ; (maybe-add-quantity-preds id gpool)
      (let* ((mt (microtheory-by-id id))
             (facts (kb::list-mt-facts mt)))

        (dolist (fact (maybe-add-quantity-preds facts gpool))
          (format t "storing ~s in ~s" fact mt)
          (fire:kb-store fact :mt mt)))
      
      (generalize-case id gpool)

      )))


(defun run-query (facts gpool)
  (remove-facts-from-case *probe-mt*)
  ;;; see if in-distribution, if so add preds
  (setf facts (append facts (maybe-add-quantity-preds facts gpool)))
  (filter-scene-by-expression facts *probe-mt* gpool nil *near-filter-expression*)
  )


(defun make-random-mt ()
  (intern (gensym "AlieenMT") :d))
  

(defun microtheory-by-id (id)
  (intern (format nil "AileenExp~D" id) :d))


(defun create-gpool (gpool)
  (cl-user::nuke-gpool gpool)
  (cl-user::setup-gpool gpool :threshold *assimilation-threshold* :strategy :gel)
  (values (length (fire:ask-it `(d::gpoolGeneralization ,gpool ?num)))
          (length (fire:ask-it `(d::gpoolExample ,gpool ?num)))))


(defmethod generalize-case ((case-id fixnum) (gpool t))
  (format t "generalizing ~s~%" case-id)
  (fire:tell-it `(d::sageSelectAndGeneralize
                  ,(microtheory-by-id case-id)
                  ,gpool)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Quantity Reasoning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *test-mt* 'd::AileenExp11)

(defparameter *test-preds* 'd::(distanceBetween distance))


; (defun test-find-quantities ()
;   (find-quantities *test-preds* *near-pos*))


(defun test-maybe-add (facts gpool)
  (maybe-add-quantity-preds facts gpool))


;;; need gpool to see how many examples so far
;;; this lets us know if we have enough evidence to
;;; learn quantity distributions
(defun maybe-add-quantity-preds (facts gpool &key (learn-after 3))
  (let (;;; how many examples in gpool
        (example-count (cl-user::compute-n-input-examples (kb::retrieve-gpool gpool :create? nil)))
        ;;; get quantity preds from facts in case
        (quantity-facts (find-quantities *test-preds* facts)))
    ; (format t "qfacts are ~s~%" quantity-facts)
    (mapcan (lambda (qfact)
              ;;; take the predicate that has at least one quantity arg
              ;;; introduce a new predicate without the quantity
              (let ((qfact-pred (make-qfact-pred qfact)))
                (cond ((< example-count learn-after)
                       (debug-format "~s examples, default add ~s~%" example-count qfact-pred)
                       (list qfact-pred))
                      ((in-distribution? qfact gpool)
                       (debug-format "in dist, adding ~s~%" qfact-pred)
                       (list qfact-pred))
                      (t 
                        (debug-format "out of dist, not adding ~s~%" qfact-pred)
                        nil)))) 
      quantity-facts)))


(defun in-distribution? (qpred gpool)
  (let* ((dist-quants (pred-quantities qpred gpool))
         (probe-quant (fact-quantity qpred))
         (mean (mean dist-quants))
         (stddev (stddev dist-quants)))
    (confidence probe-quant mean stddev)))


;;; a dumb version that doesn't take structure mapping into account
(defun pred-quantities (qpred gpool)
  (mapcan (lambda (mt)
            (let* ((facts (kb::list-mt-facts mt))
                   (relevant-facts (filter-relevant-facts qpred facts)))
              (mapcar 'fact-quantity relevant-facts)))
    (gpool->examples gpool)))


(defun fact-quantity (fact)
  (find-if 'numberp (cdr fact)))


(defun filter-relevant-facts (qfact facts)
  (mapcan (lambda (fact)
            (and (eql (car fact) (car qfact)) (list fact))) 
    facts))


;;; there's got to be a better way to do this???
(defun gpool->examples (gpool)
  (append
   (mapcar (lambda (instance)
             (third (fire::decontextualize-statement instance)))
     (fire::ask-it `(d::gpoolExample ,gpool ?example)))
   (cl-user::input-case-names-from-generalizations (kb::retrieve-gpool gpool))))


(defun make-qfact-pred (qfact)
  (let ((pred (intern (format nil "qPred-~a" (car qfact))))
        (non-quantity-arg-count (non-quantity-arg-count qfact))
        (nq-args (remove-if 'numberp (cdr qfact))))
    ;;; should we not do this if it already exists?
    (create-reasoning-predicate-simple pred non-quantity-arg-count)
    (cons pred nq-args)))

;;; take a ground predicate, return number of
;;; non-quantity args
;;; this should probably be explicitly reified
(defun non-quantity-arg-count (qfact)
  (count-if-not 'numberp (cdr qfact)))


;;; return a list of facts
(defun find-quantities (preds facts)
  (remove-if-not (lambda (fact)
                   (member (car fact) preds)) 
                 facts))


;;; this is making ugly assumptions; essentially
;;; it is ignoring structure mapping and assuming that
;;; we will ever only see one instantiation of a predicate
;;; e.g. (distanceBetween ... ... ...)
;;; to handle this more elegantly, we could invoke SME
; (defun generate-quantity-symbols (forms mt gpool)
;   (let* ((quantity-preds (find-quantities forms mt))
;          (preds (remove-duplicates (mapcar 'car quantity-preds)))
;         )
; ))


; (defun get-quantity-preds-for-gpool (pred argfn gpool)
;   (let ((preds (fire::retrieve pred :mt gpool)))
;     (mapcar argfn preds)))


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
