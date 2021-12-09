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


; (defparameter *assimilation-threshold* .6)
(defparameter *probe-mt* 'd::query-facts)
(defparameter *min-quant-count* 3)
(defparameter *test-preds* 'd::(distanceBetween distance sizeOf size))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Testing code


(defun qlearning-main ()

  (load-test-flat-files)

  ; ;;; nearness cases
  ; (create-test-generalizations 'd::rNear (list 11 12 13 14 15) :predicate)
  ; (create-test-generalizations 'd::rLeft (list 21 22 23 24 25) :predicate)
  ; (create-test-generalizations 'd::rNearTwo (list 31 32 33 34 35) :predicate)

  ; (create-test-generalizations 'd::rSmallNew (list 41 42 43 44 45) :symbol)

  (create-test-generalizations 'd::r_move_rightof1 
    'd::(r-move-right-1072 r-move-right-1757) :action)

  )


(defun test-query ()

  ; (run-query *near-pos* 'd::rNear)
  ; (run-query *near-neg* 'd::rNear)
  
  (run-query *left-pos* 'd::rLeft 'd::(rLeft ?obj1 ?obj2))
  ; (run-query *left-neg* 'd::rLeft 'd::(rLeft ?obj1 ?obj2))

  ; (run-query *mixed-pos* 'd::rNearTwo)
  ; (run-query *mixed-neg* 'd::rNearTwo)

  ; (run-query *small-pos* 'd::rSmallNew 'd::(isa ?obj rSmallNew))
  ; (run-query *small-neg* 'd::rSmallNew 'd::(isa ?obj rSmallNew))
  )


(defun test-project ()

  (project-state-for-action (kb::list-mt-facts 'd::r-move-right-452) 'd::query-facts 'd::r_move_rightof1)

  )


;;; action generalizations need to exist already
(defun test-action-quantities ()

  (run-query-mt 'd::qtest-query-facts 'd::r_move_near1 '(d::r_move_near1 ?obj1 ?obj2))

  )


(defun test-describe ()
  (describe-concepts 
    'd::((sw ob384 ob391) (distance 0.11 ob384 ob391) (size ob391 0.01) (ne ob391 ob384) (size ob384 0.0121)
         (dc ob391 ob384) (isa ob384 CVCone) (isa ob384 CVGreen)
         (dc ob384 ob391) (isa ob391 CVCylinder) (isa ob391 CVRed))
    'd::describe-facts))


(defun load-test-flat-files ()
  (fire:kr-file->kb (qrg:make-qrg-file-name
                     (qrg:make-qrg-path ".." "data")
                     "aileen-mt.krf")
    :error-on-bad-exps? t :kb fire::*kb*)
  (cl-user::load-flatfiles-in-dir (qrg:make-qrg-path ".." "data" "continuous-q-learning")))


(defun create-test-generalizations (concept-symbol case-ids type)

  (case type
    (:action (create-reasoning-action concept-symbol 2))
    (:predicate (create-reasoning-predicate concept-symbol 2))
    (:symbol (create-reasoning-symbol concept-symbol)))

  ;;; need to mod training examples
  (let ((gpool (get-concept-gpool concept-symbol)))
    
    (create-gpool gpool)
    
    (dolist (id case-ids)
      
      ;;; see if we need to add quantity preds
      ; (maybe-add-quantity-preds id gpool)
      (let* ((mt (if (numberp id) (microtheory-by-id id) id))
             (facts (kb::list-mt-facts mt)))

        (dolist (fact (maybe-add-quantity-preds facts 'd::query-facts gpool))
          ; (format t "storing ~s in ~s" fact mt)
          (fire:kb-store fact :mt mt))

        (generalize-case mt gpool)

        ))))


(defun run-query (facts concept concept-pred)
  (let ((gpool (get-concept-gpool concept)))
    (remove-facts-from-case *probe-mt*)
    (store-facts-in-case facts *probe-mt*)
    ;;; see if in-distribution, if so add preds
    (setf facts (append facts (maybe-add-quantity-preds facts *probe-mt* gpool)))
    (filter-scene-by-expression facts *probe-mt* gpool nil concept-pred)
    ))


(defun run-query-mt (mt concept concept-pred)
  (run-query (kb::list-mt-facts mt) concept concept-pred))


(defun make-random-mt ()
  (intern (gensym "AlieenMT") :d))
  

(defun microtheory-by-id (id)
  (intern (format nil "AileenExp~D" id) :d))


(defun create-gpool (gpool)
  (cl-user::nuke-gpool gpool)
  (cl-user::setup-gpool gpool :threshold *assimilation-threshold* :strategy :gel)
  (values (length (fire:ask-it `(d::gpoolGeneralization ,gpool ?num)))
          (length (fire:ask-it `(d::gpoolExample ,gpool ?num)))))


(defmethod generalize-case ((mt t) (gpool t))
  (format t "generalizing ~s~%" mt)
  (fire:tell-it `(d::sageSelectAndGeneralize
                  ,mt
                  ,gpool)))


(defmethod generalize-case ((case-id fixnum) (gpool t))
  (format t "generalizing ~s~%" case-id)
  (fire:tell-it `(d::sageSelectAndGeneralize
                  ,(microtheory-by-id case-id)
                  ,gpool)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; New Quantity Reasoning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun internal-fact? (fact internal-preds)
  (member (car (decontextualize-temporal-pred fact)) internal-preds))


(defun remove-quantity-facts (facts internal-preds)
  (remove-if (lambda (fact)
    (let ((decon (decontextualize-temporal-pred fact)))
      (or (member (car decon) internal-preds)
          (fact-has-quantity? decon)))) 
    facts))


;;; We will have just performed a mapping, as facts are a set of candidate inferences
;;; from that mapping
(defun sample-internal-preds-new (facts probe-mt sme concept)
  (let* ((internal-preds (fire:ask-it '(d::isa ?pred d::AileenInternalPredicate) :response '?pred))
         (filtered-facts (remove-quantity-facts facts internal-preds))
         (sampled-facts (sample-quantity-mhs sme internal-preds)))
    (append filtered-facts sampled-facts)))


(defun sample-quantity-mhs (sme internal-preds)

  (let ((sampled-facts nil)
        (mapping (car (sme::mappings sme))))
    (dolist (mh (sme::top-level-mhs mapping) sampled-facts)
      (let ((base-form (sme::user-form (sme::base-item mh))))
        (when (internal-fact? base-form internal-preds)
          
          
          )))))


;;; given a case potentially having quantity preds,
;;; maybe assert internal encoding preds
(defun rerep-quants (probe-context concept)
  (let ((match (sage-select probe-context concept)))
    (when match
      (let ((ipreds (use-mapping-to-support-internal-continuous-preds sme::*sme*)))
        (store-facts-in-case (append (kb::list-mt-facts) ipreds))))))


(defun filter-quantity-mhs (mhs)
  (remove-duplicates
    (remove-if-not (lambda (mh)
                   (numberp (sme::user-form (sme::base-item mh)))) 
    mhs) :test 'equal))


(defun mh-history (mh)
 (mapcar 'second (cdar (fire::ask-it `(d::gmtEntityHistory 
        ,(sme::user-form (sme::target-item mh))
        ,(sme::name (sme::target (sme::sme mh))) ?x)
    :response '?x))))

(defun quantity-mh-in-distribution? (mh)
  (let ((quants (mh-history mh))
        (probe (sme::user-form (sme::base-item mh))))
    (if (< (length quants) *min-quant-count*)
      t
      (in-distribution-new? probe quants))))


;;; this makes certain assumptions about facts and quantities in them
;;; if we start dealing with facts that have multiple quantities
;;; (and right now I don't know what that would mean), then
;;; this should be revisited.
(defun top-level-in-distribution-new? (top-level-mh)
  (let ((quant-mhs (filter-quantity-mhs (sme::descendants top-level-mh))))
    (every (lambda (mh)
             (quantity-mh-in-distribution? mh)) 
      quant-mhs)))


(defun use-mapping-to-support-internal-continuous-preds (sme)
  (let ((internal-preds nil)
        (mapping (car (sme::mappings sme))))
    (dolist (mh (sme::top-level-mhs mapping) internal-preds)
      (let ((base-form (sme::user-form (sme::base-item mh))))
        (when (fact-has-quantity? base-form)
          (when (top-level-in-distribution-new? mh)
            (push (make-qfact-pred base-form) internal-preds)
            ))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Quantity Reasoning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (defun test-find-quantities ()
;   (find-quantities *test-preds* *near-pos*))


(defun test-maybe-add (facts gpool)
  (maybe-add-quantity-preds facts 'd::query-facts gpool))


;;; need gpool to see how many examples so far
;;; this lets us know if we have enough evidence to
;;; learn quantity distributions
(defun maybe-add-quantity-preds (facts facts-context gpool &key (learn-after 3))

  (let (;;; how many examples in gpool
        (example-count (gpool->size gpool))
        ;;; get quantity preds from facts in case
        (quantity-facts (find-quantities *test-preds* facts)))

    (debug-format "~%------------------------~%~s examples for ~s~%" example-count gpool)

    ; (format t "qfacts are ~s~%" quantity-facts)
    (mapcan (lambda (fact)
              ;;; take the predicate that has at least one quantity arg
              ;;; introduce a new predicate without the quantity
              (let ((qfact-pred (make-qfact-pred fact)))

                (debug-format "    Checking ~A~%" qfact-pred)

                (cond ((< example-count learn-after)
                       ; (debug-format "    Less than ~s examples, default add ~s~%" learn-after qfact-pred)
                       (list qfact-pred))
                      ((in-distribution? fact facts-context gpool)
                       (debug-format "    In dist, adding ~s~%" qfact-pred)
                       (list qfact-pred))
                      (t 
                        (debug-format "    Out of dist, not adding ~s~%" qfact-pred)
                        nil)))) 
      quantity-facts)))


(defun decontextualize-temporal-pred (pred)
  (if (eql (car pred) 'd::holdsIn) (third pred) pred))


(defun filter-internal-preds (preds)
  (let ((internal-preds (fire:ask-it '(d::isa ?pred d::AileenInternalPredicate) :response '?pred)))
    (remove-if-not (lambda (pred)
                     (member (car (decontextualize-temporal-pred pred)) internal-preds))
                   preds)))


(defun remove-continuous-preds (preds)
  (remove-if 'fact-has-quantity? preds))


(defun sample-internal-preds (facts probe-mt gpool)
  (let ((internal-preds (fire:ask-it '(d::isa ?pred d::AileenInternalPredicate) :response '?pred)))
    (mapcan (lambda (fact)
              (let ((decon (decontextualize-temporal-pred fact)))
              (cond ((fact-has-quantity? decon)
                     nil)
                    ((member (car decon) internal-preds)
                     (list (sample-internal-pred fact probe-mt gpool)))
                    (t (list fact))))) 
      facts)))


(defun sample-internal-pred (internal-fact probe-mt gpool)
  (let* ((decon (decontextualize-temporal-pred internal-fact))
         (continuous-pred (car (fire:ask-it `(d::encodingOf ?fact ,(car decon)) :response '?fact)))
         (continuous-fact (sublis (list (cons (car decon) continuous-pred)) internal-fact))
         (quantity (sample-gpool continuous-fact probe-mt gpool)))

  (if (eql (car internal-fact) 'd::holdsIn)
    (list 'd::holdsIn (second internal-fact) (append (list continuous-pred) (list quantity) (cdr decon)))
    (append (list continuous-pred) (list quantity) (cdr continuous-fact)))
  ))


(defun sample-gpool (continuous-pred probe-mt gpool)
  (sample-series-normal (pred-quantities continuous-pred probe-mt gpool)))
         

(defun in-distribution? (qpred probe-context gpool)
  (let* ((dist-quants (pred-quantities qpred probe-context gpool))
         (probe-quant (fact-quantity qpred))
         (mean (mean dist-quants))
         (stddev (stddev dist-quants)))
    (debug-format "    Checking dist: probe is ~A, quants are ~A~%" probe-quant dist-quants)
    (confidence probe-quant mean stddev)))


(defun in-distribution-new? (probe quants)
  (let ((mean (mean quants))
        (stddev (stddev quants)))
   (confidence probe mean stddev)))


;;; a dumb version that doesn't take structure mapping into account
;;; effectively trying to find the 1:1 mapping between qpred and
;;; facts in each gpool. returns a list of facts (one from each gpool)
(defun pred-quantities (qpred probe-context gpool)
  (mapcan (lambda (mt)
            ; (format t "    Checking mt ~s~%" mt)
            (let* ((facts (kb::list-mt-facts mt))
                   (relevant-facts (filter-relevant-facts qpred facts probe-context mt)))
              ; (format t "    Rel facts are ~s~%" relevant-facts)
              (mapcar 'fact-quantity relevant-facts)))
    (gpool->cases gpool)))


(defun fact-quantity (fact)
  (or (find-if 'numberp (cdr fact))
      (find-if 'numberp (third fact))))


(defun fact-has-quantity? (fact)
  (find-if 'numberp (cdr (decontextualize-temporal-pred fact))))


(defun filter-relevant-facts (qfact facts probe-context target-context)
  (cond ((eql (car qfact) 'd::holdsIn)
         (filter-relevant-temporal-facts qfact probe-context target-context))
        (t (filter-relevant-facts-default qfact facts))))


;;; returns facts in context that have the same relation (e.g. sizeOf)
;;; AND the same temporal position (first, middle, last) as qfact
(defun filter-relevant-temporal-facts (qfact probe-context target-context)
  (let ((target-rln (car (third qfact)))
        (target-episode (episode-ci (second qfact) probe-context target-context)))
    
    (debug-format "    Mapped episode for ~s is ~s in ~s~%" (second qfact) target-episode target-context)
    (assert target-episode)
    
    (remove-if-not 
     (lambda (fact)
       (and (eql (second fact) target-episode)
            (listp (third fact))
            (eql target-rln (car (third fact)))))
     (kb::list-mt-facts target-context))))


(defun episode-ci (base-episode probe-context target-context)
  (let ((probe-position (episode-position base-episode probe-context))
        (target-episodes (episode-order target-context)))
    
    ; (debug-format "    Probe episodes: ~s Target episodes: ~s~%" probe-episodes target-episodes)
    
    (nth probe-position target-episodes)))



;;; this will probably need to be fixed for more complex situations
(defun episode-position (episode context)
  (cond ((fire::ask-it `(d::isa ,episode d::AileenActionStartTime)
           :context context)
         0)
        (t (let ((order (episode-order context)))
             (unless order (break "Can't find order of episodes"))
             (position episode order)))))


;;; tied to four episodes per context
(defun episode-order (context)
  (let* ((start (car (fire::ask-it '(d::isa ?what d::AileenActionStartTime) 
                       :response '?what :context context)))
         (second (car (fire:ask-it `(d::startsAfterEndingOf ?second ,start)
                        :response '?second :context context)))
         (terminal (car (fire::ask-it '(d::aileenTerminalTransition ?one ?two) 
                          :response (list '?one '?two) :context context))))
    (cons start (cons second terminal))))


(defun filter-relevant-facts-default (qfact facts)
  (mapcan (lambda (fact)
            (and (eql (car fact) (car qfact)) (list fact)))
    facts))


(defun gpool->size (gpool-form)
  (length (gpool->cases gpool-form)))


(defun gpool->cases (gpool-form)
  (nconc (gpool->assimilated gpool-form) (gpool->unassimilated gpool-form)))


(defun gpool->unassimilated (gpool-form)
  (mapcar 'car (fire:ask-it `(d::kbOnly (d::gpoolExample ,gpool-form ?ex)) :response '?ex)))


(defun gpool->assimilated (gpool-form)
  (fire:ask-it `(d::kbOnly (d::sageConstituent ?case ?gmt ,gpool-form)) :response '?case))


(defun make-qpred-relation (relation)
  (intern (format nil "qPred-~a" relation)))


(defmethod make-qfact-pred ((qfact t))
  (cond ((eql (car qfact) 'd::holdsIn)
         (list (car qfact) (second qfact) (make-qfact-pred (third qfact))))
        (t (let ((pred (make-qpred-relation (car qfact)))
                 (non-quantity-arg-count (non-quantity-arg-count qfact))
                 (nq-args (remove-if 'numberp (cdr qfact))))
             ;;; should we not do this if it already exists?
             (create-internal-predicate pred (car qfact) non-quantity-arg-count)
             (cons pred nq-args)))))


;;; take a ground predicate, return number of
;;; non-quantity args
;;; this should probably be explicitly reified
(defun non-quantity-arg-count (qfact)
  (count-if-not 'numberp (cdr qfact)))


;;; return a list of facts
(defun find-quantities (preds facts)
  (remove-if-not (lambda (fact)
                   (or (member (car fact) preds)
                       ; for holdsIn statements, not pretty
                       (and (eq (car fact) 'd::holdsIn) (member (car (third fact)) preds))))
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


(defun sample-series-normal (series)
  (let ((mean (mean series))
        (stddev (stddev series)))
    (sample-gaussian mean stddev)))


;-------------------------------------------------------------------------------
(defconstant +sapa-sqrt-8-over-e+ (sqrt (/ 8.0d0 (exp 1.0d0))))
(defconstant +sapa-4-time-exp-of-1-over-4+ (* 4.0d0 (exp 0.25d0)))
(defconstant +sapa-4-time-exp-of-minus-1-point-35+ (* 4.0d0 (exp (- 1.35d0))))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  ranorm
;;;                 ranorms
;;;  generate one or more uncorrelated normally distributed deviates.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun ranorm ()
  "returns a random deviate from a normal distribution
   with zero mean and unit variance"
  (let ((u (random 1.0)))
    (cond
     ((= u 0.0) (ranorm))   ;bad choice!
     (t
      (let* ((x (/ (* (- (random 1.0) 0.5)
                      +sapa-sqrt-8-over-e+) u))
             (xs (* x x)))
        (cond
         ((<= xs (- 5.0 (* u +sapa-4-time-exp-of-1-over-4+)))
          x)   ;done
         ((>= xs (+ 1.4 (/ +sapa-4-time-exp-of-minus-1-point-35+ u)))
          (ranorm))   ;do it again
         ((<= xs (- (* 4.0 (log u))))
          x)
         (t
          (ranorm))))))))

(defun sample-gaussian (mean stddev)
  (+ mean (* stddev (ranorm))))


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
