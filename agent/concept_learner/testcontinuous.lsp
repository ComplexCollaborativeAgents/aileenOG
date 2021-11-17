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
  (explain-concepts 
    'd::((isa query-facts AileenCaseMt) (sw ob384 ob391) (distance 0.1634 ob384 ob391) (size ob391 0.01) (ne ob391 ob384) (size ob384 0.0121)
         (qPred-size ob384) (qPred-distance ob384 ob391) (qPred-size ob391) (dc ob391 ob384) (isa ob384 CVCone) (isa ob384 CVGreen)
         (dc ob384 ob391) (isa ob391 CVCylinder) (isa ob391 CVRed))
    'd::query-facts))


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

        (dolist (fact (maybe-add-quantity-preds facts gpool))
          ; (format t "storing ~s in ~s" fact mt)
          (fire:kb-store fact :mt mt))

        (generalize-case mt gpool)

        ))))


(defun run-query (facts concept concept-pred)
  (let ((gpool (get-concept-gpool concept)))
    (remove-facts-from-case *probe-mt*)
    (store-facts-in-case facts *probe-mt*)
    ;;; see if in-distribution, if so add preds
    (setf facts (append facts (maybe-add-quantity-preds facts gpool)))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Quantity Reasoning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (defun test-find-quantities ()
;   (find-quantities *test-preds* *near-pos*))


(defun test-maybe-add (facts gpool)
  (maybe-add-quantity-preds facts gpool))


;;; need gpool to see how many examples so far
;;; this lets us know if we have enough evidence to
;;; learn quantity distributions
(defun maybe-add-quantity-preds (facts gpool &key (learn-after 3))

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
                      ((in-distribution? fact gpool)
                       (debug-format "    In dist, adding ~s~%" qfact-pred)
                       (list qfact-pred))
                      (t 
                        (debug-format "    Out of dist, not adding ~s~%" qfact-pred)
                        nil)))) 
      quantity-facts)))


(defun in-distribution? (qpred gpool)
  (let* ((dist-quants (pred-quantities qpred gpool))
         (probe-quant (fact-quantity qpred))
         (mean (mean dist-quants))
         (stddev (stddev dist-quants)))
    (debug-format "    Checking dist: probe is ~A, quants are ~A~%" probe-quant dist-quants)
    (confidence probe-quant mean stddev)))


;;; a dumb version that doesn't take structure mapping into account
;;; effectively trying to find the 1:1 mapping between qpred and
;;; facts in each gpool. returns a list of facts (one from each gpool)
(defun pred-quantities (qpred gpool)
  (mapcan (lambda (mt)
            ; (format t "    Checking mt ~s~%" mt)
            (let* ((facts (kb::list-mt-facts mt))
                   (relevant-facts (filter-relevant-facts qpred facts mt)))
              ; (format t "    Rel facts are ~s~%" relevant-facts)
              (mapcar 'fact-quantity relevant-facts)))
    (gpool->cases gpool)))


(defun fact-quantity (fact)
  (or (find-if 'numberp (cdr fact))
      (find-if 'numberp (third fact))))


(defun filter-relevant-facts (qfact facts context)
  (cond ((eql (car qfact) 'd::holdsIn)
         (filter-relevant-temporal-facts qfact context))
        (t (filter-relevant-facts-default qfact facts context))))


;;; returns facts in context that have the same relation (e.g. sizeOf)
;;; AND the same temporal position (first, middle, last) as qfact
(defun filter-relevant-temporal-facts (qfact context)
  (let ((target-rln (car (third qfact)))
        (target-episode (episode-ci (second qfact) context)))

    ; (debug-format "    Mapped episode for ~s is ~s in ~s~%" (second qfact) target-episode context)
    (assert target-episode)

    (remove-if-not 
     (lambda (fact)
       (and (eql (second fact) target-episode)
            (listp (third fact))
            (eql target-rln (car (third fact)))))
     (kb::list-mt-facts context))))


(defun episode-ci (base-episode target-context)
  (let ((probe-episodes (episode-order 'd::query-facts))
        (target-episodes (episode-order target-context)))

    ; (debug-format "    Probe episodes: ~s Target episodes: ~s~%" probe-episodes target-episodes)

    (nth (position base-episode probe-episodes) target-episodes)))


;;; tied to four episodes per context
(defun episode-order (context)
  (let* ((start (car (fire::ask-it '(d::isa ?what d::AileenActionStartTime) 
                       :response '?what :context context)))
         (second (car (fire:ask-it `(d::startsAfterEndingOf ?second ,start)
                        :response '?second :context context)))
         (terminal (car (fire::ask-it '(d::aileenTerminalTransition ?one ?two) 
                          :response (list '?one '?two) :context context))))
    (cons start (cons second terminal))))


(defun filter-relevant-facts-default (qfact facts context)
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


(defmethod make-qfact-pred ((qfact t))
  (cond ((eql (car qfact) 'd::holdsIn)
         (list (car qfact) (second qfact) (make-qfact-pred (third qfact))))
        (t (let ((pred (intern (format nil "qPred-~a" (car qfact))))
                 (non-quantity-arg-count (non-quantity-arg-count qfact))
                 (nq-args (remove-if 'numberp (cdr qfact))))
             ;;; should we not do this if it already exists?
             (create-reasoning-predicate-simple pred non-quantity-arg-count)
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
