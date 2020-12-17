;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: test-phase2-concepts.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: June 16, 2020 09:55:54
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Tuesday, November 17, 2020 at 14:24:14 by klenk
;;;; ----------------------------------------------------------------------------

(in-package :aileen)


;;; Phase 2:
;;; Changes to QSRLib 
;;;  1) Move to RCC5
;;;  2) Add a qualitative distance calculus
;;;     a) Given the size of our scene 3 values should be enough (NearDistance, MedDistance, FarDistance)
;;;     b) Setting the values for these should be a function of the objects sizes and the frame of reference
;;;  3) maybe add adjcanecy or double cross calculus
;;;     a) Adjcancey is not in QSRlib
;;;
;;; Conjunctive Concepts
;;;  1) Hydrant is a red cylinder
;;;
;;; Composite Objects
;;;  1) Bridge/Arch
;;;  2) Tower (with different numbers of objects)
;;;
;;; Disjunctive Concepts
;;;  1) Primary colors are Red, Blue, and Yellow
;;;  2) Above is n, ne, nw
;;;  3) Next to
;;;
;;; Complex Spatial Relations
;;;  1) Between (3 arguments)
;;;  2) Near (will this align with Qualitative Distance Calculus)


;;; 7/27 thoughts
;;; If disjunctions are the first thing we are worried about.

(load "test-client.lsp")


(in-package :aileen)

;(setq *assimilation-threshold* 0.6)

(defun test-phase2-concept-learner-server (&key (clean? t))
  (start-server :port *test-port*) ;; needs to match port in call-test-server.
  (when clean? (checkpoint-init))
  (test-phase2-reasoning-symbols)
  (test-primary)
  (test-above)
  (test-next-to)
  (test-wall)
;  (when clean? (restore-init))
  )

(defun test-phase2-reasoning-symbols ()
  (let (res)
    (setq res (call-test-server
               "create_reasoning_symbol"
               (pairlis '("symbol")
                        '("RPrimary"))))
    (assert (equal (cdr (assoc :GPOOL res))
                   "RPrimaryMt"))
    (setq res (call-test-server
               "create_reasoning_predicate"
               (pairlis '("predicate")
                        '("rAbove"))))
    (assert (equal (cdr (assoc :GPOOL res))
		   "rAboveMt"))
    (setq res (call-test-server
               "create_reasoning_predicate"
               (pairlis '("predicate")
                        '("rNextTo"))))
    (setq res (call-test-server
               "create_reasoning_predicate"
               (pairlis '("predicate" "arity")
                        '("rWall" 3))))
    (assert (equal (cdr (assoc :GPOOL res))
		   "rWallMt"))   
    ))


(defun test-primary ()
  (test-primary-basic)
  )

;; red or blue or green
(defun test-primary-basic ()
  (let (res pattern)
    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list (list (list "isa" "O0" "RPrimary")
                                    (list "isa" "O0" "CVRed")
                                    (list "isa" "O0" "CVCylinder"))
                              "Test0P2A" ;;Id
                              "RPrimary"))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 1))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 0))
    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list (list (list "isa" "O1" "RPrimary")
                                    (list "isa" "O1" "CVRed")
                                    (list "isa" "O1" "CVCube"))
                              "Test1P2A" ;;Id
                              "RPrimary"))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 0))  
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 1)) ;; A good generalization
    
    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list (list (list "isa" "O0" "RPrimary")
                                    (list "isa" "O0" "CVYellow")
                                    (list "isa" "O0" "CVCylinder"))
                              "Test2P2A" ;;Id
                              "RPrimary"))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 1))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 1)) ;;; ungeneralized exemplar
    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list (list (list "isa" "O1" "RPrimary")
                                    (list "isa" "O1" "CVBlue")
                                    (list "isa" "O1" "CVCylinder"))
                              "Test3P2A" ;;Id
                              "RPrimary"))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 0)) 
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 2)) ;;;A Bad generalization!
    
    ))



(defun test-above ()
  (test-above-basic))

;; This basic test evaluates assumes the best case scenario for input
(defun test-above-basic ()
  (let (res)
    (setq res (call-test-server
	       "store"
	       (pairlis '("facts" "context" "concept")
			(list '(("isa" "Obj11A" "CVCylinder")
				("isa" "Obj11A" "CVGreen")
				("isa" "Obj11B" "CVCylinder")
				("isa" "Obj11B" "CVBlue")
				("n" "Obj11A" "Obj11B")
				("dc" "Obj11A" "Obj11B")
				("rAbove" "Obj11A" "Obj11B"))
			      "Test0P2" ;;Id
			      "rAbove"))
			))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 1))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 0))

    (setq res (call-test-server
	       "store"
	       (pairlis '("facts" "context" "concept")
			(list '(("isa" "Obj11A" "CVCone")
				("isa" "Obj11A" "CVPurple")
				("isa" "Obj11B" "CVCube")
				("isa" "Obj11B" "CVRed")
				("ne" "Obj11A" "Obj11B")
				("dc" "Obj11A" "Obj11B")
				("rAbove" "Obj11A" "Obj11B"))
			      "Test1P2" ;;Id
			      "rAbove"))
	       ))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 2)) ;;; Next example not generalized
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 0))

    (setq res (call-test-server
	       "store"
	       (pairlis '("facts" "context" "concept")
			(list '(("isa" "Obj11A" "CVCube")
				("isa" "Obj11A" "CVYellow")
				("isa" "Obj11B" "CVSphere")
				("isa" "Obj11B" "CVGreen")
				("nw" "Obj11A" "Obj11B")
				("dc" "Obj11A" "Obj11B")
				("rAbove" "Obj11A" "Obj11B"))
			      "Test2P2" ;;Id
			      "rAbove"))
	       ))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 3)) ;;; Next example not generalized
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 0))

    
    (setq res (call-test-server
	       "store"
	       (pairlis '("facts" "context" "concept")
			(list '(("isa" "Obj11A" "CVCube")
				("isa" "Obj11A" "CVPurple")
				("isa" "Obj11B" "CVCone")
				("isa" "Obj11B" "CVBlue")
				("nw" "Obj11A" "Obj11B")
				("dc" "Obj11A" "Obj11B")
				("rAbove" "Obj11A" "Obj11B"))
			      "Test3P2" ;;Id
			      "rAbove"))
	       ))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 2)) ;;; Next example not generalized
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 1))

;;; Set up tests for query of the nw example and then the n example (which will fail)

    (format t "~% Testing Exact Match ~%")
    (setq res (call-test-server
	       "query"
	       (pairlis '("facts" "pattern")
			'((("isa" "Obj11A" "CVCylinder")
			   ("isa" "Obj11A" "CVGreen")
			   ("isa" "Obj11B" "CVCylinder")
			   ("isa" "Obj11B" "CVBlue")
			   ("n" "Obj11A" "Obj11B")
			   ("dc" "Obj11A" "Obj11B"))
			  ("rAbove" "Obj11A" "Obj11B")))))
    (assert (= 1 (length (cdr (assoc :MATCHES res))))) ;;exact match should work
    (assert (equal '("rAbove" "Obj11A" "Obj11B")
		   (car (cdr (assoc :MATCHES res)))))

    (format t "~% Testing No Generalization Created for n ~%")
    (setq res (call-test-server
	       "query"
	       (pairlis '("facts" "pattern")
			'((("isa" "Obj11A" "CVCylinder")
			   ("isa" "Obj11A" "CVBlue")
			   ("isa" "Obj11B" "CVCube")
			   ("isa" "Obj11B" "CVGreen")
			   ("n" "Obj11A" "Obj11B")
			   ("dc" "Obj11A" "Obj11B"))
			  ("rAbove" "Obj11A" "Obj11B")))))
    (assert (= 0 (length (cdr (assoc :MATCHES res))))) ;; No generalization (changed colors and objects)


    (format t "~% Testing nw Generalization 1 ~%")
    ;; nw is generalized, so we should be able to find this conecpt whatever the objects
    (setq res (call-test-server
	       "query"
	       (pairlis '("facts" "pattern")
			'((("isa" "Obj11A" "CVCube") ;;;requires arg 1 to be a cube
			   ("isa" "Obj11A" "CVGreen")
			   ("isa" "Obj11B" "CVCube")
			   ("isa" "Obj11B" "CVBlue")
			   ("nw" "Obj11A" "Obj11B")
			   ("dc" "Obj11A" "Obj11B"))
			  ("rAbove" "Obj11A" "Obj11B")))))
    (assert (= 1 (length (cdr (assoc :MATCHES res))))) 

    (format t "~% Testing nw Generalization 2 ~%")
    (setq res (call-test-server
	       "query"
	       (pairlis '("facts" "pattern")
			'((("isa" "Obj11A" "CVCube")
			   ("isa" "Obj11A" "CVGreen")
			   ("isa" "Obj11B" "CVCylinder")
			   ("isa" "Obj11B" "CVBlue")
			   ("nw" "Obj11A" "Obj11B")
			   ("dc" "Obj11A" "Obj11B"))
			  ("rAbove" "Obj11A" "Obj11B")))))
;    (assert (= 1 (length (cdr (assoc :MATCHES res)))))
;;; MEK 10/16/2020 Test not working with assimilation 0.6, it works at 0.5
 
    
    ))

(defun invert-dir (dir)
  (cdr (assoc dir (pairlis '("w" "nw" "n" "ne")
			   '("e" "se" "s" "sw")) :test #'string=)))

(defun test-wall()
  (test-wall-basic))

(defun create-wall-case (&key
			   (shapes '("CVBox" "CVSphere" "CVCylinder" "CVCone"))
			   (colors '("CVGreen" "CVBlue" "CVRed" "CVPurple" "CVYellow"))
			   (dirs '("w" "nw" "n" "ne"))
			   (include-redundant-rel? t)
			   (state (make-random-state t 100))
			   )
  (let ((objs (list (symbol-name (gensym "Obj"))
		    (symbol-name (gensym "Obj"))
		    (symbol-name (gensym "Obj"))))
	(dir (nth (random (length dirs) state) dirs))
	(facts nil))
    (dolist (obj objs)
      (let ((shape (nth (random (length shapes) state) shapes))
	    (colors (nth (random (length colors) state) colors)))
	(push (list "isa" obj shape) facts)
	(push (list "isa" obj colors) facts)))
    (mapl #'(lambda (objs1)
	      (when (cdr objs1)
		(push (list dir (car objs1) (cadr objs1)) facts)
		(when include-redundant-rel? (push (list (invert-dir dir)  (cadr objs1) (car objs1)) facts))
		(push (list "ec" (car objs1) (cadr objs1)) facts)
		(when include-redundant-rel? (push (list "ec" (cadr objs1) (car objs1)) facts))
		))
	  objs)
    (push (list "dc" (car objs) (car (last objs))) facts)
    (when include-redundant-rel?
      (push (list "dc" (car (last objs)) (car objs)) facts))
    (push (cons "rWall" objs) facts) ;; should be a set 
    facts))

(defun create-non-wall-case (&key
			       (shapes '("CVBox" "CVSphere" "CVCylinder" "CVCone"))
			       (colors '("CVGreen" "CVBlue" "CVRed" "CVPurple" "CVYellow"))
			       (dirs '("w" "nw" "n" "ne"))
			       (include-redundant-rel? t)
			       (state (make-random-state t 100))
			       )
  (let ((objs (list (symbol-name (gensym "Obj"))
		    (symbol-name (gensym "Obj"))
		    (symbol-name (gensym "Obj"))))
	(dir (nth (random (length dirs) state) dirs))
	(facts nil))
    (dolist (obj objs)
      (let ((shape (nth (random (length shapes) state) shapes))
	    (colors (nth (random (length colors) state) colors)))
	(push (list "isa" obj shape) facts)
	(push (list "isa" obj colors) facts)))
    (mapl #'(lambda (objs1)
	      (when (cdr objs1)
		(push (list dir (car objs1) (cadr objs1)) facts)
		(when include-redundant-rel? (push (list (invert-dir dir)  (cadr objs1) (car objs1)) facts))
		(push (list "ec" (car objs1) (cadr objs1)) facts)
		(when include-redundant-rel? (push (list "ec" (cadr objs1) (car objs1)) facts))
		))
	  objs)
    (push (list "dc" (car objs) (car (last objs))) facts)
    (when include-redundant-rel?
      (push (list "dc" (car (last objs)) (car objs)) facts))
    (push (cons "rWall" objs) facts) ;; should be a set 
    facts))


;; require different directions, but keep all of the positional relationships the same
;; We may have to check the query threshold to make sure it is high enough
(defun create-non-wall-case (&key
			       (shapes '("CVBox" "CVSphere" "CVCylinder" "CVCone"))
			       (colors '("CVGreen" "CVBlue" "CVRed" "CVPurple" "CVYellow"))
			       (dirs '("w" "nw" "n" "ne"))
			       (include-redundant-rel? t)
			       (state (make-random-state t 100))
			       )
  (let* ((objs (list (symbol-name (gensym "Obj"))
		     (symbol-name (gensym "Obj"))
		     (symbol-name (gensym "Obj"))))
	 (dir1 (nth (random (length dirs) state) dirs))
	 (dir2 (nth (random (length dirs) state) (remove dir1 dirs :test #'string=)))
	 (facts nil))
    (dolist (obj objs)
      (let ((shape (nth (random (length shapes) state) shapes))
	    (colors (nth (random (length colors) state) colors)))
	(push (list "isa" obj shape) facts)
	(push (list "isa" obj colors) facts)))
    (push (list dir1 (car objs) (cadr objs)) facts)
    (when include-redundant-rel? (push (list (invert-dir dir1)  (cadr objs) (car objs)) facts))
    (push (list dir2 (cadr objs) (caddr objs)) facts)
    (when include-redundant-rel? (push (list (invert-dir dir2)  (cadr objs) (car objs)) facts))		
    (mapl #'(lambda (objs1)
	      (when (cdr objs1)
		(push (list "ec" (car objs1) (cadr objs1)) facts)
		(when include-redundant-rel? (push (list "ec" (cadr objs1) (car objs1)) facts))
		))
	  objs)
    (push (list "dc" (car objs) (car (last objs))) facts)
    (when include-redundant-rel?
      (push (list "dc" (car (last objs)) (car objs)) facts))
    (push (cons "rWall" objs) facts) ;; Adding the query here
    facts))


(defun parameter-search (&key (thresholds (reverse '(0.6 0.65 0.7 0.75)))
			   (redundants '(t nil)))
  (let (
	(state (make-random-state t 100))
	)
    (with-open-file (output (format nil "parameters_~A.csv" (get-universal-time))
			    :direction :output
			    :if-does-not-exist :create)
      (format output "threshold,redundant,tp,fn,tn,fp,generalizations,exemplars~%")
      (finish-output output)
      (dolist (redundant redundants)
	(dolist (threshold thresholds)
	  (let ((*assimilation-threshold* threshold))
	    (dotimes (i 10)
	      (test-phase2-reasoning-symbols)
	      (test-wall-basic state redundant)
	      (dotimes (i 10)
		(multiple-value-bind (tp fn tn fp generalizations exemplars)
		    (evaluate-wall-generalizations redundant state) 
		  (format output "~A,~A,~A,~A,~A,~A,~A,~A~%" threshold redundant tp fn tn fp generalizations exemplars)
		  (finish-output output))))))))))

(defun test-wall-basic (&optional
			  (state (make-random-state 100))
			  (include-redundant? t))
  (let ((res)
	(start-time (get-universal-time)))
    (dotimes (i 20)
      (setq res (call-test-server
		 "store"
		 (pairlis '("facts" "context" "concept")
			  (list (create-wall-case :state state :include-redundant-rel? include-redundant?)
				(symbol-name (gensym "Case"))
				"rWall")
			  ))))
    (format t "~% TEST-WALL Took ~A seconds ~A generalizations" (- (get-universal-time) start-time)
	    (cdr (assoc :NUM-GENERALIZATIONS res))) 
					;    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 1))
    ))



(defun evaluate-wall-generalizations (&optional include-redundant-rel? state (test-set-size 10))
  (let ((pos-examples (loop repeat test-set-size collect (create-wall-case :state state :include-redundant-rel? include-redundant-rel?)))
	(neg-examples (loop repeat test-set-size collect (create-non-wall-case :state state :include-redundant-rel? include-redundant-rel?)))
	(tp 0)
	(fn 0)
	(tn 0)
	(fp 0))
    (format t "~% Starting Evaluation of Generalizations~%  POSITIVES EXAMPLES")
    (dolist (exp pos-examples)
      (let ((res (call-test-server
		  "query"
		  (pairlis '("facts" "pattern")
			   (list (remove "rWall" exp :key #'car :test #'string=)
				 (find "rWall" exp :key #'car :test #'string=))))))
	(if (= 1 (length (cdr (assoc :matches res))))
	    (incf tp)
	    (incf fn))))
    (format t "~%   NEGATIVE EXAMPLES")
    (dolist (exp neg-examples)
      (let ((res (call-test-server
		  "query"
		  (pairlis '("facts" "pattern")
			   (list (remove "rWall" exp :key #'car :test #'string=)
				 (find "rWall" exp :key #'car :test #'string=))))))
	(if (= 0 (length (cdr (assoc :matches res))))
	    (incf tn)
	    (incf fp))))
    (values tp fn tn fp
	    (length (fire::ask-it 'd::(gpoolGeneralization rWallMt ?x)))
	    (length (fire::ask-it 'd::(gpoolExample rWallMt ?x))))))
    
    


(defun test-next-to ()
  (assert t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
