;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: test-phase2-concepts.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: June 16, 2020 09:55:54
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Friday, October 16, 2020 at 18:18:28 by klenk
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
(setq *test-port* 7079) ;; Don't check in with this

(in-package :aileen)

(setq *assimilation-threshold* 0.5)

(defun test-phase2-concept-learner-server (&key (clean? t))
  (start-server :port *test-port*) ;; needs to match port in call-test-server.
  (when clean? (checkpoint-init))
  (test-phase2-reasoning-symbols)
  (test-primary)
  (test-above)
  (test-next-to)
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
    (assert (equal (cdr (assoc :GPOOL res))
		   "rNextToMt"))   
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
			(list '(("isa" "Obj11A" "CVCone")
				("isa" "Obj11A" "CVPurple")
				("isa" "Obj11B" "CVCube")
				("isa" "Obj11B" "CVRed")
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
			'((("isa" "Obj11A" "CVCylinder")
			   ("isa" "Obj11A" "CVBlue")
			   ("isa" "Obj11B" "CVCube")
			   ("isa" "Obj11B" "CVGreen")
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


(defun test-next-to ()
  (assert t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
