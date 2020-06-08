;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: test-client.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: November 13, 2019 16:35:48
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Tuesday, May 19, 2020 at 15:46:37 by klenk
;;;; ----------------------------------------------------------------------------

(load "server.lsp")

(in-package :aileen)

(defparameter *test-port* 7000)

(defun test-concept-learner-server (&key (clean? t))
  (start-server :port *test-port*) ;; needs to match port in call-test-server.
  (when clean? (checkpoint-init))
  (test-reasoning-symbols)
  (test-generalization-obj)
  (test-generalization-rel)
  (test-generalization-action)
  (when clean? (restore-init))
;  (when clean? (clean-tests))
  )

(defun checkpoint-init ()
  (let ((res (call-test-server
	      "checkpoint_kb"
	      (pairlis '("dir")
		       '("init_kb")))))
    (assert (equal (cdr (assoc :DIR res)) "init_kb"))))

(defun restore-init ()
  (let ((res (call-test-server
               "restore_kb"
               (pairlis '("dir")
                        '("init_kb")))))
  (assert (equal (cdr (assoc :DIR res)) "init_kb"))))

  

(defun call-test-server (function arguments)
  (cl-json::decode-json-from-string
   (net.xml-rpc:xml-rpc-call
    (net.xml-rpc:encode-xml-rpc-call
     function
     (net.xml-rpc:make-xml-rpc-encoding
      (cl-json::encode-json-alist-to-string arguments)
      :base64))
    :url (format nil "http://dubs:~A/ConceptLearner" *test-port*)
    )))

(defun test-reasoning-symbols ()
  (let (res)
    (setq res (call-test-server
               "create_reasoning_symbol"
               (pairlis '("symbol")
                        '("RRed"))))
    (assert (equal (cdr (assoc :GPOOL res))
                   "RRedMt"))
    (setq res (call-test-server
               "create_reasoning_symbol"
               (pairlis '("symbol")
                        '("RBox"))))

    (setq res (call-test-server
               "create_reasoning_predicate"
               (pairlis '("predicate")
                        '("rRight"))))
    (assert (equal (cdr (assoc :GPOOL res))
		   "rRightMt"))

        (setq res (call-test-server
               "create_reasoning_action"
               (pairlis '("action" "arity")
                        '("rMove" 2))))
    (assert (equal (cdr (assoc :GPOOL res))
		   "rMoveMt"))
    ))

(defun test-generalization-obj ()
  ;; This also tests checkpoint and restore
  ;; Add two cases for RRed to the RRedMT gpool
  ;; generalize them
  ;; Match a new scene against it
  ;; verifies that removing facts works.
  (let (res pattern)
    (setq res (call-test-server
               "checkpoint_kb"
               (pairlis '("dir")
                        '("test_kb"))))
    ;; TEST STORE
    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list (list (list "isa" "O0" "RRed")
                                    (list "isa" "O0" "CVRed")
                                    (list "isa" "O0" "CVCylinder"))
                              "Test0" ;;Id
                              "RRed"))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 1))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 0))

    (setq res (call-test-server
               "restore_kb"
               (pairlis '("dir")
                        '("test_kb"))))
    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list (list (list "isa" "O1" "RRed")
                                    (list "isa" "O1" "CVRed")
                                    (list "isa" "O1" "CVCylinder"))
                              "Test1" ;;Id
                              "RRed"))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 1))  ;;becasue we restored the kb this is 1
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 0))



    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list (list (list "isa" "O2" "RRed")
                                    (list "isa" "O2" "CVRed")
                                    (list "isa" "O2" "CVSphere"))
                              "Test2" ;;Id
                              "RRed"))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 0))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 1))

    ;; TEST QUERY
    (setf pattern (list "isa" "Object3" "RRed"))
    (setq res (call-test-server "query"
               (pairlis '("facts" "pattern")
                        (list (list (list "isa" "Object3" "CVBlue")
                                    (list "isa" "Object3" "CVPyramid"))
                              pattern))))
    (assert (= 0 (length (cdr (assoc :MATCHES res)))))
    
    (setq res (call-test-server "query"
				(pairlis '("facts" "pattern")
                        (list (list (list "isa" "Object3" "CVRed")
                                    (list "isa" "Object3" "CVCube"))
                               pattern))))
    (assert (= 1 (length (cdr (assoc :MATCHES res)))))
    (assert (equal (list "isa" "Object3" "RRed") (car (cdr (assoc :MATCHES res)))))
    (assert (equal pattern (cdr (assoc :PATTERN res))))

    ;; Test deletion of query facts.
    (setq res (call-test-server "query"
               (pairlis '("facts" "pattern")
                        (list (list (list "isa" "Object3" "CVBlue")
                                    (list "isa" "Object3" "CVPyramid"))
                              pattern))))
    (assert (= 0 (length (cdr (assoc :MATCHES res)))))

;;;    ;; TEST REMOVE
;;;    (setq res (call-test-server
;;;               "remove"
;;;               (pairlis (list "concept")
;;;                        (list "RRed"))))
;;;    (assert (cdr (assoc :SUCCESS res)))
;;;
;;;    ;;;PATTERN
;;;    (setf pattern (list "isa" "O3" "RRed"))
;;;    (setq res (call-test-server
;;;               "filter_scene_by_expression"
;;;               (pairlis '("facts" "context" "pattern")
;;;                        (list (list (list "isa" "O3" "CVRed")
;;;                                    (list "isa" "O3" "CVCube"))
;;;                              "Test3" ;;Id
;;;                              pattern))))
;;;    (format t "~% filter_scene_by_expression returned ~A" res)
;;;    (assert (= 0 (length (cdr (assoc :MATCHES res)))))
;;;    (assert (equal pattern (cdr (assoc :PATTERN res))))
    ))

(defun add-case-to-gen-rel-gpool ()
 (let (res)
    ;;; Add two cases without cubes to remove cubes from the generalization
    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list '(("isa" "Obj11A" "CVCylinder") ("isa" "Obj11A" "CVGreen")
				("isa" "Obj11B" "CVCylinder") ("isa" "Obj11B" "CVBlue")
				("n" "Obj11A" "Obj11B") ("ec" "Obj11A" "Obj11B")
				("rRight" "Obj11A" "Obj11B"))
                              "Test11" ;;Id
                              "rRight"))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 0))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 1)) ;;;still one generalization
    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list '(("isa" "Obj12A" "CVPyramid") ("isa" "Obj12A" "CVGreen")
				("isa" "Obj12B" "CVCylinder") ("isa" "Obj12B" "CVBlue")
				("n" "Obj12A" "Obj12B") ("dc" "Obj12A" "Obj12B")
				("rRight" "Obj12A" "Obj12B"))
                              "Test12" ;;Id
                              "rRight"))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 0))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 1)) ;;;still one generalization
  ))

(defun make-test-gen-rel-gpool ()
  (let (res )
    ;; TEST STORE
    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list '(("isa" "Obj8A" "CVCylinder") ("isa" "Obj8A" "CVRed")
				("isa" "Obj8B" "CVCube") ("isa" "Obj8B" "CVBlue")
				("n" "Obj8A" "Obj8B") ("ec" "Obj8A" "Obj8B")
				("rRight" "Obj8A" "Obj8B"))
                              "Test8" ;;Id
                              "rRight"))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 1))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 0))

    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list '(("isa" "Obj9A" "CVCube") ("isa" "Obj9A" "CVRed")
				("isa" "Obj9B" "CVCube") ("isa" "Obj9B" "CVGreen")
				("n" "Obj9A" "Obj9B") ("dc" "Obj9A" "Obj9B")
				("rRight" "Obj9A" "Obj9B"))
                              "Test9" ;;Id
                              "rRight"))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 0))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 1))))

;; Not called currently
(defun make-test-gen-rel-gpool-1 ()
  (let (res )
    (setq res (call-test-server
               "create_reasoning_predicate"
               (pairlis '("predicate")
                        '("rNextTo"))))
    (assert (equal (cdr (assoc :GPOOL res))
		   "rNextToMt"))

    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list '(("isa" "Obj8A" "CVCube") ("isa" "Obj8A" "CVRed")
				("isa" "Obj8B" "CVCylinder") ("isa" "Obj8B" "CVBlue")
				("n" "Obj8A" "Obj8B") ("s" "Obj8B" "Obj8A") 
				("dc" "Obj8B" "Obj8A")("dc" "Obj8A" "Obj8B")
				("rNextTo" "Obj8A" "Obj8B"))
                              "Test8A" ;;Id
                              "rNextTo"))))


    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list '(("isa" "Obj9A" "CVCube") ("isa" "Obj9A" "CVBlue")
				("isa" "Obj9B" "CVSphere") ("isa" "Obj9B" "CVGreen")
				("e" "Obj9A" "Obj9B")("w" "Obj9B" "Obj9A")
				("po" "Obj9A" "Obj9B")("po" "Obj9B" "Obj9A")
				("rNextTo" "Obj9A" "Obj9B"))
                              "Test9A" ;;Id
                              "rNextTo"))))
;;    (break) ;; The nextTo facts don't appear in the mapping
    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list '(("isa" "Obj9A" "CVSphere") ("isa" "Obj9A" "CVBlue")
				("isa" "Obj9B" "CVCube") ("isa" "Obj9B" "CVGreen")
				("nw" "Obj9A" "Obj9B")("se"  "Obj9B" "Obj9A")
				("po" "Obj9A" "Obj9B")("po" "Obj9B" "Obj9A")
				("rNextTo" "Obj9A" "Obj9B"))
                              "Test10A" ;;Id
                              "rNextTo"))))

    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list '(("isa" "Obj9A" "CVSphere") ("isa" "Obj9A" "CVBlue")
				("isa" "Obj9B" "CVCube") ("isa" "Obj9B" "CVGreen")
				("sw" "Obj9A" "Obj9B")("ne"  "Obj9B" "Obj9A")
				("po" "Obj9A" "Obj9B")("po"  "Obj9B" "Obj9A")
				("rNextTo" "Obj9A" "Obj9B"))
                              "Test11A" ;;Id
                              "rNextTo"))))
    ))



(defun test-generalization-rel ()
  ;; Add two cases for rRight to the rRightMT gpool
  ;; generalize them
  ;; Match a new scene against it
  ;; verifies that removing facts works.
  (make-test-gen-rel-gpool)  ;;; an almost perfect generalization (requires the second argument to be cube)
  (query-test-gen-rel-gpool-1) ;;; does not find the generalization
  (add-case-to-gen-rel-gpool) ;;; add an example where it is not a cube
  (query-test-gen-rel-gpool) ;;; does not find the generalization
  (query-test-gen-rel-var-gpool))

(defun query-test-gen-rel-gpool-1 ()
  (let (res pattern)
    ;; TEST QUERY Should not match
    (setf pattern (list "rRight" "Obj10A" "Obj10B"))
    (setq res (call-test-server "query"
				(pairlis '("facts" "pattern")
                        (list '(("isa" "Obj10A" "CVPyramid") ("isa" "Obj10A" "CVBlue")
				("isa" "Obj10B" "CVPyramid") ("isa" "Obj10B" "CVGreen")
				("n" "Obj10A" "Obj10B") ("dc" "Obj10A" "Obj10B")
				)
                              pattern))))
    (assert (= 0 (length (cdr (assoc :MATCHES res))))) ;;verify the concept is over specific
    (setf pattern (list "rRight" "Obj10A" "Obj10B"))
    (setq res (call-test-server "query"
				(pairlis '("facts" "pattern")
                        (list '(("isa" "Obj10A" "CVPyramid") ("isa" "Obj10A" "CVRed")
				("isa" "Obj10B" "CVCube") ("isa" "Obj10B" "CVGreen")
				("n" "Obj10A" "Obj10B") ("dc" "Obj10A" "Obj10B")
				)
                              pattern))))
    (assert (= 1 (length (cdr (assoc :MATCHES res)))))
;;;This test should match, but greedy merge is missing the CVCube fact
;;;A simple fix would be to extend the match any reverse candidate inferences that are already true, but I'm not sure all the things that work for it
    ;; also exhaustive-sme works, but it can't be used with filters
    ;; 12/18 I have emailed Ken and Tom
    ))

(defun query-test-gen-rel-gpool ()
  (let (res pattern)
    ;; TEST QUERY Should not match
    (setf pattern (list "rRight" "Obj10A" "Obj10B"))
    (setq res (call-test-server "query"
				(pairlis '("facts" "pattern")
                        (list '(("isa" "Obj10A" "CVPyramid") ("isa" "Obj10A" "CVBlue")
				("isa" "Obj10B" "CVCube") ("isa" "Obj10B" "CVGreen")
				("s" "Obj10A" "Obj10B") ("dc" "Obj10A" "Obj10B")
				)
                              pattern))))
    (assert (= 0 (length (cdr (assoc :MATCHES res)))))
    
    (setq res (call-test-server "query"
				(pairlis '("facts" "pattern")
                        (list '(("isa" "Obj10A" "CVPyramid") ("isa" "Obj10A" "CVGreen")
				("isa" "Obj10B" "CVCube") ("isa" "Obj10B" "CVBlue")
				("n" "Obj10A" "Obj10B") ("dc" "Obj10A" "Obj10B")
				)
                               pattern))))
    (assert (= 1 (length (cdr (assoc :MATCHES res)))))  
    (assert (equal '("rRight" "Obj10A" "Obj10B")
		   (car (cdr (assoc :MATCHES res)))))  ;;unclear how to match this one
    (assert (equal pattern (cdr (assoc :PATTERN res))))

    ;; Test deletion of query facts.
    (setq res (call-test-server "query"
               (pairlis '("facts" "pattern")
                        (list '(("isa" "Obj10A" "CVPyramid") ("isa" "Obj10A" "CVBlue")
				("isa" "Obj10B" "CVCube") ("isa" "Obj10B" "CVGreen")
				("s" "Obj10A" "Obj10B") ("dc" "Obj10A" "Obj10B")
				)
                              pattern))))
    (assert (= 0 (length (cdr (assoc :MATCHES res)))))

    ))


(defun query-test-gen-rel-var-gpool ()
  ;; tests with multiple bindings
  (let (res pattern)
    (setf pattern (list "rRight" "Obj10A" "?Obj"))
    (setq res (call-test-server "query"
				(pairlis '("facts" "pattern")
                        (list '(("isa" "Obj10A" "CVPyramid") ("isa" "Obj10A" "CVBlue")
				("isa" "Obj10B" "CVCube") ("isa" "Obj10B" "CVGreen")
				("s" "Obj10A" "Obj10B") ("dc" "Obj10A" "Obj10B")
				("isa" "Obj10C" "CVPyramid") ("isa" "Obj10C" "CVBlue")
				("s" "Obj10A" "Obj10C") ("dc" "Obj10A" "Obj10C")
				("s" "Obj10B" "Obj10C") ("po" "Obj10B" "Obj10C")
				)
                              pattern))))
    (assert (= 0 (length (cdr (assoc :MATCHES res)))))

    (setf pattern (list "rRight" "Obj10A" "?Obj"))
    (setq res (call-test-server "query"
				(pairlis '("facts" "pattern")
                        (list '(("isa" "Obj10A" "CVPyramid") ("isa" "Obj10A" "CVBlue")
				("isa" "Obj10B" "CVCube") ("isa" "Obj10B" "CVGreen")
				("n" "Obj10A" "Obj10B") ("dc" "Obj10A" "Obj10B")
				("isa" "Obj10C" "CVPyramid") ("isa" "Obj10C" "CVBlue")
				("n" "Obj10A" "Obj10C") ("dc" "Obj10A" "Obj10C")
				("s" "Obj10B" "Obj10C") ("po" "Obj10B" "Obj10C")
				)
                               pattern))))
    (assert (= 1 (length (cdr (assoc :MATCHES res)))))  
;    (assert (find '("rRight" "Obj10A" "Obj10B") ;this would be true of OBJ10B was Blue
;		  (cdr (assoc :MATCHES res)) :test #'equal))
    (assert (find '("rRight" "Obj10A" "Obj10C")
		   (cdr (assoc :MATCHES res)) :test #'equal))
    

    ;;; test both variables
    (setf pattern (list "rRight" "?Obj1" "?Obj2"))
    (setq res (call-test-server "query"
               (pairlis '("facts" "pattern")
                        (list '(("isa" "Obj10A" "CVPyramid") ("isa" "Obj10A" "CVBlue")
				("isa" "Obj10B" "CVCube") ("isa" "Obj10B" "CVGreen")
				("n" "Obj10A" "Obj10B") ("dc" "Obj10A" "Obj10B")
				("isa" "Obj10C" "CVPyramid") ("isa" "Obj10C" "CVBlue")
				("n" "Obj10A" "Obj10C") ("dc" "Obj10A" "Obj10C")
				("n" "Obj10B" "Obj10C") ("po" "Obj10B" "Obj10C")
				)
                              pattern))))
    (assert (= 2 (length (cdr (assoc :MATCHES res)))))
;    (assert (find '("rRight" "Obj10A" "Obj10B")  ;this would be true of OBJ10B was Blue
;		  (cdr (assoc :MATCHES res)) :test #'equal))
    (assert (find '("rRight" "Obj10A" "Obj10C")
		  (cdr (assoc :MATCHES res)) :test #'equal))
    (assert (find '("rRight" "Obj10B" "Obj10C")
		   (cdr (assoc :MATCHES res)) :test #'equal))
    ))


(defun test-generalization-action ()
  ;; Add two cases for rMove to the rMoveMt gpool
  ;; generalize them
  ;; Match a new scene against it
  ;; verifies that removing facts works.
  (make-test-gen-action-gpool)  
  (query-test-gen-action-gpool)
  (project-test-gen-action-gpool)
  )

;; Move the red to the right of the box
(defparameter *action-test-case-12*
  'd::((rMove Obj12A (rRight Obj12A Obj12B))
       (holdsIn Time12_0 (isa Obj12A CVRed))
       (holdsIn Time12_0 (isa Obj12A RRed))
       (holdsIn Time12_0 (isa Obj12A CVCone))
       (holdsIn Time12_0 (isa Obj12B CVBlue))
       (holdsIn Time12_0 (isa Obj12B CVBox))
       (holdsIn Time12_0 (isa Obj12B RBox))
       (holdsIn Time12_0 (ne Obj12A Obj12B))
       (holdsIn Time12_0 (dc Obj12A Obj12B))
       (holdsIn Time12_0 (dc Obj12B Obj12A))
       (holdsIn Time12_1 (isa Obj12A CVRed))
       (holdsIn Time12_1 (isa Obj12A RRed))
       (holdsIn Time12_1 (isa Obj12A CVCone))
       (holdsIn Time12_1 (isa Obj12B CVBlue))
       (holdsIn Time12_1 (isa Obj12B CVBox))
       (holdsIn Time12_1 (isa Obj12B RBox))
       (holdsIn Time12_1 (holdsInHand Aileen1 Obj12A))
       ;;(holdsIn Time12_1 (dc Obj12A Obj12B)) ;;Do we just remove all spatial relations?
       (holdsIn Time12_2 (isa Obj12A CVRed))
       (holdsIn Time12_2 (isa Obj12A RRed))
       (holdsIn Time12_2 (isa Obj12A CVCone))
       (holdsIn Time12_2 (isa Obj12B CVBlue))
       (holdsIn Time12_2 (isa Obj12B CVBox))
       (holdsIn Time12_2 (isa Obj12B RBox))
       (holdsIn Time12_2 (n Obj12A Obj12B))
       (holdsIn Time12_2 (dc Obj12A Obj12B))
       (holdsIn Time12_2 (dc Obj12B Obj12A))
       (holdsIn Time12_2 (rRight Obj12A Obj12B))
       (isa Time12_0 AileenActionStartTime)
       (startsAfterEndingOf Time12_1 Time12_0)
       (startsAfterEndingOf Time12_2 Time12_1)
       (aileenTerminalTransition Time12_2 Time12_1)
       ))

;;; move the box to the right of the cylinder
(defparameter *action-test-case-13*
  'd::((rMove Obj13B (rRight Obj13B Obj13A))
       (holdsIn Time13_0 (isa Obj13A CVGreen))
       (holdsIn Time13_0 (isa Obj13A CVCylinder))
       (holdsIn Time13_0 (isa Obj13B CVGreen))
       (holdsIn Time13_0 (isa Obj13B CVBox))
       (holdsIn Time13_0 (isa Obj13B RBox))
       (holdsIn Time13_0 (w Obj13A Obj13B))
       (holdsIn Time13_0 (dc Obj13A Obj13B))
       (holdsIn Time13_0 (dc Obj13B Obj13A))
       (holdsIn Time13_1 (isa Obj13A CVGreen))
       (holdsIn Time13_1 (isa Obj13A CVCylinder))
       (holdsIn Time13_1 (isa Obj13B CVGreen))
       (holdsIn Time13_1 (isa Obj13B CVBox))
       (holdsIn Time13_1 (isa Obj13B RBox))
       (holdsIn Time13_1 (holdsInHand Aileen1 Obj13B))
       ;;(holdsIn Time13_1 (dc Obj13A Obj13B)) ;;Do we just remove all spatial relations?
       (holdsIn Time13_2 (isa Obj13A CVGreen))
       (holdsIn Time13_2 (isa Obj13A CVCylinder))
       (holdsIn Time13_2 (isa Obj13B CVGreen))
       (holdsIn Time13_2 (isa Obj13B CVBox))
       (holdsIn Time13_2 (isa Obj13B RBox))
       (holdsIn Time13_2 (n Obj13B Obj13A))
       (holdsIn Time13_2 (dc Obj13B Obj13A))
       (holdsIn Time13_2 (dc Obj13A Obj13B))
       (holdsIn Time13_2 (rRight Obj13B Obj13A))
       (isa Time13_0 AileenActionStartTime)
       (startsAfterEndingOf Time13_1 Time13_0)
       (startsAfterEndingOf Time13_2 Time13_1) 
       (aileenTerminalTransition Time13_2 Time13_1) 
       ))



(defun make-test-gen-action-gpool ()
  (let (res )
    ;; TEST STORE
    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list (symbols->strs *action-test-case-12*)
                              "Test12" ;;Id
                              "rMove"))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 1))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 0))

    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list (symbols->strs *action-test-case-13*)
                              "Test13" ;;Id
                              "rMove")))) 
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 0))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 1))))


;;; Do we include any reasoning symbols? For recognition, not yet
;;; Or if we are going to include them, we would first try to generate
;;; reasoning symbols for each object, and each pair of objects, then
;;; we could match against the gpool
(defparameter *action-test-case-14*
  'd::((holdsIn Time14_0 (isa Obj14A CVBlue))
       (holdsIn Time14_0 (isa Obj14A CVCylinder))
       (holdsIn Time14_0 (isa Obj14B CVGreen))
       (holdsIn Time14_0 (isa Obj14B CVPyramid))
       (holdsIn Time14_0 (nw Obj14A Obj14B))
       (holdsIn Time14_0 (dc Obj14A Obj14B))
       (holdsIn Time14_0 (dc Obj14B Obj14A))
       (holdsIn Time14_1 (isa Obj14A CVBlue))
       (holdsIn Time14_1 (isa Obj14A CVCylinder))
       (holdsIn Time14_1 (isa Obj14B CVGreen))
       (holdsIn Time14_1 (isa Obj14B CVPyramid))
       (holdsIn Time14_1 (holdsInHand Aileen1 Obj14A))
       ;;(holdsIn Time14_1 (dc Obj14A Obj14B)) ;;Do we just remove all spatial relations?
       (holdsIn Time14_2 (isa Obj14A CVBlue))
       (holdsIn Time14_2 (isa Obj14A CVCylinder))
       (holdsIn Time14_2 (isa Obj14B CVGreen))
       (holdsIn Time14_2 (isa Obj14B CVPyramid))
       (holdsIn Time14_2 (n Obj14B Obj14A))
       (holdsIn Time14_2 (dc Obj14B Obj14A))
       (holdsIn Time14_2 (dc Obj14A Obj14B))
       (holdsIn Time14_2 (rRight Obj14B Obj14A)) ;; spatial relation recognized
       (isa Time14_0 AileenActionStartTime)
       (startsAfterEndingOf Time14_1 Time14_0)
       (startsAfterEndingOf Time14_2 Time14_1)
       (aileenTerminalTransition Time14_2 Time14_1)
       ))

(defun query-test-gen-action-gpool ()
  (let (res pattern)
    (setf pattern (list "rMove" "Obj14B" (list "rRight" "Obj14B" "Obj14A")))
    (setq res (call-test-server "query"
				(pairlis '("facts" "pattern")
					 (list (symbols->strs *action-test-case-14*)
					       pattern))))
    (assert (= 1 (length (cdr (assoc :MATCHES res)))))

    (setf pattern (list "rMove" "Obj14A" (list "rRight" "Obj14B" "Obj14A")))
    (setq res (call-test-server "query"
				(pairlis '("facts" "pattern")
					 (list (symbols->strs *action-test-case-14*)
					       pattern))))
    (assert (= 0 (length (cdr (assoc :MATCHES res)))))
    ))


(defparameter *action-test-case-15*
  'd::((isa Time15_0 AileenActionStartTime)
       (holdsIn Time15_0 (isa Obj15A CVBlue))
       (holdsIn Time15_0 (isa Obj15A CVCylinder))
       (holdsIn Time15_0 (isa Obj15B CVGreen))
       (holdsIn Time15_0 (isa Obj15B CVPyramid))
       (holdsIn Time15_0 (nw Obj15A Obj15B))
       (holdsIn Time15_0 (dc Obj15A Obj15B))
       (holdsIn Time15_0 (dc Obj15B Obj15A))
       (rMove Obj15A (rRight Obj15A Obj15B))
       ))

(defparameter *action-test-case-15-1*
  'd::((holdsIn Time15_0 (isa Obj15A CVBlue))
       (holdsIn Time15_0 (isa Obj15A CVCylinder))
       (holdsIn Time15_0 (isa Obj15B CVGreen))
       (holdsIn Time15_0 (isa Obj15B CVPyramid))
       (holdsIn Time15_0 (nw Obj15A Obj15B))
       (holdsIn Time15_0 (dc Obj15A Obj15B))
       (holdsIn Time15_0 (dc Obj15B Obj15A))
       (holdsIn Time15_1 (isa Obj15A CVBlue))
       (holdsIn Time15_1 (isa Obj15A CVCylinder))
       (holdsIn Time15_1 (isa Obj15B CVGreen))
       (holdsIn Time15_1 (isa Obj15B CVPyramid))
       (holdsIn Time15_1 (holdsInHand Aileen15 Obj15A))
       (rMove Obj15A (rRight Obj15A Obj15B))
       (isa Time15_0 AileenActionStartTime)
       (startsAfterEndingOf Time15_1 Time15_0)       
       ))

(defun project-test-gen-action-gpool ()
  (let (res)
    (setq res (call-test-server "project"
				(pairlis '("facts" "action")
					 (list (symbols->strs *action-test-case-15*)
					       "rMove"
					       ))))
    (let* ((cis (str->symbols (cdr (assoc :CIS res))))
	   (next-state (second (find 'd::startsAfterEndingOf cis :key #'car)))
	   (next-state-facts (remove-if-not
			      #'(lambda (fact)
				  (and (equal 'd::holdsIn (car fact))
				       (equalp next-state (second fact))))
			      cis)))
      ;; Check if the inferences include holding the object in hand
      (assert (find `(d::holdsIn ,next-state
				  (d::holdsInHand (d::AnalogySkolemFn d::Aileen1) d::Obj15A))
		    next-state-facts :test #'equalp))
      ;; Check that the next state is not a terminal state
      (assert (not (find-if #'(lambda (ci)
				(and (eql 'd::aileenTerminalTransition (car ci))
				     (equalp next-state (second ci))))
			    cis))))

    
    (setq res (call-test-server "project"
				(pairlis '("facts" "action")
					 (list (symbols->strs *action-test-case-15-1*)
					       "rMove"
					       ))))
    (let* ((cis (str->symbols (cdr (assoc :CIS res))))
	   (next-state (second (find 'd::startsAfterEndingOf cis :key #'car)))
	   (next-state-facts (remove-if-not
			      #'(lambda (fact)
				  (and (equal 'd::holdsIn (car fact))
				       (equalp next-state (second fact))))
			      cis)))
      ;;; Check that the next state includes the following spatial relationships
      (dolist (fact 'd::((dc Obj15B Obj15A)
			 (rRight Obj15A Obj15B)
			 (n Obj15A Obj15B)))
	(assert (find `(d::holdsIn ,next-state ,fact) next-state-facts :test #'equalp)))
      ;;; check that the next state is the last state of the action
      (assert (find-if #'(lambda (ci)
			   (and (eql 'd::aileenTerminalTransition (car ci))
				(equalp next-state (second ci))))
		       cis))
      )))



(defun clean-tests ()
  (cl-user::nuke-gpool 'd::rMoveMt)
  (cl-user::nuke-gpool 'd::rRightMt)
  (cl-user::nuke-gpool 'd::rNextToMt)
  (cl-user::nuke-gpool 'd::rOnMt)
  (cl-user::nuke-gpool 'd::RRedMt)
  (cl-user::nuke-gpool 'd::RBoxMt)
  )
	
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; testing out minimal ascension in SAGE

;; right is n

(defparameter *action-test-case-16*
  'd::((rMove Obj16B (rRight Obj16B Obj16A))
       (holdsIn Time16_0 (isa Obj16A CVGreen))
       (holdsIn Time16_0 (isa Obj16A CVCylinder))
       (holdsIn Time16_0 (isa Obj16B CVGreen))
       (holdsIn Time16_0 (isa Obj16B CVBox))
       (holdsIn Time16_0 (isa Obj16B RBox))
       (holdsIn Time16_0 (w Obj16A Obj16B))
       (holdsIn Time16_0 (dc Obj16A Obj16B))
       (holdsIn Time16_0 (dc Obj16B Obj16A))
       (holdsIn Time16_1 (isa Obj16A CVGreen))
       (holdsIn Time16_1 (isa Obj16A CVCylinder))
       (holdsIn Time16_1 (isa Obj16B CVGreen))
       (holdsIn Time16_1 (isa Obj16B CVBox))
       (holdsIn Time16_1 (isa Obj16B RBox))
       (holdsIn Time16_1 (holdsInHand Aileen1 Obj16B))
       ;;(holdsIn Time16_1 (dc Obj16A Obj16B)) ;;Do we just remove all spatial relations?
       (holdsIn Time16_2 (isa Obj16A CVGreen))
       (holdsIn Time16_2 (isa Obj16A CVCylinder))
       (holdsIn Time16_2 (isa Obj16B CVGreen))
       (holdsIn Time16_2 (isa Obj16B CVBox))
       (holdsIn Time16_2 (isa Obj16B RBox))
       (holdsIn Time16_2 (n Obj16B Obj16A))
       (holdsIn Time16_2 (dc Obj16B Obj16A))
       (holdsIn Time16_2 (dc Obj16A Obj16B))
       (holdsIn Time16_2 (rRight Obj16B Obj16A))
       (isa Time16_0 AileenActionStartTime)
       (startsAfterEndingOf Time16_1 Time16_0)
       (startsAfterEndingOf Time16_2 Time16_1) ;;Do we want to be explicit about T2 T0 relation, I think not
       ))


(defparameter *action-test-case-17-proj*
  'd::((rMove Obj17B (rOn Obj17B Obj17A))
       (holdsIn Time17_0 (isa Obj17A CVGreen))
       (holdsIn Time17_0 (isa Obj17A CVCylinder))
       (holdsIn Time17_0 (isa Obj17B CVGreen))
       (holdsIn Time17_0 (isa Obj17B CVBox))
       (holdsIn Time17_0 (isa Obj17B RBox))
       (holdsIn Time17_0 (w Obj17A Obj17B))
       (holdsIn Time17_0 (dc Obj17A Obj17B))
       (holdsIn Time17_0 (dc Obj17B Obj17A))
       (isa Time17_0 AileenActionStartTime)
       ))


;; on is east and po
(defparameter *action-test-case-17-gen*
  'd::((rMove Obj17B (rOn Obj17B Obj17A))
       (holdsIn Time17_0 (isa Obj17A CVGreen))
       (holdsIn Time17_0 (isa Obj17A CVCylinder))
       (holdsIn Time17_0 (isa Obj17B CVGreen))
       (holdsIn Time17_0 (isa Obj17B CVBox))
       (holdsIn Time17_0 (isa Obj17B RBox))
       (holdsIn Time17_0 (w Obj17A Obj17B))
       (holdsIn Time17_0 (dc Obj17A Obj17B))
       (holdsIn Time17_0 (dc Obj17B Obj17A))
       (holdsIn Time17_1 (isa Obj17A CVGreen))
       (holdsIn Time17_1 (isa Obj17A CVCylinder))
       (holdsIn Time17_1 (isa Obj17B CVGreen))
       (holdsIn Time17_1 (isa Obj17B CVBox))
       (holdsIn Time17_1 (isa Obj17B RBox))
       (holdsIn Time17_1 (holdsInHand Aileen1 Obj17B))
       ;;(holdsIn Time17_1 (dc Obj17A Obj17B)) ;;Do we just remove all spatial relations?
       (holdsIn Time17_2 (isa Obj17A CVGreen))
       (holdsIn Time17_2 (isa Obj17A CVCylinder))
       (holdsIn Time17_2 (isa Obj17B CVGreen))
       (holdsIn Time17_2 (isa Obj17B CVBox))
       (holdsIn Time17_2 (isa Obj17B RBox))
       (holdsIn Time17_2 (e Obj17B Obj17A))
       (holdsIn Time17_2 (po Obj17B Obj17A))
       (holdsIn Time17_2 (po Obj17A Obj17B))
       (holdsIn Time17_2 (rOn Obj17B Obj17A))
       (isa Time17_0 AileenActionStartTime)
       (startsAfterEndingOf Time17_1 Time17_0)
       (startsAfterEndingOf Time17_2 Time17_1) ;;Do we want to be explicit about T2 T0 relation, I think not
       ))

;; (defun test-minmimal-ascension-action (&aux res)
;;   (when clean? (restore-init))
;;   (setq res (call-test-server
;; 	     "create_reasoning_predicate"
;; 	     (pairlis '("predicate")
;; 		      '("rRight"))))
;;   (assert (equal (cdr (assoc :GPOOL res))
;; 		 "rRightMt"))
;;   (setq res (call-test-server
;; 	     "create_reasoning_predicate"
;; 	     (pairlis '("predicate")
;; 		      '("rOn"))))
;;   (assert (equal (cdr (assoc :GPOOL res))
;; 		 "rOnMt"))
;;   (setq res (call-test-server
;; 	     "create_reasoning_action"
;; 	     (pairlis '("action" "arity")
;; 		      '("rMove" 2))))
;;   (assert (equal (cdr (assoc :GPOOL res))
;; 		 "rMoveMt"))

;;   (setq res (call-test-server "store"
;; 			      (pairlis '("facts" "context" "concept")
;; 				       (symbols->strs *action-test-case-12*)
;; 				       "Test0" ;;Id
;; 				       "RRed")))
;;   (assert (= (cdr (assoc :NUM-EXAMPLES res)) 1))
;;   (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 0))
  
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
