;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: test-client.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: November 13, 2019 16:35:48
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Monday, December 16, 2019 at 08:33:34 by klenk
;;;; ----------------------------------------------------------------------------

(load "server.lsp")

(in-package :aileen)

(defparameter *test-port* 7000)

(defun test-concept-learner-server ()
  (start-server :port *test-port*) ;; needs to match port in call-test-server.
  (test-reasoning-symbols)
  (test-generalization-obj)
  (test-generalization-rel)
  
  (clean-tests))

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
               "create_reasoning_predicate"
               (pairlis '("predicate")
                        '("rRight"))))
    (assert (equal (cdr (assoc :GPOOL res))
		   "rRightMt"))
    ))

(defun test-generalization-obj ()
  ;; Add two cases for RRed to the RRedMT gpool
  ;; generalize them
  ;; Match a new scene against it
  ;; verifies that removing facts works.
  (let (res pattern)
    ;; TEST STORE
    (setq res (call-test-server "store"
               (pairlis '("facts" "context" "concept")
                        (list (list (list "isa" "O1" "RRed")
                                    (list "isa" "O1" "CVRed")
                                    (list "isa" "O1" "CVCylinder"))
                              "Test1" ;;Id
                              "RRed"))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 1))
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
    (assert (equal "Object3" (car (cdr (assoc :MATCHES res)))))
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

(defun make-test-gen-rel-gpool ()
  (let (res pattern)
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
                        (list '(("isa" "Obj9A" "CVCube") ("isa" "Obj9A" "CVBlue")
				("isa" "Obj9B" "CVPyramid") ("isa" "Obj9B" "CVGreen")
				("n" "Obj9A" "Obj9B") ("dc" "Obj9A" "Obj9B")
				("rRight" "Obj9A" "Obj9B"))
                              "Test9" ;;Id
                              "rRight"))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 0))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 1))))


(defun test-generalization-rel ()
  ;; Add two cases for rRight to the rRightMT gpool
  ;; generalize them
  ;; Match a new scene against it
  ;; verifies that removing facts works.
  (make-test-gen-rel-gpool)
  (query-test-gen-rel-gpool)
  (query-test-gen-rel-var-gpool))

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
    (assert (= 2 (length (cdr (assoc :MATCHES res)))))  
    (assert (find '("rRight" "Obj10A" "Obj10B")
		  (cdr (assoc :MATCHES res)) :test #'equal))
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
    (assert (= 3 (length (cdr (assoc :MATCHES res)))))
    (assert (find '("rRight" "Obj10A" "Obj10B")
		  (cdr (assoc :MATCHES res)) :test #'equal))
    (assert (find '("rRight" "Obj10A" "Obj10C")
		  (cdr (assoc :MATCHES res)) :test #'equal))
    (assert (find '("rRight" "Obj10B" "Obj10C")
		   (cdr (assoc :MATCHES res)) :test #'equal))
    ))

(defun clean-tests ()
  (cl-user::nuke-gpool 'd::rRightMt)
  (cl-user::nuke-gpool 'd::rOnMt)
  )
	
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
