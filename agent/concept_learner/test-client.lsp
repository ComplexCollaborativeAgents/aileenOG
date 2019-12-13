;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: test-client.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: November 13, 2019 16:35:48
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Thursday, November 14, 2019 at 10:29:14 by klenk
;;;; ----------------------------------------------------------------------------

(load "server.lsp")

(in-package :aileen)

(defun test-concept-learner-server ()
  (start-server :port 7000) ;; needs to match port in call-test-server.
  (test-reasoning-symbols)
  (test-generalization)
  (clean-tests))

(defun call-test-server (function arguments)
  (cl-json::decode-json-from-string
   (net.xml-rpc:xml-rpc-call
    (net.xml-rpc:encode-xml-rpc-call
     function
     (net.xml-rpc:make-xml-rpc-encoding
      (cl-json::encode-json-alist-to-string arguments)
      :base64))
    :url "http://dubs:7000/ConceptLearner"
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
                        '("rOn"))))
    (assert (equal (cdr (assoc :GPOOL res))
		   "rOnMt"))
    ))

(defun test-generalization ()
  ;; Add two cases for RRed to the RRedMT gpool
  ;; generalize them
  ;; Match a new scene against it
  (let (res pattern)
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

    ;;;PATTERN
    (setf pattern (list "isa" "Object4" "RRed"))
    (setq res (call-test-server "query"
               (pairlis '("facts" "pattern")
                        (list (list (list "isa" "Object4" "CVGreen")
                                    (list "isa" "Object4" "CVPyramid"))
                              pattern))))
    (format t "~% query result = ~A" res)
    (assert (= 0 (length (cdr (assoc :MATCHES res)))))
    
;;;    (setq res (call-test-server "query"
;;;               (pairlis '("facts" "pattern")
;;;                        (list (list (list "isa" "O3" "CVRed")
;;;                                    (list "isa" "O3" "CVCube"))
;;;                               pattern))))
;;;    (assert (= 1 (length (cdr (assoc :MATCHES res)))))
;;;    (assert (equal "O3" (car (cdr (assoc :MATCHES res)))))
;;;    (assert (equal pattern (cdr (assoc :PATTERN res))))
;;;
;;;    ;; Test deletion of query facts.
;;;    (setq res (call-test-server "query"
;;;               (pairlis '("facts" "pattern")
;;;                        (list (list (list "isa" "O3" "CVBlue")
;;;                                    (list "isa" "O3" "CVPyramid"))
;;;                              pattern))))
;;;    (format t "~% query result = ~A" res)
;;;    (assert (= 0 (length (cdr (assoc :MATCHES res)))))
    
    (when nil
    ;; REMOVE
    (setq res (call-test-server
               "remove"
               (pairlis (list "concept")
                        (list "RRed"))))
    (assert (cdr (assoc :SUCCESS res)))

    ;;;PATTERN
    (setf pattern (list "isa" "O3" "RRed"))
    (setq res (call-test-server
               "filter_scene_by_expression"
               (pairlis '("facts" "context" "pattern")
                        (list (list (list "isa" "O3" "CVRed")
                                    (list "isa" "O3" "CVCube"))
                              "Test3" ;;Id
                              pattern))))
    (format t "~% filter_scene_by_expression returned ~A" res)
    (assert (= 0 (length (cdr (assoc :MATCHES res)))))
    (assert (equal pattern (cdr (assoc :PATTERN res)))))
    ))

(defun clean-tests ()
  (fire:kb-forget (car(fire:retrieve-references 'd::RRed)))
  (fire:kb-forget (car(fire:retrieve-references 'd::rOn)))
  )
	
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
