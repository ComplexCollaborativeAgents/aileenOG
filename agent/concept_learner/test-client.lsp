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
  (let (res)
    (setq res (call-test-server
               "add_case_to_gpool"
               (pairlis '("facts" "context" "gpool")
                        (list (list (list "isa" "O1" "RRed")
                                    (list "isa" "O1" "CVRed")
                                    (list "isa" "O1" "CVCylinder"))
                              "Test1" ;;Id
                              "RRedMt"))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 1))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 0))

    (setq res (call-test-server
               "add_case_to_gpool"
               (pairlis '("facts" "context" "gpool")
                        (list (list (list "isa" "O2" "RRed")
                                    (list "isa" "O2" "CVRed")
                                    (list "isa" "O2" "CVSphere"))
                              "Test2" ;;Id
                              "RRedMt"))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 0))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 1))

    ;;;PATTERN
    (setq res (call-test-server
               "filter_scene_by_expression"
               (pairlis '("facts" "context" "gpool" "pattern")
                        (list (list (list "isa" "O3" "CVRed")
                                    (list "isa" "O3" "CVCube"))
                              "Test3" ;;Id
                              "RRedMt"
                              (list "isa" "O3" "RRed")))))
    ))

(defun clean-tests ()
  (fire:kb-forget (car(fire:retrieve-references 'd::RRed)))
  (fire:kb-forget (car(fire:retrieve-references 'd::rOn)))
  )
	
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
