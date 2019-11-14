;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: rcpclient.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: November 13, 2019 16:35:48
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Wednesday, November 13, 2019 at 16:36:20 by klenk
;;;; ----------------------------------------------------------------------------

(load "rcpserver.lsp")

(in-package :aileen)

(defun test-concept-learner-server ()
  (start-server)
  (test-reasoning-symbols)
  (test-generalization)
  (clean-tests))


(defun test-generalization ()
  ;; Add two cases for RRed to the RRedMT gpool
  ;; generalize them
  ;; Match a new scene against it
  (let (res)
    (setq res (cl-json:decode-json-from-string 
	      (net.xml-rpc:xml-rpc-call
	       (net.xml-rpc:encode-xml-rpc-call
		"add-case-to-gpool"
		(net.xml-rpc:make-xml-rpc-encoding 
		 (cl-json::encode-json-alist-to-string
		  (pairlis '("facts" "context" "gpool")
			   (list (list (list "isa" "O1" "RRed")
				       (list "isa" "O1" "CVRed")
				       (list "isa" "O1" "CVCylinder"))
				 "Test1" ;;Id
				 "RRedMt")))
		 :base64))
	       :url "http://dubs:8000/ConceptLearner"
	       )))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 1))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 0))

    (setq res (cl-json:decode-json-from-string 
	      (net.xml-rpc:xml-rpc-call
	       (net.xml-rpc:encode-xml-rpc-call
		"add-case-to-gpool"
		(net.xml-rpc:make-xml-rpc-encoding 
		 (cl-json::encode-json-alist-to-string
			      (pairlis '("facts" "context" "gpool")
				       (list (list (list "isa" "O2" "RRed")
						   (list "isa" "O2" "CVRed")
						   (list "isa" "O2" "CVSphere"))
					     "Test2" ;;Id
					     "RRedMt")))
		 :base64))
	       :url "http://dubs:8000/ConceptLearner"
	       )))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 0))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 1))

    ;;;PATTERN
    (setq res (cl-json:decode-json-from-string 
	      (net.xml-rpc:xml-rpc-call
	       (net.xml-rpc:encode-xml-rpc-call
		"match-case-against-gpool"
		(net.xml-rpc:make-xml-rpc-encoding 
		 (cl-json::encode-json-alist-to-string
			      (pairlis '("facts" "context" "gpool" "pattern")
				       (list (list (list "isa" "O3" "CVRed")
						   (list "isa" "O3" "CVCube"))
					     "Test3" ;;Id
					     "RRedMt"
					     (list "isa" "O3" "RRed"))))
		 :base64))
	       :url "http://dubs:8000/ConceptLearner"
	       )))
    (assert (= (length (cdr (assoc :MATCHES res))) 1))
    ))

(defun test-reasoning-symbols ()
  (let ((res (cl-json:decode-json-from-string 
	      (net.xml-rpc:xml-rpc-call
	       (net.xml-rpc:encode-xml-rpc-call
		"create-reasoning-symbol"
		(net.xml-rpc:make-xml-rpc-encoding 
		 (cl-json::encode-json-alist-to-string
		  (pairlis '("symbol")
			   '("RRed")))
		 :base64))
	       :url "http://dubs:8000/ConceptLearner"
	       ))))
    (assert (equal (cdr (assoc :GPOOL res))
		   "RRedMt"))
    (setq res (cl-json::decode-json-from-string
	       (net.xml-rpc:xml-rpc-call
		(net.xml-rpc:encode-xml-rpc-call
		 "create-reasoning-predicate"
		 (net.xml-rpc:make-xml-rpc-encoding 
		  (cl-json::encode-json-alist-to-string
		   (pairlis '("predicate")
			    '("rOn")))
		  :base64))
	       :url "http://dubs:8000/ConceptLearner"
	       )))
    (assert (equal (cdr (assoc :GPOOL res))
		   "rOnMt"))
    ))

(defun clean-tests ()
  (fire:kb-forget (car(fire:retrieve-references 'd::RRed)))
  (fire:kb-forget (car(fire:retrieve-references 'd::rOn)))
  )
	
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
