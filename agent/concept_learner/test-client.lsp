;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: test-client.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: November 11, 2019 16:00:46
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Wednesday, November 13, 2019 at 09:15:18 by klenk
;;;; ----------------------------------------------------------------------------

(load "server.lsp")

(in-package :aileen)

(defun test-concept-learner-server ()
  (start-server :port 8000)
  (test-reasoning-symbols)
  (test-generalization)
  )

(defun test-generalization ()
  ;; Add two cases for RRed to the RRedMT gpool
  ;; generalize them
  ;; Match a new scene against it
  (let (res)
    (setq res (cl-json::decode-json-from-string 
	       (do-http-request "http://localhost:8000/add-case-to-gpool"
		 :method :post :content-type "text/plain"
		 :query
		 (list (cons "json"
			     (cl-json::encode-json-alist-to-string
			      (pairlis '("facts" "context" "gpool")
				       (list (list (list "isa" "O1" "RRed")
						   (list "isa" "O1" "CVRed")
						   (list "isa" "O1" "CVCylinder"))
					     "Test1" ;;Id
					     "RRedMt"))))))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 1))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 0))

    (setq res (cl-json::decode-json-from-string 
	       (do-http-request "http://localhost:8000/add-case-to-gpool"
		 :method :post :content-type "text/plain"
		 :query
		 (list (cons "json"
			     (cl-json::encode-json-alist-to-string
			      (pairlis '("facts" "context" "gpool")
				       (list (list (list "isa" "O2" "RRed")
						   (list "isa" "O2" "CVRed")
						   (list "isa" "O2" "CVSphere"))
					     "Test2" ;;Id
					     "RRedMt"))))))))
    (assert (= (cdr (assoc :NUM-EXAMPLES res)) 0))
    (assert (= (cdr (assoc :NUM-GENERALIZATIONS res)) 1))

    ;;;PATTERN
    (setq res (cl-json::decode-json-from-string 
	       (do-http-request "http://localhost:8000/match-case-against-gpool"
		 :method :post :content-type "text/plain"
		 :query
		 (list (cons "json"
			     (cl-json::encode-json-alist-to-string
			      (pairlis '("facts" "context" "gpool" "pattern")
				       (list (list (list "isa" "O3" "CVRed")
						   (list "isa" "O3" "CVCube"))
					     "Test3" ;;Id
					     "RRedMt"
					     (list "isa" "O3" "RRed")))))))))
    (assert (= (length (cdr (assoc :MATCHES res))) 1))
    ))

(defun test-reasoning-symbols()
  (let ((res (cl-json::decode-json-from-string 
	      (do-http-request "http://localhost:8000/create-new-reasoning-symbol"
		:method :post :content-type "text/plain"
		:query (list (cons "json" "{\"symbol\":\"RRed\"}"))))))
    (assert (equal (cdr (assoc :GPOOL res))
		   "RRedMt"))
    (let ((res (cl-json::decode-json-from-string
		(do-http-request "http://localhost:8000/create-new-reasoning-predicate"
		  :method :post :content-type "text/plain"
		  :query (list (cons "json" "{\"predicate\":\"rOn\"}"))))))
      (assert (equal (cdr (assoc :GPOOL res))
		     "rOnMt"))
      )))

;; Everything else is in wm
(defun clean-tests ()
  (fire:kb-forget (car(fire:retrieve-references 'd::RRed)))
  (fire:kb-forget (car(fire:retrieve-references 'd::rOn)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
