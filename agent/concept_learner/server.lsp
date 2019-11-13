;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: server.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: November  9, 2019 16:19:17
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Wednesday, November 13, 2019 at 09:12:47 by klenk
;;;; ----------------------------------------------------------------------------

(load "analogystack/qrgsetup.lsp")
(require-module "fire" :fire) ;; assumes you already have a KB
(require-module "rbrowse" :rbrowse) ;; don't need this now

(require :aserve)

(require :asdf)

(load "cl-json/cl-json.asd")


(defpackage :aileen
  (:use :common-lisp :cl-user :excl :net.aserve :net.html.generator :net.aserve.client))

(asdf:load-system :cl-json)

(load "generalization.lsp")

(in-package :aileen)

;;;; API Assume all arguments in a single json dictionary
;;; 1. Add case to gpool
;;;    - It should have the reasoning symbol included (necessary for relations)
;;;    - It should have an identifier so later concepts can be added
;;; 2. Resolve a reasoning symbol in a case
;;;       - What is left of red cube
;;; 3. Given a case, does it match for the gpool
;;; 4. how do I change the scene to make a given reasoning relation true
;;; 5. Add reasoning symbol Collection or Relation
;;;    - Unsure how we should talk about events. Using microtheories/
;;;      submicrotheories, or explicit events?

(defparameter *last-req* nil)

(defun str->symbols (lst)
  (cond ((null lst) nil)
	((stringp lst)
	 (let ((*package* (find-package :data)))
	   (read-from-string lst)))
	((numberp lst) lst)
	((consp lst)
	 (cons (str->symbols (car lst))(str->symbols (cdr lst))))
	(t (error "str->symbols "))))
	

(defun create-reasoning-symbol-helper (req ent)
  (format t "~%Creating Reasoning Symbol" )
  (setq *last-req* req)
  (let* ((json (cl-json:decode-json-from-string (request-query-value "json" req)))
	 (symbol (str->symbols (cdr (assoc :SYMBOL json)))))
    (with-http-response (req ent)
      (with-http-body (req ent)
	(cond (symbol
	       (format t "~%Creating Reasoning Symbol ~A" symbol)
	       (multiple-value-bind (num gpool)
		   (create-reasoning-symbol symbol)
		 (cl-json:encode-json-alist (pairlis '("numSymbols" "gpool")
						     (list num (symbol-name gpool)))
					    *html-stream*)))
	      (t
	       (format t "~%Ill formed create-reasoning-symbol request ~A" req)
	       (format *html-stream* "{}")))))))

(defun create-reasoning-predicate-helper (req ent)
  (setq *last-req* req)
  (let* ((json (cl-json:decode-json-from-string (request-query-value "json" req)))
	(symbol (str->symbols (cdr (assoc :PREDICATE json)))))
    (with-http-response (req ent)
      (with-http-body (req ent)
	(cond (symbol
	       (format t "~%Creating Reasoning Predicate ~A" symbol)
	       (multiple-value-bind (num gpool)
		   (create-reasoning-predicate symbol)
		 (cl-json:encode-json-alist (pairlis '("numPredicates" "gpool") (list num (symbol-name gpool)))
					    *html-stream*)))
	      (t
	       (format t "~%Ill formed create-reasoning-predicate request ~A" req)
	       (format *html-stream* "{}")))))))

;; curl -d 'json={"facts":[["isa", "Obj3", "CVSphere"],["isa", "Obj3", "CVRed"]],"context":"AileenExp3","gpool":"RRed"}' --request POST http://localhost:8000/add-case-to-gpool
(defun add-case-to-gpool-helper (req ent)
  (setq *last-req* req)
  (let* ((json (cl-json:decode-json-from-string (request-query-value "json" req)))
	 (facts (str->symbols (cdr (assoc :FACTS json))))
	 (context (str->symbols (cdr (assoc :CONTEXT json))))
	 (gpool (str->symbols (cdr (assoc :GPOOL json)))))
    (format t "~%Adding context ~A to gpool ~A" context gpool)
    (with-http-response (req ent)
      (with-http-body (req ent)
	(cond ((and facts context gpool)
	       (multiple-value-bind (num-gen num-exp)
		   (add-case-to-gpool facts context gpool)
		 (format t "~%Gpool ~A has ~a generalizations and ~A examples" gpool num-gen num-exp)
		 (cl-json:encode-json-alist (pairlis '("numGeneralizations" "numExamples") (list num-gen num-exp))
				      *html-stream*)))
	      (t
	       (format t "~%Ill formed add-case-to-gpool request ~A" req)
	       (format *html-stream* "{}")))))))

(defun match-case-against-gpool-helper (req ent)
  (setq *last-req* req)
  (let* ((json (cl-json:decode-json-from-string (request-query-value "json" req)))
	(facts (str->symbols (cdr (assoc :FACTS json))))
	(context (str->symbols (cdr (assoc :CONTEXT json))))
	(gpool (str->symbols (cdr (assoc :GPOOL json))))
	(pattern (str->symbols (cdr (assoc :PATTERN json)))))
    (format t "~%Checking context ~A against gpool ~A for pattern ~A" context gpool pattern)
    (with-http-response (req ent)
      (with-http-body (req ent)
	(cond ((and facts context gpool pattern)
	       (let ((matches (match-case-against-gpool facts context gpool pattern))) 
		 (format t "~%Found matches ~A" matches)
		 (cl-json:encode-json-alist (pairlis '("matches" "pattern") (list matches pattern))
				      *html-stream*)))
	      (t
	       (format t "~%Ill formed match-case-against-gpool-helper request ~A" req)
	       (format *html-stream* "{}")))))))


(defun start-server (&key (port 8000))
  (make-reasoner)
  (start :port port)
  (publish :path "/create-new-reasoning-symbol"
	   :content-type "text/plain"
	   :function #'create-reasoning-symbol-helper)
  (publish :path "/create-new-reasoning-predicate"
	   :content-type "text/plain"
	   :function #'create-reasoning-predicate-helper)  
  (publish :path "/add-case-to-gpool"
	   :content-type "text/plain"
	   :function #'add-case-to-gpool-helper)
  (publish :path "/match-case-against-gpool"
	   :content-type "text/plan"
	   :function #'match-case-against-gpool-helper)
  
  
  )


; These are the commands to run from the commandline
;(Start-server)  
;(loop (sleep 60))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
