;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: server.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: November  9, 2019 16:19:17
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Monday, November 11, 2019 at 13:57:00 by klenk
;;;; ----------------------------------------------------------------------------

(load "analogystack/qrgsetup.lsp")
(require-module "fire" :fire) ;; assumes you already have a KB
(require-module "rbrowse" :rbrowse) ;; don't need this now

(require :aserve)

(require :asdf)

(load "cl-json/cl-json.asd")


(defpackage :aileen
  (:use :common-lisp :cl-user :excl :net.aserve :net.html.generator))

(asdf:load-system :cl-json)

(load "generalization.lsp")

(in-package :aileen)

;;;; API Assume all arguments in a single json dictionary
;;; 1. Add case to gpool
;;;    - It should have the reasoning symbol included (necessary for relations)
;;;    - It should have an identifier so later concepts can be added
;;; 2. Resolve a reasoning symbol in a case
;;;       - What is left of red cube
;;; 3. Given a case
;;; 4. how do I change the scene to make a given reasoning relation true

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
	


;; curl -d 'json={"facts":[["isa", "Obj3", "CVSphere"],["isa", "Obj3", "CVRed"]],"context":"AileenExp3","gpool":"RRed"}' --request POST http://localhost:8000/add-case-to-gpool
(defun add-case-to-gpool-helper (req ent)
  (setq *last-req* req)
  (let ((json (cl-json:decode-json-from-string
	       (request-query-value "json" req))))
    (print json)
    (let(
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
		 (format *html-stream* "{'generalizations' : ~A,'examples' : ~A}" num-gen num-exp)))
	      (t
	       (format t "~%Ill formed request ~A" req)
	       (format *html-stream* "{}"))))))))


(defun start-server (&key (port 8000))
  (make-reasoner)
  (start :port port)
  (publish :path "/run-example"
	   :content-type "text/plain"
	   :function
	   #'(lambda (req ent)
	       (with-http-response (req ent)
		 (with-http-body (req ent)
		   (make-reasoner)
		   (generalization-of-concepts-aileen)
		   ))))
  (publish :path "/add-case-to-gpool"
	   :content-type "text/plain"
	   :function #'add-case-to-gpool-helper)
  )


; These are the commands to run from the commandline
;(Start-server)  
;(loop (sleep 60))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
