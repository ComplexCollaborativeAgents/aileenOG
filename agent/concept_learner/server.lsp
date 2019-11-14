;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: rpcserver.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: November 13, 2019 16:14:37
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Wednesday, November 13, 2019 at 16:35:31 by klenk
;;;; ----------------------------------------------------------------------------

(load "analogystack/qrgsetup.lsp")
(require-module "fire" :fire) ;; assumes you already have a KB
(require-module "rbrowse" :rbrowse) ;; don't need this now

(require :asdf)

(load "cl-json/cl-json.asd")
(asdf:load-system :cl-json)

(require :aserve)
(require :xml-rpc)

(defpackage :aileen
  (:use :common-lisp :cl-user :excl))

(load "generalization.lsp")

(in-package :aileen)

(defun str->symbols (lst)
  (cond ((null lst) nil)
	((stringp lst)
	 (let ((*package* (find-package :data)))
	   (read-from-string lst)))
	((numberp lst) lst)
	((consp lst)
	 (cons (str->symbols (car lst))(str->symbols (cdr lst))))
	(t (error "str->symbols "))))

(defun create-reasoning-symbol-helper (str)
  (format t "~%Creating Reasoning Symbol2 ~A" str)
  (let* ((json (cl-json:decode-json-from-string str))
	 (symbol (str->symbols (cdr (assoc :SYMBOL json)))))
    (format t "~% json ~A symbol ~A" json symbol)
    (cond (symbol
	   (format t "~%Creating Reasoning Symbol ~A" symbol)
	   (multiple-value-bind (num gpool)
	       (create-reasoning-symbol symbol)
	     (cl-json:encode-json-alist-to-string (pairlis '("numSymbols" "gpool")
							   (list num (symbol-name gpool)))
						  )))
	  (t
	   (format t "~%Ill formed create-reasoning-symbol request ~A" str)
	   ""))))


(defparameter *str* nil)

(defun create-reasoning-predicate-helper (str)
  (let* ((json (cl-json:decode-json-from-string str))
	 (symbol (str->symbols (cdr (assoc :PREDICATE json)))))
    (cond (symbol
	   (format t "~%Creating Reasoning Predicate ~A" symbol)
	   (multiple-value-bind (num gpool)
	       (create-reasoning-predicate symbol)
	     (cl-json:encode-json-alist-to-string
	      (pairlis '("numPredicates" "gpool") (list num (symbol-name gpool))))))
	  (t
	   (format t "~%Ill formed create-reasoning-predicate request ~A" str)
	   ""))))

(defun add-case-to-gpool-helper (str)
  (setq *str* str)
  (let* ((json (cl-json:decode-json-from-string str))
	 (facts (str->symbols (cdr (assoc :FACTS json))))
	 (context (str->symbols (cdr (assoc :CONTEXT json))))
	 (gpool (str->symbols (cdr (assoc :GPOOL json)))))
    (cond ((and facts context gpool)
	   (multiple-value-bind (num-gen num-exp)
	       (add-case-to-gpool facts context gpool)
	     (format t "~%Gpool ~A has ~a generalizations and ~A examples" gpool num-gen num-exp)
	     (cl-json:encode-json-alist-to-string
	      (pairlis '("numGeneralizations" "numExamples") (list num-gen num-exp)))))
	  (t
	   (format t "~%Ill formed add-case-to-gpool request ~A" str)
	   ""))))


(defun match-case-against-gpool-helper (str)
  (format t "~%Checking context against gpool for pattern ~A" str)
  (setq *str* str)
  (pprint str)
  (let* ((json (cl-json:decode-json-from-string str))
	(facts (str->symbols (cdr (assoc :FACTS json))))
	(context (str->symbols (cdr (assoc :CONTEXT json))))
	(gpool (str->symbols (cdr (assoc :GPOOL json))))
	(pattern (str->symbols (cdr (assoc :PATTERN json)))))
    (format t "~%Checking context ~A against gpool ~A for pattern ~A" context gpool pattern)
    (cond ((and facts context gpool pattern)
	   (let ((matches (match-case-against-gpool facts context gpool pattern))) 
	     (format t "~%Found matches ~A" matches)
	     (cl-json:encode-json-alist-to-string
	      (pairlis '("matches" "pattern") (list matches pattern))
					)))
	      (t
	       (format t "~%Ill formed match-case-against-gpool-helper request ~A" str)
	       ""))))



(defun start-server (&key (port 8000))
  (make-reasoner)
  (let ((rcp (net.xml-rpc:make-xml-rpc-server
	      :start nil :enable t
	      :publish '(:path "/ConceptLearner")  )))
    (net.xml-rpc:export-xml-rpc-method
     rcp '("create-reasoning-symbol" create-reasoning-symbol-helper)
     :base64 :base64)
    (net.xml-rpc:export-xml-rpc-method
     rcp '("create-reasoning-predicate" create-reasoning-predicate-helper)
     :base64 :base64)
    (net.xml-rpc:export-xml-rpc-method
     rcp '("add-case-to-gpool" add-case-to-gpool-helper)
     :base64 :base64)
    (net.xml-rpc:export-xml-rpc-method
     rcp '("match-case-against-gpool" match-case-against-gpool-helper)
     :base64 :base64)
    				       
    (net.aserve:start :port port)
    ))
  
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
