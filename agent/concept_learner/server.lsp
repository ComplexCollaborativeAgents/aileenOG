;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: server.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: November 13, 2019 16:14:37
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Tuesday, December 17, 2019 at 15:57:16 by klenk
;;;; ----------------------------------------------------------------------------

(in-package :cl-user)
(load "analogystack/qrgsetup.lsp")
(require-module "fire" :fire) ;; assumes you already have a KB
(require-module "rbrowse" :rbrowse) ;; don't need this now

(require :asdf)

(load "cl-json/cl-json.asd")
(asdf:load-system :cl-json)

(require :aserve)
(require :xml-rpc)
(require :autozoom)

(defpackage :aileen
  (:use :common-lisp :cl-user :excl))

(load "generalization.lsp")

(in-package :aileen)

(defun print-backtrace (condition)
  (format t "error condition: ~A~%"  condition)
  (top-level.debug::zoom cl-user::*standard-output* :all nil :relative t :count 30)
  )

(defun str->symbols (lst)
  (cond ((null lst) nil)
	((stringp lst)
	 (let ((*package* (find-package :data)))
	   (read-from-string lst)))
	((numberp lst) lst)
	((consp lst)
	 (cons (str->symbols (car lst))(str->symbols (cdr lst))))
	(t (error "str->symbols "))))

(defun symbols->strs (lst)
  (cond ((null lst) nil)
	((symbolp lst)
	 (symbol-name lst))
	((numberp lst) lst)
	((consp lst)
	 (cons (symbols->strs (car lst))(symbols->strs (cdr lst))))
	(t (error "symbols->strs "))))


(defun create-reasoning-symbol-helper (str)
  (handler-bind ((error #'print-backtrace))
  (format t "~%Creating Reasoning Symbol2 ~A" str)
  (let* ((json (cl-json:decode-json-from-string str))
	 (symbol (str->symbols (cdr (assoc :SYMBOL json)))))
    (cond (symbol
	   (format t "Creating Reasoning Symbol ~A~%" symbol)
	   (multiple-value-bind (num gpool)
	       (create-reasoning-symbol symbol)
	     (cl-json:encode-json-alist-to-string (pairlis '("numSymbols" "gpool")
							   (list num (symbol-name gpool)))
						  )))
	  (t
	   (format t "Ill formed create-reasoning-symbol request ~A~%" str)
	   "")))))


(defparameter *str* nil)

(defun create-reasoning-predicate-helper (str)
  (handler-bind ((error #'print-backtrace))
  (let* ((json (cl-json:decode-json-from-string str))
	 (symbol (str->symbols (cdr (assoc :PREDICATE json))))
	 (arity (cdr (assoc :arity json))))
    (cond (symbol
	   (format t "Creating Reasoning Predicate ~A~%" symbol)
	   (multiple-value-bind (num gpool)
	       (create-reasoning-predicate symbol 2)
	     (cl-json:encode-json-alist-to-string
	      (pairlis '("numPredicates" "gpool") (list num (symbol-name gpool))))))
	  (t
	   (format t "Ill formed create-reasoning-predicate request ~A~%" str)
	   "")))))

(defun add-case-to-gpool-helper (str)
  (handler-bind ((error #'print-backtrace))
  (setq *str* str)
  (let* ((json (cl-json:decode-json-from-string str))
	 (facts (str->symbols (cdr (assoc :FACTS json))))
	 (context (str->symbols (cdr (assoc :CONTEXT json))))
	 (gpool (str->symbols (cdr (assoc :GPOOL json)))))
    (cond ((and facts context gpool)
 	   (multiple-value-bind (num-gen num-exp)
	       (add-case-to-gpool facts context gpool)
	     (format t "Gpool ~A has ~a generalizations and ~A examples~%" gpool num-gen num-exp)
	     (cl-json:encode-json-alist-to-string
	      (pairlis '("numGeneralizations" "numExamples") (list num-gen num-exp)))))
	  (t
	   (format t "Ill formed add-case-to-gpool request ~A~%" str)
    "")))))

(defun store-helper (str)
  (handler-bind ((error #'print-backtrace))
  (setq *str* str)
  (format t "Storing ~A~%" str)
  (let* ((json (cl-json:decode-json-from-string str))
         (facts (str->symbols (cdr (assoc :FACTS json))))
         (context (str->symbols (cdr (assoc :CONTEXT json))))
         (concept (str->symbols (cdr (assoc :CONCEPT json)))))
    (cond ((and facts context concept)
	   (multiple-value-bind (num-gen num-exp)
	       (add-case-to-gpool facts context (get-concept-gpool concept))
	     (format t "Gpool for ~A has ~a generalizations and ~A examples~%" concept num-gen num-exp)
	     (cl-json:encode-json-alist-to-string
	      (pairlis '("numGeneralizations" "numExamples") (list num-gen num-exp)))))
          (t
           (format t "Ill formed store request ~A~%" str)
           (cl-json:encode-json-alist-to-string
            (pairlis '("numGeneralizations" "numExamples") (list 0 0)))))
    )))

(defun remove-helper (str)
  (handler-bind ((error #'print-backtrace))
    (setq *str* str)
  (format t "Removing ~A~%" str)
  (let* ((json (cl-json:decode-json-from-string str))
         (context (str->symbols (cdr (assoc :CONTEXT json))))
         (concept (str->symbols (cdr (assoc :CONCEPT json)))))
    (cond ((and concept (not context))
           ;; Remove the given concept and its concept pool.
           ;; TODO: Does this delete all of the instances from the concept pool?
           (fire:kb-forget (car(fire:retrieve-references concept)))
           (fire:kb-forget (car(fire:retrieve-references (get-concept-gpool concept))))
           (format t "Done removing ~A and ~A~%" concept (get-concept-gpool concept))
           (cl-json:encode-json-alist-to-string
            (pairlis '("success") (list t))))
          ((and concept context)
           ;; Forget the given instance.
           (fire:kb-forget (car(fire:retrieve-references context)))
           (cl-json:encode-json-alist-to-string
            (pairlis '("success") (list t))))
          (t
	   (format t "Ill formed remove request ~A~%" str)
           (cl-json:encode-json-alist-to-string
            (pairlis '("success") (list nil))))))))

(defun query-helper (str)
  (handler-bind ((error #'print-backtrace))
  (setq *str* str)
  (format t "Querying ~A~%" str)
  (let* ((json (cl-json:decode-json-from-string str))
         (facts (str->symbols (cdr (assoc :FACTS json)))) ;;; all facts in scene
         (context 'data::query-facts)
	 (pattern (str->symbols (cdr (assoc :PATTERN json))))) ;; Statement with variables
    (cond ((and facts pattern)
           ;; Clear previous facts from context.
           (remove-facts-from-case context)
           ;; Store facts in context and match query.
           (let ((matches (filter-scene-by-expression facts context nil nil pattern)))
             (format t "Found matches ~A~%" matches)
             (cl-json:encode-json-alist-to-string
              (pairlis '("matches" "pattern")
                       (list (symbols->strs matches)
			     (symbols->strs pattern))))))
          (t
           (format t "Ill formed filter scene request ~A~%" str)
           (cl-json:encode-json-alist-to-string
              (pairlis '("matches" "pattern") '(nil nil))))))))

(defun match-case-against-gpool-helper (str)
  (assert nil) ;;this is not used anymore
  (format t "Checking context against gpool for pattern ~A~%" str)
  (setq *str* str)
  (pprint str)
  (let* ((json (cl-json:decode-json-from-string str))
	(facts (str->symbols (cdr (assoc :FACTS json))))
	(context (str->symbols (cdr (assoc :CONTEXT json))))
	(gpool (str->symbols (cdr (assoc :GPOOL json))))
	(pattern (str->symbols (cdr (assoc :PATTERN json)))))
    (format t "Checking context ~A against gpool ~A for pattern ~A~%" context gpool pattern)
    (cond ((and facts context gpool pattern)
	   (let ((matches (match-case-against-gpool facts context gpool pattern))) 
	     (format t "Found matches ~A~%" matches)
	     (cl-json:encode-json-alist-to-string
	      (pairlis '("matches" "pattern") (list matches pattern))
					)))
	      (t
	       (format t "Ill formed match-case-against-gpool-helper request ~A~%" str)
	       ""))))

(defun filter-scene-by-expression-helper (str)
  (handler-bind ((error #'print-backtrace))
    (setq *str* str)
    (format t "Checking context against pattern ~A~%" str)
    (let* ((json (cl-json:decode-json-from-string str))
	   (facts (str->symbols (cdr (assoc :FACTS json)))) ;;; all facts in scene
	   (context (str->symbols (cdr (assoc :CONTEXT json)))) ;;; id for current scene
	   (gpool (str->symbols (cdr (assoc :GPOOL json)))) ;;;
	   (prev-matches (str->symbols (cdr (assoc :PREVQUERIES json)))) ;;; Previous Statements that Constraint Current Query
	   (pattern (str->symbols (cdr (assoc :PATTERN json))))) ;; Statement with variables
      (format t "Checking facts ~A context ~A against gpool ~A for pattern ~A~%" facts context gpool pattern)
      (cond ((and facts context pattern)
	     (let ((matches (filter-scene-by-expression facts context gpool prev-matches pattern)))
	       (format t "Found matches ~A~%" matches)
	       (cl-json:encode-json-alist-to-string
		(pairlis '("matches" "pattern")
			 (list (loop for item in matches collect
				    (if (symbolp item) (symbol-name item) item))
			       (loop for item in pattern collect
				    (if (symbolp item) (symbol-name item) item)))))
	       ))
	    (t
	     (format t "Ill formed filter scene request ~A~%" str)
	     "")))))

  
(defun start-server (&key (port 8000) (kbdir "nextkb"))
  (format t "~% starting server port ~A" port)
  (make-reasoner :kbdir kbdir)
  (let ((rcp (net.xml-rpc:make-xml-rpc-server
	      :start nil :enable t
	      :publish '(:path "/ConceptLearner")  )))
    ;; Add an init_kb helper that reinitializes the kb.
    (net.xml-rpc:export-xml-rpc-method
     rcp '("create_reasoning_symbol" create-reasoning-symbol-helper)
     :base64 :base64)
    (net.xml-rpc:export-xml-rpc-method
     rcp '("create_reasoning_predicate" create-reasoning-predicate-helper)
     :base64 :base64)
    (net.xml-rpc:export-xml-rpc-method
     rcp '("add_case_to_gpool" add-case-to-gpool-helper)
     :base64 :base64)
    (net.xml-rpc:export-xml-rpc-method
     rcp '("store" store-helper)
     :base64 :base64)
    (net.xml-rpc:export-xml-rpc-method
     rcp '("remove" remove-helper)
     :base64 :base64)
    (net.xml-rpc:export-xml-rpc-method
     rcp '("query" query-helper)
     :base64 :base64)
;; Klenk: We want to match against the whole scene.
    (net.xml-rpc:export-xml-rpc-method
     rcp '("match_case_against_gpool" match-case-against-gpool-helper)
     :base64 :base64)
    (net.xml-rpc:export-xml-rpc-method  
     rcp '("filter_scene_by_expression" filter-scene-by-expression-helper)
     :base64 :base64)
    (net.aserve:start :port port)
    ))
  
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
