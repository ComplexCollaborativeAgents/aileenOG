;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: server.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: November 13, 2019 16:14:37
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Wednesday, May 20, 2020 at 19:56:33 by klenk
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


;; The current KB is always the next kb
(defun checkpoint-helper (str)
  (handler-bind ((error #'print-backtrace))
    (format t "~%Checkpoint-kb ~A" str)
    (let* ((json (cl-json:decode-json-from-string str))
	   (dir (cdr (assoc :DIR json))))
      (assert (fire:open-kb? fire:*kb*))
      (fire:copy-kb-files fire:*kb* (qrg:make-qrg-path ".." "data" dir))
      (cl-json:encode-json-alist-to-string
       (pairlis
	'("dir")
	(list dir))
       ))))


(defun restore-helper (str)
  (handler-bind ((error #'print-backtrace))
    (format t "~%Restoring-kb ~A" str)
    (let* ((json (cl-json:decode-json-from-string str))
	   (dir (cdr (assoc :DIR json))))
      (assert (fire:open-kb? fire:*kb*))
      (fire:copy-kb-files (qrg:make-qrg-path ".." "data" dir)
			  (qrg::make-qrg-path "planb" "kbs""nextkb"))
      (assert (string= (car (last (pathname-directory (fire:kb-path fire:*kb*)))) "nextkb"))
      (cl-json:encode-json-alist-to-string
       (pairlis
	'("dir")
	(list dir))
       ))))

(defparameter *str* nil)

(defun create-reasoning-symbol-helper (str)
  (setq *str* str)
  (handler-bind ((error #'print-backtrace))
    (format t "~%Creating Reasoning Symbol2 ~A" str)
  (let* ((json (cl-json:decode-json-from-string str))
	 (symbol (str->symbols (cdr (assoc :SYMBOL json)))))
    (cond (symbol
	   (format t "Creating Reasoning Symbol ~A~%" str)
	   (multiple-value-bind (num gpool)
	       (create-reasoning-symbol symbol)
	     (cl-json:encode-json-alist-to-string (pairlis '("numSymbols" "gpool")
							   (list num (symbol-name gpool)))
						  )))
	  (t
	   (format t "Ill formed create-reasoning-symbol request ~A~%" str)
	   "")))))



(defun create-reasoning-predicate-helper (str)
  (setq *str* str)
  (handler-bind ((error #'print-backtrace))
  (let* ((json (cl-json:decode-json-from-string str))
	 (symbol (str->symbols (cdr (assoc :PREDICATE json))))
	 )
    (cond (symbol
	   (format t "Creating Reasoning Predicate ~A~%" str)
	   (multiple-value-bind (num gpool)
	       (create-reasoning-predicate symbol 2)
	     (cl-json:encode-json-alist-to-string
	      (pairlis '("numPredicates" "gpool") (list num (symbol-name gpool))))))
	  (t
	   (format t "Ill formed create-reasoning-predicate request ~A~%" str)
	   "")))))

;;; assumes action has two arguments
;;; Long term a davidsonian representation makes more sense.
(defun create-reasoning-action-helper (str)
  (handler-bind ((error #'print-backtrace))
    (setq *str* str)
    (let* ((json (cl-json:decode-json-from-string str))
	   (symbol (str->symbols (cdr (assoc :ACTION json))))
	   (arity (cdr (assoc :ARITY json)))
	   )
    (cond ((and symbol arity)
	   (format t "Creating Reasoning Action  ~A~%" str)
	   (multiple-value-bind (num gpool)
	       (create-reasoning-action symbol arity) 
	     (cl-json:encode-json-alist-to-string
	      (pairlis '("numActions" "gpool") (list num (symbol-name gpool))))))
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
	       (add-case-to-gpool facts context concept)
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

(defun project-helper (str)
  (handler-bind ((error #'print-backtrace))
  (setq *str* str)
  (format t "Projecting ~A~%" str)
  (let* ((json (cl-json:decode-json-from-string str))
         (facts (str->symbols (cdr (assoc :FACTS json)))) ;;; all facts in scene
         (context 'data::project-facts)
	 (action (str->symbols (cdr (assoc :ACTION json))))) ;; Statement with variables
    (cond ((and facts action)
           ;; Clear previous facts from context.
           (remove-facts-from-case context)
           ;; Store facts in context and match query.
           (let ((cis (project-state-for-action facts context  action)))
             (format t "~%Found candidate inferences  ~A~%" cis)
             (cl-json:encode-json-alist-to-string
              (pairlis '("cis")
                       (list (symbols->strs cis)
			     )))))
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
	      :start `(:port ,port) :enable t
	      :publish '(:path "/ConceptLearner")  )))
    ;; Add an init_kb helper that reinitializes the kb.
    (net.xml-rpc:export-xml-rpc-method
     rcp '("create_reasoning_symbol" create-reasoning-symbol-helper)
     :base64 :base64)
    (net.xml-rpc:export-xml-rpc-method
     rcp '("create_reasoning_predicate" create-reasoning-predicate-helper)
     :base64 :base64)
    (net.xml-rpc:export-xml-rpc-method
     rcp '("create_reasoning_action" create-reasoning-action-helper)
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
    (net.xml-rpc:export-xml-rpc-method
     rcp '("project" project-helper)
     :base64 :base64)
    (net.xml-rpc:export-xml-rpc-method
     rcp '("checkpoint_kb" checkpoint-helper)
     :base64 :base64)
    (net.xml-rpc:export-xml-rpc-method
     rcp '("restore_kb" restore-helper)
     :base64 :base64)
;; Klenk: We want to match against the whole scene.
    (net.xml-rpc:export-xml-rpc-method
     rcp '("match_case_against_gpool" match-case-against-gpool-helper)
     :base64 :base64)
    (net.xml-rpc:export-xml-rpc-method  
     rcp '("filter_scene_by_expression" filter-scene-by-expression-helper)
     :base64 :base64)
					;    (net.aserve:start :port port)
    rcp
    ))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LXML patch

(in-package :net.xml-rpc)

(defun xml-rpc-server-implementation (request entity)
  ;; parse an XML rpc call and pass it to the exported function
  (when (xml-rpc-server-enabled *xml-rpc-server*)
    (let* ((body (get-request-body request
				   ;; If we do not specify an external format, Aserve
				   ;; uses :octets but here we want to do any requested
				   ;; character conversions   [bug19830]
				   :external-format   
				   net.aserve:*default-aserve-external-format*
				   ))
	   code string rval params pvals ptypes)
      (multiple-value-bind (name e)
	  (ignore-errors
	   (multiple-value-list
	    (let* ((xml (parse-to-lxml body :content-only t :normalize t))
		   name v alist w)
	      (multiple-value-setq (v alist)
		(decode-xml-parse
		 xml '(("methodCall"
			("methodName" :name)
			.
			(:or (("params" (:* :params ("param" :param))))
			     nil)))
		 nil :ignore-whitespace))
	      (or (and v
		       (setf name (assoc :name alist))
		       (setf name (cdr name)))
		  (error "missing methodName"))
	      ;; <params> may be omitted if empty
	      (setf params (assoc :params alist))
	      (dolist (p (cdr params))
		(or (and p
			 (setf w (assoc :param p))
			 (multiple-value-bind (val type)
			     (decode-xml-rpc-value(cdr w))
			   (when type
			     (push val pvals)
			     (push type ptypes)
			     t)))
		    (error "param ~S" p)))
	      (setf pvals (reverse pvals))
	      (setf ptypes (reverse ptypes))
	      name)))
	(if* e
	   then 
		(typecase e
		  (xml-rpc-fault
		   ;; The exported method body signalled an error of the form
		   ;;  (error 'xml-rpc-fault :fault-code c :fault-string s)
		   ;;  [bug19752]
		   (setf code (xml-rpc-fault-code e)
			 string (xml-rpc-fault-string e)))
		  (otherwise
		   (setf code 2
			 string (format nil "Error in XML-RPC call: ~A" e))))
;;;;TODO: should this be here???  Seems to clobber the values just set above
		(setf code 1
		      string
		      (format nil "Ill-formed XML-RPC call: ~A" e))
	   else 
		(setf name (car name))
		(multiple-value-bind (v e)
		    (ignore-errors
		     (multiple-value-list
		      (let* ((sig ptypes)
			     (entry   (xml-rpc-server-method *xml-rpc-server* name sig))
			     (function (and entry 
					    ;; is this mp safe???
					    (xml-rpc-export-function entry))))
			(or function (error "Unknown XML-RPC method ~A(~{ ~A~})"
					    name sig))
			(encode-xml-rpc-value (apply function pvals)
					      (xml-rpc-export-result entry))
			)))
		  (if* e
		     then 
			  (setf code (xml-rpc-fault-code e)
				string (xml-rpc-fault-string e))
		     else 
			  ;; emit result
			  (setf rval (first v))
			  )))
	(with-http-response 
	    (request entity)
	  ;; must send content-length [spr28777] [bug14271]
	  (if* (equal (request-reply-strategy request) '(:use-socket-stream))
	     then (setf (request-reply-strategy request)
		    '(:string-output-stream :post-headers))) 
	  (with-http-body 
	      (request entity 
	       :headers `((:server . ,(xml-rpc-server-name *xml-rpc-server*))))
	    (html
	     (:princ "<?xml version=\"1.0\"?>")
	     (:princ "<methodResponse>")

	     (if* code
		then
		     ;; emit fault
		     (html
		      (:princ "<fault>"
			      (encode-xml-rpc-value nil :struct
						    (list "faultCode" code)
						    (list "faultString" string))
			      "</fault>"))
		else
		     ;; emit value
		     (html
		      (:princ "<params><param>"
			      rval
			      "</param></params>"))
		     )

	     (:princ "</methodResponse>")
	     )))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
