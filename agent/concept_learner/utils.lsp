;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: utils.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: April 26, 2020 06:11:24
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Thursday, October 15, 2020 at 17:42:16 by klenk
;;;; ----------------------------------------------------------------------------

;;;
;;; Two main functions
;;; 1) replay-concept-memory-interactions takes a log and just replays all of the interactions in order
;;; 2) replay-experiment identifies the evaluation set and then measures performance after each storage


(in-package :aileen)

(defparameter *test-port* 7090)

(defun replay-concept-memory-interactions (filename)
  (dolist (interaction (extract-concept-memory-interactions filename))
    (format t "~% interaction: ~A" interaction)
    (funcall (car interaction) (second interaction))))

	
; return positive and negative query tests
; assumes equal number of positive and negative tests
; would be better as a loop
(defun identify-evaluation-sets (interactions &optional (test-set-size 10))
  (assert (evenp test-set-size)) 
  (let* ((start (+ 2 (position #'store-helper interactions :key #'car)))
	 (end (- (position #'store-helper interactions :key #'car :start start) 1)))
    (cond ((= (-  end start) test-set-size)
	   (values
	    (subseq interactions start (+ start (/ test-set-size 2)))
	    (subseq interactions (+ start (/ test-set-size 2)) end)))
	  (t
	   (identify-evaluation-sets (subseq interactions end)  test-set-size)))))
	   

(defun identify-stores (interactions)
  (remove-if-not #'(lambda (int) (eql (car int) #'store-helper)) interactions))

(defun identify-new-symbols (interactions)
  (remove-if-not #'(lambda (int) (find (car int)
				       (list #'create-reasoning-predicate-helper
					     #'create-reasoning-action-helper
					     #'create-reasoning-symbol-helper)))
			   interactions))

(defun replay-experiment (ints &optional (no-eval? nil))
  (multiple-value-bind (poss negs)
      (unless no-eval? (identify-evaluation-sets ints))
    (replay-experiment-1 poss
			 negs
			 (identify-stores ints)
			 (identify-new-symbols ints)
			 no-eval?)))

(defun replay-experiment-1 (poss negs stores symbols &optional (no-eval? nil))
  (dolist (symbol symbols)
    (funcall (car symbol) (second symbol)))
  (let (ret)
    (dolist (store stores (reverse ret))
      (funcall (car store) (second store))
      (unless no-eval?
	(push (measure-performance poss negs) ret)))))

(defun measure-performance (poss negs)
  (let ((pos_res 0)
	(neg_res 0))
    (dolist (pos poss)
      (print pos)
      (when (query-matches pos) 
	(incf pos_res)))
    (dolist (neg negs)
      (unless (query-matches neg)
	(incf neg_res)))
    (cons pos_res neg_res)))

(defun query-matches (query)
  (let ((results (cl-json:decode-json-from-string
		  (funcall (car query) (second query)))))
    (cdr (assoc :MATCHES results))))


;"Creating Reasoning Action  r_move1" => but in the future this will be a json string
;"Storing {...}"
;"Querying {...}"
(defun api-call (str)
  (cond ((starts-with-p str "Projecting") (list #'project-helper (json-in-str str)))
	((starts-with-p str "Querying") (list #'query-helper (json-in-str str)))
	((starts-with-p str "Storing") (list #'store-helper (json-in-str str)))
	((starts-with-p str "Creating Reasoning Action")
	 (list #'create-reasoning-action-helper 
	       (if (json-in-str str)
		   (json-in-str str)
		   (format nil "{\"action\": \"~A\",\"arity\": 2}" (subseq str (1+ (search " " str :from-end t))))))) 
	((starts-with-p str "Creating Reasoning Symbol2")
	 (list #'create-reasoning-symbol-helper (json-in-str str)))
	((starts-with-p str "Creating Reasoning Predicate")
	 (list #'create-reasoning-predicate-helper (json-in-str str)))
	(t nil)))
	 
(defun starts-with-p (str1 str2)
  "Determine whether `str1` starts with `str2`"
  (let ((p (search str2 str1)))
        (and p (= 0 p))))

(defun json-in-str (str)
  (if (search "{" str)
      (subseq str (search "{" str) (1+ (search "}" str :from-end t)))
  ))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
