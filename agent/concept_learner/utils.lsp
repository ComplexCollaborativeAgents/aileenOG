;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: utils.lsp
;;;;    System: 
;;;;    Author: Matthew Klenk
;;;;   Created: April 26, 2020 06:11:24
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Sunday, April 26, 2020 at 07:52:38 by klenk
;;;; ----------------------------------------------------------------------------

(in-package :aileen)

(defun extract-concept-memory-interactions (filename)
  (with-open-file (f filename :direction :input)
    (do ((result nil (if (api-call next) (cons (api-call next) result) result))
	 (next (read-line f nil 'eof) (read-line f nil 'eof)))
	((equal next 'eof) (reverse result)))))


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
		   (format nil "{\"symbol\": \"~A\",\"arity\": 2}" (subseq str (1+ (search " " str :from-end t))))))) 
	((starts-with-p str "Creating Reasoning Symbol")
	 str)
	((starts-with-p str "Creating Reasoning Predicate")
	 str)
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
