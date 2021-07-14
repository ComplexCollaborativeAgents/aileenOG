;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ----------------------------------------------------------------------------
;;;; File name: testgeneralization.lsp
;;;;    System: 
;;;;    Author: Will Hancock
;;;;   Created: July 13, 2021 13:04:34
;;;;   Purpose: 
;;;; ----------------------------------------------------------------------------
;;;;  Modified: Tuesday, July 13, 2021 at 11:27:28 by Hancock
;;;; ----------------------------------------------------------------------------

(in-package :cl-user)

; (require :asdf)


; (load "server.lsp") ;;;Should probably move this to asdf at some point

(in-package :aileen)


(defparameter *threshold* .2)

; mt ids for concept
(defparameter *r-near* '(11 12 13))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Testing code



(defun qlearning-main ()

  (load-test-flat-files)
  (create-test-generalizations 'd::RNear)

  )

(defun load-test-flat-files ()
  (fire:kr-file->kb (qrg:make-qrg-file-name
			    (qrg:make-qrg-path ".." "data")
			    "aileen-mt.krf")
	       :error-on-bad-exps? t :kb fire::*kb*)
  (cl-user::load-flatfiles-in-dir (qrg:make-qrg-path ".." "data" "continuous-q-learning")))

(defun create-test-generalizations (concept-symbol)
  (create-reasoning-symbol concept-symbol)
  (multiple-value-bind (gens examples) ;;do I need to add reasoning symbols?
      (create-gpools *r-near* (get-concept-gpool concept-symbol))
   
    )
  )

(defun make-random-mt ()
  (intern (gensym "AlieenMT") :d))
  

(defun microtheory-by-id (id)
  (intern (format nil "AileenExp~D" id) :d))


(defun create-gpools (ids gpool)
  (cl-user::nuke-gpool gpool)
  (cl-user::setup-gpool gpool :threshold *threshold* :strategy :gel)  
  (dolist (id ids)
    (fire:tell-it `(d::sageSelectAndGeneralize
		    ,(microtheory-by-id id)
		    ,gpool)))
  (values (length (fire:ask-it `(d::gpoolGeneralization ,gpool ?num)))
	  (length (fire:ask-it `(d::gpoolExample ,gpool ?num)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
