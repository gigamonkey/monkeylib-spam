(in-package :cl-user)

(defpackage :com.gigamonkeys.spam
  (:use :common-lisp :com.gigamonkeys.pathnames)
  (:export
   :add-directory-to-corpus
   :add-file-to-corpus
   :classify
   :clear-database
   :train
   :ham
   :spam
   :*max-ham-score*
   :*min-spam-score*))





