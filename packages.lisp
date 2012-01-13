(in-package :cl-user)

(defpackage :com.gigamonkeys.spam
  (:nicknames :spam)
  (:use :common-lisp
        :com.gigamonkeys.pathnames
        :com.gigamonkeys.utilities)
  (:export
   :make-feature-database
   :clear-database
   :intern-feature
   :train
   :untrain
   :classify
   :all-features
   :feature-spamminess

   :id
   :hams
   :spams))
