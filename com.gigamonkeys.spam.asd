;;
;; Copyright (c) 2005-2011, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.spam
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :description "Spam filter based on code from Practical Common Lisp."
  :components
  ((:file "packages")
   (:file "spam" :depends-on ("packages"))
   (:file "testing" :depends-on ("packages"))
   (:file "utilities" :depends-on ("packages")))
  :depends-on (:cl-ppcre
               :com.gigamonkeys.pathnames
               :com.gigamonkeys.utilities))
