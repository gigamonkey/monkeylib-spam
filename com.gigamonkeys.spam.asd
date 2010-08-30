;;
;; Copyright (c) 2005-2010, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.spam
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :components
  ((:file "packages")
   (:file "spam" :depends-on ("packages"))
   (:file "testing" :depends-on ("packages"))
   (:file "utilities" :depends-on ("packages")))
  :depends-on (:cl-ppcre 
               :com.gigamonkeys.pathnames 
               :com.gigamonkeys.utilities))

        
