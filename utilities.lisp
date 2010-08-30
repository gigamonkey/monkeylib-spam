(in-package :com.gigamonkeys.spam)

(defun explain-classification (features db)
  (multiple-value-bind (classification score) (classify features db)
    (format t "Classification: ~a; score: ~,5f~%" classification score)
    (dolist (feature (sorted-interesting features db))
      (show-feature feature db))))

(defun show-all-features (db)
  (loop for feature being the hash-values of (features db) do (show-feature feature db)))
  
(defun show-feature (feature db)
  (if (untrained-p feature)
      (format t "~&~2t~s~30tuntrained~%" (id feature))
      (with-slots (id hams spams) feature
        (format t
                "~&~2t~s~30thams: ~5d; spams: ~5d;~,10tprob: ~,f~%"
                id hams spams (bayesian-spam-probability feature db)))))

(defun sorted-interesting (features db)
  (sort (remove-if #'untrained-p features) #'< :key #'(lambda (f) (bayesian-spam-probability f db))))