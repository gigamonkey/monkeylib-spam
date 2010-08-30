(in-package :com.gigamonkeys.spam)

(defparameter *default-ham-threshold* .4d0)
(defparameter *default-spam-threshold* .6d0)

(defclass ham-spam-counter ()
  ((hams :initarg :hams :accessor hams :initform 0)
   (spams :initarg :spams :accessor spams :initform 0)))

(defclass feature (ham-spam-counter)
  ((id :initarg :id :initform (error "Must supply :id") :accessor id)))

(defclass feature-database (ham-spam-counter)
  ((features :initform (make-hash-table :test 'equal) :accessor features)))

(defmethod print-object ((object feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (id hams spams) object
      (format stream "~s :hams ~d :spams ~d" id hams spams))))

(defmethod print-object ((object feature-database) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (features hams spams) object
      (format stream "~:d features in ~:d hams and ~:d spams" 
              (hash-table-count features) hams spams))))

;;; Public API

(defun make-feature-database ()
  (make-instance 'feature-database))

(defun clear-database (db)
  (with-slots (features hams spams) db
    (setf 
     features (make-hash-table :test 'equal)
     hams 0
     spams 0))
  db)

(defun train (features type db)
  "Update the database to reflect that each of `features' appeared in a message of type `type'."
  (flet ((increment-count (thing)
           (ecase type
             (:ham (incf (hams thing)))
             (:spam (incf (spams thing))))))
    (dolist (feature features)
      (increment-count feature))
    (increment-count db)))

(defun untrain (features type db)
  "Remove the effect of having added a set of features to the database under a given type."
  (flet ((decrement-count (thing)
           (ecase type
             (:ham (decf (hams thing)))
             (:spam (decf (spams thing))))))
    (dolist (feature features)
      (decrement-count feature))
    (decrement-count db)))

(defun classify (features db &key
                 (ham-threshold *default-ham-threshold*) 
                 (spam-threshold *default-spam-threshold*))
  "Classify a set of features as SPAM, HAM, or UNSURE, returning the
classification and the spamminess score."
  (assert (< ham-threshold spam-threshold))
  (let ((score (score features db)))
    (values
     (cond
       ((<= score ham-threshold) :ham)
       ((>= score spam-threshold) :spam)
       (t ':unsure))
     score)))

(defun intern-feature (id db)
  "Intern a feature by id, which must be distinguishable under EQUAL
from other features' ids."
  (with-slots (features) db
    (or (gethash id features)
        (setf (gethash id features) (make-instance 'feature :id id)))))

;;; Internals

(defun spam-probability (feature db)
  "Basic probability that a feature with the given relative
frequencies will appear in a spam assuming spams and hams are
otherwise equally probable. One of the two frequencies must be
non-zero."
  (with-slots (spams hams) feature
    (let ((spam-frequency (/ spams (max 1 (spams db))))
          (ham-frequency (/ hams (max 1 (hams db)))))
      (/ spam-frequency (+ spam-frequency ham-frequency)))))

(defun bayesian-spam-probability (feature db &optional
                                  (assumed-probability 1/2)
                                  (weight 1))
  "Bayesian adjustment of a given probability given the number of
data points that went into it, an assumed probability, and a
weight we give that assumed probability."
  (let ((basic-probability (spam-probability feature db))
        (data-points (+ (spams feature) (hams feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))

(defun score (features db)
  "Combined 'spamminess' score of the set of features."
  (let ((spam-probs ())
	(ham-probs ())
	(number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature db) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun untrained-p (feature)
  (with-slots (spams hams) feature
    (and (zerop spams) (zerop hams))))

(defun fisher (probs number-of-probs)
  "The Fisher computation described by Robinson."
  (inverse-chi-square 
   (* -2 (reduce #'+ probs :key #'log))
   (* 2 number-of-probs)))

(defun inverse-chi-square (value degrees-of-freedom)
  "Probability that chi-square >= value with given degrees-of-freedom.
Based on Gary Robinson's Python implementation."
  (assert (evenp degrees-of-freedom))
  ;; Due to rounding errors in the multiplication and exponentiation
  ;; the sum computed in the loop may end up a shade above 1.0 which
  ;; we can't have since it's supposed to represent a probability.
  (min 
   (loop with m = (/ value 2)
      for i below (/ degrees-of-freedom 2)
      for prob = (safe-exp (- m)) then (* prob (/ m i))
      summing prob)
   1.0d0))

(defun safe-exp (x)
  (handler-case (exp x)
    (floating-point-underflow () least-positive-double-float)))
