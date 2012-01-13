(in-package :com.gigamonkeys.spam)

(defparameter *max-chars* (* 10 1024))
(defparameter *corpus* (make-array 1000 :adjustable t :fill-pointer 0))

(defun extract-words (text)
  "Simple function to extract words from a text."
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))

(defun extract-features (text db)
  "Map the words in a text to features using the word itself as the feature id"
  (mapcar (lambda (w) (intern-feature w db)) (extract-words text)))

(defun start-of-file (file max-chars)
  (with-open-file (in file)
    (let* ((length (min (file-length in) max-chars))
           (text (make-string length))
           (read (read-sequence text in)))
      (if (< read length)
        (subseq text 0 read)
        text))))

(defun add-file-to-corpus (filename type corpus)
  (vector-push-extend (list filename type) corpus))

(defun add-directory-to-corpus (dir type corpus)
  (dolist (filename (list-directory dir))
    (add-file-to-corpus filename type corpus)))

(defun test-classifier (corpus testing-fraction)
  (let* ((db (make-instance 'feature-database))
         (shuffled (shuffle-vector corpus))
         (size (length corpus))
         (train-on (floor (* size (- 1 testing-fraction)))))
    (train-from-corpus shuffled db :start 0 :end train-on)
    (test-from-corpus shuffled db :start train-on)))

(defun train-from-corpus (corpus db &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) do
        (destructuring-bind (file type) (aref corpus idx)
          (train (extract-features (start-of-file file *max-chars*) db) type db))))

(defun test-from-corpus (corpus db &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) collect
        (destructuring-bind (file type) (aref corpus idx)
          (multiple-value-bind (classification score)
              (classify (extract-features (start-of-file file *max-chars*) db))
            (list
             :file file
             :type type
             :classification classification
             :score score)))))

(defun result-type (result)
  (destructuring-bind (&key type classification &allow-other-keys) result
    (ecase type
      (:ham
       (ecase classification
         (:ham 'correct)
         (:spam 'false-positive)
         (:unsure 'missed-ham)))
      (:spam
       (ecase classification
         (:ham 'false-negative)
         (:spam 'correct)
         (:unsure 'missed-spam))))))

(defun false-positive-p (result)
  (eql (result-type result) 'false-positive))

(defun false-negative-p (result)
  (eql (result-type result) 'false-negative))

(defun missed-ham-p (result)
  (eql (result-type result) 'missed-ham))

(defun missed-spam-p (result)
  (eql (result-type result) 'missed-spam))

(defun correct-p (result)
  (eql (result-type result) 'correct))

(defun analyze-results (results)
  (let* ((keys '(total correct false-positive
                 false-negative missed-ham missed-spam))
         (counts (loop for x in keys collect (cons x 0))))
    (dolist (item results)
      (incf (cdr (assoc 'total counts)))
      (incf (cdr (assoc (result-type item) counts))))
    (loop with total = (cdr (assoc 'total counts))
          for (label . count) in counts
          do (format t "~&~@(~a~):~20t~5d~,5t: ~6,2f%~%"
                     label count (* 100 (/ count total))))))
