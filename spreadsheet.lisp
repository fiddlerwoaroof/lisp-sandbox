(defpackage :fwoar.amex-parser
  (:use :cl )
  (:export ))
(in-package :fwoar.amex-parser)

(defvar *headings*
  '("Date" "Description" "Card Member" "Account #" "Amount" "Extended Details"
    "Appears On Your Statement As" "Address" "City/State" "Zip Code" "Country"
    "Reference" "Category"))

(defclass sheet ()
  ((%data :initarg :data :reader data-of)
   (%headings :initarg :headings :reader headings-of)))

(defclass column ()
  ((%name :initarg :name :reader name-of)))
(defclass computed-column ()
  ((%name :initarg :name :reader name-of)
   (%deps :initarg :deps :reader deps-of)
   (%fun :initarg :fun :reader fun-of)))

(defun computed-column (name deps fun)
  (fw.lu:new 'computed-column name deps fun))

(defgeneric ensure-column (column)
  (:method ((c string))
    (make-instance 'column :name c))
  (:method ((c column))
    c)
  (:method ((c computed-column))
    c))

(defgeneric column (sheet heading cont)
  (:method ((sheet sheet) (heading string) cont)
    (gethash heading
             (data-of sheet)))
  (:method ((sheet sheet) (heading column) cont)
    (gethash (name-of heading)
             (data-of sheet)))
  (:method ((sheet sheet) (heading computed-column) cont)
    (let* ((dep-defs (deps-of heading)))
      (apply #'map 'vector (fun-of heading)
             (mapcar (lambda (dep)
                       (or (gethash (name-of (ensure-column dep)) cont)
                           (column sheet dep cont)))
                     dep-defs)))))

(defun translate (column fun)
  (computed-column column (list column) fun))

(defun to-rows (sheet)
  (let ((headings (headings-of sheet)))
    (cons headings
          (apply #'map 'list #'list
                 (loop with cont = (make-hash-table :test #'equal)
                       for heading in headings
                       collect (column sheet heading cont))))))

(defun subset-sheet (sheet row-limit)
  (let ((headings (headings-of sheet)))
    (make-sheet headings
                (map 'list
                     (data-lens:∘ (lambda (it)
                                    (subseq it 0 row-limit))
                                  (data-lens:functionalize (data-of sheet)))
                     headings))))

(defun project-sheet (sheet columns)
  (make-sheet (mapcar (data-lens:∘ #'name-of #'ensure-column)
                      columns)
              (loop with cont = (make-hash-table :test #'equal)
                    for column in columns
                    for idx from 0
                    for col-val = (column sheet column cont)
                    do (setf (gethash (name-of (ensure-column column))
                                      cont)
                             col-val)
                    collect col-val)))

(defun augment (sheet new-columns)
  (project-sheet sheet
                 (append (headings-of sheet)
                         new-columns)))

(defun make-sheet (headings columns)
  (assert (= (length headings)
             (length columns))
          (headings columns))

  (let ((column-ht (make-hash-table :test #'equal)))
    (loop for column in columns
          for heading in headings
          do (setf (gethash heading column-ht) (coerce column 'vector)))
    (make-instance 'sheet
                   :data column-ht
                   :headings headings)))

(defun build-sheet (data)
  (destructuring-bind (headings . rows) data
    (let ((columns (make-hash-table :test #'equal)))
      (loop for heading in headings
            for idx from 0
            do (setf (gethash heading columns)
                     (map 'vector (data-lens:element idx) rows)))
      (make-instance 'sheet :data columns :headings headings))))

(defun parse-line (line)
  )

(defun parse-ts (ts)
  "for dates like \"2019-12-23 22:34:11 -0700\""
  (local-time:encode-timestamp
   0
   (parse-integer ts :start 17 :end 20)
   (parse-integer ts :start 14 :end 16)
   (parse-integer ts :start 11 :end 13)

   (parse-integer ts :start 8 :end 10)
   (parse-integer ts :start 5 :end 7)
   (parse-integer ts          :end 4)

   :offset
   (+ (* 60 60 (parse-integer ts :start 20 :end 23))
      (* 60 (parse-integer ts :start 23)))))

(defun pivot-first (rows summarizer)
  (destructuring-bind (headings . data) rows
    (declare (ignore headings))
    (let ((categorized (make-hash-table :test #'equal)))
      (loop for (cat . rest) in data
            do (push rest (gethash cat categorized)))

      (loop for category being the hash-keys of categorized using (hash-value dat)
            collect (list* category
                           (funcall (data-lens:over (data-lens:calling* summarizer category))
                                    (nreverse dat)))))))
