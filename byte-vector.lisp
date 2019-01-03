(defpackage :byvest(:use :cl :trivial-gray-streams)
  (:import-from :alexandria #:parse-body)
  (:export ; main api
    #:with-output-to-byte-vector
    #:with-input-from-byte-vector
    )
  (:export ; internals as dsl
    ;; as class
    #:byte-vector-stream
    ;; as constrctor
    #:make-byte-vector-input-stream
    #:make-byte-vector-output-stream
    #:make-byte-vector-side-effect-stream
    ;; miscellaneous
    #:get-output-to-byte-vector
  )
  )
(in-package :byvest)

#+design
(with-output-to-byte-vector(out)
  (write-byte 1 out))
; => #(1)
#+design
(with-input-from-byte-vector(in #(1 2 3))
  (read-byte in))
; => 1

;;; classes

(defclass byte-vector-stream(fundamental-binary-stream)())

(defclass byte-vector-input-stream(byte-vector-stream
				    fundamental-binary-input-stream)
  ((vector :initarg :vector :initform (error "Init vector is required.")
	   :reader stream-buffer)
   (index :initarg :start :accessor stream-file-position)
   (end :initarg :end :reader stream-file-length)
   ))

(defclass byte-vector-output-stream(byte-vector-stream
				     fundamental-binary-output-stream)
  ((element-type :initarg :element-type
		 :reader stream-element-type)
   (buffer :initform nil :accessor stream-buffer)))

(defclass byte-vector-side-effect-stream(byte-vector-stream
					 fundamental-binary-output-stream)
  ((buffer :initarg :vector :accessor stream-buffer)))

;;; constructors

(defun make-byte-vector-input-stream(vector &optional start end)
  (check-type vector vector) ; <--- ecl need this.
  (make-instance 'byte-vector-input-stream
		 :vector vector
		 :start (or start 0)
		 :end (or end (length vector))))

(defun make-byte-vector-output-stream(&key(element-type '(unsigned-byte 8)))
  (make-instance 'byte-vector-output-stream
		 :element-type element-type))

(defun make-byte-vector-side-effect-stream(vector)
  (assert(array-has-fill-pointer-p vector)(vector)
    "Does not have fill-pointer. ~S" vector)
  (make-instance 'byte-vector-side-effect-stream
		 :vector vector))
    
;;; miscellaneous

(defun get-output-to-byte-vector(stream)
  (check-type stream byte-vector-output-stream)
  (apply #'concatenate 'vector (reverse(stream-buffer stream))))

;;; write-byte

(defmethod stream-write-byte((stream byte-vector-output-stream)
			     byte)
  (assert(typep byte(stream-element-type stream))(byte)
    'simple-type-error
    :format-control "~S is not type-of ~S."
    :format-arguments `(,byte ,(stream-element-type stream))
    :expected-type (stream-element-type stream)
    :datum byte)
  (push (vector byte) (stream-buffer stream))
  byte)

(defmethod stream-write-byte((stream byte-vector-side-effect-stream)
			     byte)
  (vector-push-extend byte (stream-buffer stream)))

;;; read-byte

(defmethod stream-read-byte((stream byte-vector-input-stream))
  (let((index(stream-file-position stream)))
    (if(not(< index (stream-file-length stream)))
      :eof
      (progn (incf (stream-file-position stream))
	     (aref (stream-buffer stream) index)))))

;;; write-sequence

(defmethod stream-write-sequence((stream byte-vector-output-stream)
				 vector start end &key &allow-other-keys)
  (assert(typep vector `(VECTOR ,(stream-element-type stream) *))(vector)
    'simple-type-error
    :format-control "Stream must element-type ~S, but ~S"
    :format-arguments `(,(stream-element-type stream),(type-of vector))
    :expected-type `(VECTOR ,(stream-element-type stream) *)
    :datum vector)
  (if(not(or start end))
    (push vector (stream-buffer stream))
    (let((s (or start 0)))
      (push (make-array (- (or end (length vector)) s)
			:element-type (array-element-type vector)
			:displaced-to vector
			:displaced-index-offset s)
	    (stream-buffer stream))))
  vector)

(defmethod stream-write-sequence((stream byte-vector-side-effect-stream)
				 vector start end &key &allow-other-keys)
  (loop :with buffer = (stream-buffer stream)
	:for index :upfrom (or start 0) :below (or end (length vector))
	:do (vector-push-extend (aref vector index) buffer))
  vector)

;;; read-sequence

(defmethod stream-read-sequence((stream byte-vector-input-stream)
				     sequence start end &key &allow-other-keys)
  (let*((start1 (or start 0))
	(end1 (or end (length sequence)))
	(vector (stream-buffer stream))
	(start2 (stream-file-position stream))
	(end2 (stream-file-length stream))
	(range(min (- end1 start1)
		   (- end2 start2))))
    (replace sequence vector
	     :start1 start1 :end1 end1
	     :start2 start2 :end2 end2)
    (incf (stream-file-position stream)range)
    range))

;;; main api

(defmacro with-output-to-byte-vector((var
				       &optional vector
				       &key(element-type ''(unsigned-byte 8)))
				     &body body)
  (multiple-value-bind(body declare)(parse-body body)
    `(LET((,var ,(if vector
		   `(MAKE-BYTE-VECTOR-SIDE-EFFECT-STREAM ,vector)
		   `(MAKE-BYTE-VECTOR-OUTPUT-STREAM :ELEMENT-TYPE ,element-type))))
       (DECLARE(DYNAMIC-EXTENT ,var))
       ,@declare
       (UNWIND-PROTECT(PROGN ,@body
			     ,@(unless vector
				 `((GET-OUTPUT-TO-BYTE-VECTOR ,var))))
	 (CLOSE ,var)))))

(defmacro with-input-from-byte-vector((var vector &key start end index)
				      &body body)
  `(LET*((,var (MAKE-BYTE-VECTOR-INPUT-STREAM ,vector ,start ,end)))
     (DECLARE(DYNAMIC-EXTENT ,var))
     (UNWIND-PROTECT(PROG1 (PROGN ,@body)
			   ,@(when index
			       `((SETF ,index (STREAM-FILE-POSITION ,var)))))
       (CLOSE ,var))))
