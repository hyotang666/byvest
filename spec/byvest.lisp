(defpackage :byvest.spec (:use :cl :jingoh :byvest))
(in-package :byvest.spec)
(setup :byvest)

(requirements-about WITH-OUTPUT-TO-BYTE-VECTOR)

#+syntax
(WITH-OUTPUT-TO-BYTE-VECTOR (var &optional vector &key (element-type ''(unsigned-byte 8))) &body body) ; => result

;;;; Description:
; Like CL:WITH-OUTPUT-TO-STRING, get outputted bytes into vector.
#?(with-output-to-byte-vector(out)
    (write-byte 1 out))
=> #(1)
,:test equalp

;;;; Arguments and Values:

; var := symbol
; Bound by BYTE-VECTOR-OUTPUT-STREAM.
#?(let(s)
    (with-output-to-byte-vector(out)
      (setf s out))
    s)
:satisfies #`(& (typep $result 'byte-vector-stream)
		(output-stream-p $result))

; when VAR is not symbol, an error is signaled.
#?(with-output-to-byte-vector("out")
    (write-byte 1 "out"))
:signals error
,:lazy T

; vector := form which generate vector which has fill point

; When VECTOR is specified, outputted bytes are added to such vector
; as is by CL:PUSH-VECTOR-EXTEND.
; And in such context, return value is return value of BODY.
; When VECTOR is NIL, outputted bytes are returned as vector.
; The default is NIL.
#?(defparameter *v*(make-array 3 :initial-element 0
			       :fill-pointer 0
			       :element-type '(unsigned-byte 8)))
=> *V*
,:lazy NIL

#?(with-output-to-byte-vector(out *V*)
    (write-byte 1 out))
=> 1 ; return value of BODY.

#?*V* => #(1)
,:test equalp

#?(with-output-to-byte-vector(out nil)
    (write-byte 1 out))
=> #(1)
,:test equalp

; element-type := element-type-specifier

; Specifying element-type of returned vector.
; When VECTOR is also specified, ELEMENT-TYPE is ignored.
#?(with-output-to-byte-vector(out nil :element-type'bit)
    (write-byte 2 out))
:signals ERROR ; 2 is not of type BIT.

#?(let((v(make-array 2
		     :fill-pointer 0
		     :element-type '(unsigned-byte 3):initial-element 0)))
    (with-output-to-byte-vector(out v :element-type 'bit)
      (write-byte 2 out))
    v)
=> #(2) ; 2 is not of type BIT, but ignored.
,:test equalp

; body := implicit PROGN
; result := (or vector ; if VECTOR is NIL.
;		T ; if VECTOR is specified as non-NIL.
;		)

;;;; Affected By:
; none

;;;; Side-Effects:
; When VECTOR is specified, such vector is modified.

;;;; Notes:
; When VECTOR is specified NIL, returned vector does not have FILL-POINTER.
#?(with-output-to-byte-vector(out)
    (write-byte 4 out))
:satisfies #`(not(array-has-fill-pointer-p $vector))

;;;; Exceptional-Situations:
; When specified VECTOR does not have fill-pointer, an error is signaled.
#?(let((v(make-array 2 :element-type '(unsigned-byte 3):initial-element 0)))
    (with-output-to-byte-vector(out v :element-type 'bit)
      (write-byte 2 out))
    v)
:signals ERROR

(requirements-about WITH-INPUT-FROM-BYTE-VECTOR)

;;;; Description:
; Like CL:WITH-INPUT-FROM-STRING, within the BODY,
; we can read some bytes from specified vector.
#?(with-input-from-byte-vector(in #(12 23 34 45))
    (read-byte in))
=> 12

#+syntax
(WITH-INPUT-FROM-BYTE-VECTOR (var vector &key start end index) &body body) ; => result

;;;; Arguments and Values:

; var := symbol
; Bound by BYTE-VECTOR-INPUT-STREAM.
#?(with-input-from-byte-vector(in #())
    in)
:satisfies #`(& (typep $in 'byte-vector-stream)
		(input-stream-p $in))

; Can CL:READ-BYTE, CL:READ-SEQUENCE from such stream which has contents of VECTOR.
#?(with-input-from-byte-vector(in #(1 2 3))
    (read-byte in))
=> 1
#?(let((v(make-array 6 :element-type '(unsigned-byte 8) :initial-element 0)))
    (with-input-from-byte-vector(in #(1 2 3))
      (read-sequence v in))
    v)
=> #(1 2 3 0 0 0)
,:test equalp

; When VAR is not symbol, an error is signaled.
#?(with-input-from-byte-vector("IN" #(1 2 3))
    (read-byte "IN"))
:signals ERROR
,:lazy T

; vector := Form which generates vector
; When generated value is not VECTOR, an error is signaled.
#?(with-input-from-byte-vector(in '(1 2 3))
    (read-byte in))
:signals ERROR

; start := Non negative integer
; When specified, made stream's first contents becomes content of START position (inclusive) of generated VECTOR.
#?(with-input-from-byte-vector(in #(1 2 3) :start 1)
    (read-byte in))
=> 2

; When specified, and value is not non-negative-integer, an error is signaled.
#?(with-input-from-byte-vector(in #(1 2 3) :start -2)
    (read-byte in))
:signals ERROR
#?(with-input-from-byte-vector(in #(1 2 3) :start 1.3)
    (read-byte in))
:signals ERROR

; end :=

; index :=

; body :=

; result :=

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:


(requirements-about BYTE-VECTOR-STREAM)

#|[Class] BYTE-VECTOR-STREAM |#

;; Class Precedence List: (case in CLISP)
;; byte-vector-stream fundamental-binary-stream fundamental-stream fundamental-binary-stream fundamental-stream stream standard-object t

;; Effective Slots:

;; $OPEN [Type] BOOLEAN

;; $FASL [Type] BOOLEAN

;; $PENL [Type] BOOLEAN

;; $OPEN [Type] BOOLEAN

;; $FASL [Type] BOOLEAN

;; $PENL [Type] BOOLEAN

#| Notes: |#

(requirements-about MAKE-BYTE-VECTOR-INPUT-STREAM)

#|[Function] MAKE-BYTE-VECTOR-INPUT-STREAM
|#

#| Syntax:
(MAKE-BYTE-VECTOR-INPUT-STREAM vector &optional start end) => result
|#

#| Arguments and Values: |#

;; vector :=

;; start :=

;; end :=

;; result := 

#| Affected By: |#

#| Side-Effects: |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about MAKE-BYTE-VECTOR-OUTPUT-STREAM)

#|[Function] MAKE-BYTE-VECTOR-OUTPUT-STREAM
|#

#| Syntax:
(MAKE-BYTE-VECTOR-OUTPUT-STREAM &key (element-type '(unsigned-byte 8))) => result
|#

#| Arguments and Values: |#

;; element-type :=

;; result := 

#| Affected By: |#

#| Side-Effects: |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about MAKE-BYTE-VECTOR-SIDE-EFFECT-STREAM)

#|[Function] MAKE-BYTE-VECTOR-SIDE-EFFECT-STREAM
|#

#| Syntax:
(MAKE-BYTE-VECTOR-SIDE-EFFECT-STREAM vector) => result
|#

#| Arguments and Values: |#

;; vector :=

;; result := 

#| Affected By: |#

#| Side-Effects: |#

#| Notes: |#
#| Exceptional-Situations: |#

(requirements-about GET-OUTPUT-TO-BYTE-VECTOR)

#|[Function] GET-OUTPUT-TO-BYTE-VECTOR
|#

#| Syntax:
(GET-OUTPUT-TO-BYTE-VECTOR stream) => result
|#

#| Arguments and Values: |#

;; stream :=

;; result := 

#| Affected By: |#

#| Side-Effects: |#

#| Notes: |#

#| Exceptional-Situations: |#

