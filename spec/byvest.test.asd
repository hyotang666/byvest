; vim: ft=lisp et
(in-package :asdf)
(defsystem :byvest.test :depends-on (:jingoh :jingoh.parallel "byvest") :components
 ((:file "byvest")) :perform (test-op (o c) (symbol-call :jingoh.parallel :pexamine :byvest)))
