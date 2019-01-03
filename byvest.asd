; vim: ft=lisp et
(in-package :asdf)
(defsystem "byvest"
  :depends-on (
               "alexandria" ; public domain utilities.
               "trivial-gray-streams" ; wrapper for gray streams.
               )
  :in-order-to((test-op(test-op "byvest.test")))
  :components((:file "byte-vector")))
