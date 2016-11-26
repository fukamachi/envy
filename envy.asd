(in-package :cl-user)
(defpackage envy-asd
  (:use :cl :asdf))
(in-package :envy-asd)

(defsystem envy
  :version "2.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :components ((:module "src"
                :components
                ((:file "envy"))))
  :description "Configuration switcher by an environment variable."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op envy-test))))
