#|
  This file is a part of Envy project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage envy-asd
  (:use :cl :asdf))
(in-package :envy-asd)

(defsystem envy
  :version "0.1"
  :author "Eitarow Fukamachi"
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
  :in-order-to ((test-op (load-op envy-test))))
