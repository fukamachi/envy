#|
  This file is a part of Envy project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage envy-test-asd
  (:use :cl :asdf))
(in-package :envy-test-asd)

(defsystem envy-test
  :author "Eitarow Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:envy
               :cl-test-more
               :osicat)
  :components ((:module "t"
                :components
                ((:file "envy"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
