(in-package :cl-user)
(defpackage envy-test-asd
  (:use :cl :asdf))
(in-package :envy-test-asd)

(defsystem envy-test
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:envy
               :prove
               :uiop)
  :components ((:module "t"
                :components
                ((:test-file "envy"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
