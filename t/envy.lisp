(in-package :cl-user)
(defpackage :envy.myapp.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig))
(in-package :envy.myapp.config)

(setf (config-env-var) "APP_ENV")

(defconfig :common
  `(:application-root #P"/path/to/application/"))

(defconfig |development|
  '(:a 1
    :b 2))

(defconfig |production|
  '(:a 10
    :b 200))

(defconfig |staging|
  `(:a 0
    :c 3000
    ,@|production|))

(in-package :cl-user)
(defpackage envy-test
  (:use :cl
        :envy
        :cl-test-more))
(in-package :envy-test)

(plan 13)

(defparameter *env-var* "APP_ENV")

(defparameter *env-backup*
  (uiop:getenv *env-var*))

(diag "unbound")
(setf (uiop:getenv *env-var*) "")
(is (getf (config :envy.myapp.config) :application-root) #P"/path/to/application/")
(is (getf (config :envy.myapp.config) :a) nil)

(diag "development")
(setf (uiop:getenv *env-var*) "development")

(is (getf (config :envy.myapp.config) :a) 1)
(is (getf (config :envy.myapp.config) :b) 2)
(is (getf (config :envy.myapp.config) :application-root)
    #P"/path/to/application/"
    "Has a common configuration")

(diag "production")
(setf (uiop:getenv *env-var*) "production")

(is (getf (config :envy.myapp.config) :a) 10)
(is (getf (config :envy.myapp.config) :b) 200)
(is (getf (config :envy.myapp.config) :application-root)
    #P"/path/to/application/"
    "Has a common configuration")

(diag "staging")
(setf (uiop:getenv *env-var*) "staging")

(is (getf (config :envy.myapp.config) :a) 0
    "Can override a parent configuration")
(is (getf (config :envy.myapp.config) :c) 3000
    "Can add another configuration")
(is (getf (config :envy.myapp.config) :application-root)
    #P"/path/to/application/"
    "Has a common configuration")

(diag "other environment variable")
(setf (uiop:getenv *env-var*) "test")
(is (getf (config :envy.myapp.config) :application-root) #P"/path/to/application/")
(is (getf (config :envy.myapp.config) :a) nil)

(finalize)

(setf (uiop:getenv *env-var*) (or *env-backup* ""))
