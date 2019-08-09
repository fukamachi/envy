#|
  This file is a part of Envy project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage :envy.myapp.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig))
(in-package :envy.myapp.config)

(setf (config-env-var) "APP_ENV")

(defconfig :common
  `(:application-root #P"/path/to/application/"
    :dynamic-config ,(let ((i 0)) (lambda () (incf i)))))

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
        :cl-test-more)
  (:import-from :osicat
                :environment-variable
                :makunbound-environment-variable))
(in-package :envy-test)

(plan 15)

(defparameter *env-var* "APP_ENV")

(defparameter *env-backup*
  (environment-variable *env-var*))

(diag "unbound")
(osicat:makunbound-environment-variable *env-var*)
(is (getf (config :envy.myapp.config) :application-root) #P"/path/to/application/")
(is (getf (config :envy.myapp.config) :a) nil)

(diag "development")
(setf (environment-variable *env-var*) "development")

(is (getf (config :envy.myapp.config) :a) 1)
(is (getf (config :envy.myapp.config) :b) 2)
(is (getf (config :envy.myapp.config) :application-root)
    #P"/path/to/application/"
    "Has a common configuration")

(diag "production")
(setf (environment-variable *env-var*) "production")

(is (getf (config :envy.myapp.config) :a) 10)
(is (getf (config :envy.myapp.config) :b) 200)
(is (getf (config :envy.myapp.config) :application-root)
    #P"/path/to/application/"
    "Has a common configuration")

(diag "staging")
(setf (environment-variable *env-var*) "staging")

(is (getf (config :envy.myapp.config) :a) 0
    "Can override a parent configuration")
(is (getf (config :envy.myapp.config) :c) 3000
    "Can add another configuration")
(is (getf (config :envy.myapp.config) :application-root)
    #P"/path/to/application/"
    "Has a common configuration")

(diag "other environment variable")
(setf (environment-variable *env-var*) "test")
(is (getf (config :envy.myapp.config) :application-root) #P"/path/to/application/")
(is (getf (config :envy.myapp.config) :a) nil)

(diag "dynamic config")
(setf (environment-variable *env-var*) "development")
(is (config :envy.myapp.config :dynamic-config) 1
    "Can return execution-time evaled configuration")
(is (config :envy.myapp.config :dynamic-config) 2
    "Can return execution-time evaled configuration")

(finalize)

(if *env-backup*
    (setf (environment-variable *env-var*) *env-backup*)
    (makunbound-environment-variable *env-var*))
