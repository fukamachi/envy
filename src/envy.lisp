#|
  This file is a part of Envy project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage envy
  (:use :cl)
  (:export :config-env-var
           :defconfig
           :config
           :env-var
           :remove-env-var))
(in-package :envy)

(defvar *config-env-map* (make-hash-table :test 'equal))
(defvar *package-common-configurations* (make-hash-table :test 'equal))

(defun config-env-var (&optional (package-name (package-name *package*)))
  (gethash (package-name (find-package package-name)) *config-env-map*))

(defun (setf config-env-var) (val &optional (package-name (package-name *package*)))
  (setf (gethash (package-name (find-package package-name)) *config-env-map*) val))

(defmacro defconfig (name configurations)
  (if (eq name :common)
      (let ((package-name (package-name *package*)))
        `(setf (gethash ,package-name *package-common-configurations*)
               ,configurations))
      `(progn
         (defparameter ,name ,configurations)
         (setf (get ',name 'configurationp) t))))

(defun package-config (package-name)
  (let* ((package (find-package package-name))
         (package-name (package-name package))
         (env-var (config-env-var package-name)))
    (unless env-var
      (error "Package \"~A\" is not configured. Set which environment variable to determine a configuration by using ~S."
             package-name
             'config-env-var))
    (let ((env (asdf::getenv env-var)))
      (if env
          (let ((symbol (find-symbol env package)))
            (if (and symbol
                     (get symbol 'configurationp)
                     (boundp symbol))
                (append (symbol-value symbol)
                        (gethash package-name *package-common-configurations* nil))
                nil))
          nil))))

(defun config (package-name &optional key)
  (if key
      (getf (package-config package-name) key)
      (package-config package-name)))

(defun env-var (var)
  (osicat:environment-variable var))

(defun (setf env-var) (val var)
  (setf (osicat:environment-variable var) val))

(defun remove-env-var (var)
  (osicat:makunbound-environment-variable var))
