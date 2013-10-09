#|
  This file is a part of Envy project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage envy
  (:use :cl)
  (:export :config-env-var
           :defconfig
           :config))
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

(defun config (package-name)
  (let* ((package (find-package package-name))
         (package-name (package-name package))
         (env-var (config-env-var package-name)))
    (unless env-var
      (error "Package \"~A\" is not configured. Set config-env-var."
             package-name))
    (let ((env (asdf::getenv env-var)))
      (if env
          (let ((symbol (find-symbol env package)))
            (if (and symbol
                     (get symbol 'configurationp)
                     (boundp symbol))
                (values (append (symbol-value symbol)
                                (gethash package-name *package-common-configurations* nil))
                        t)
                (values nil nil)))
          (values nil nil)))))
