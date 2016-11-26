(in-package :cl-user)
(defpackage envy
  (:use :cl)
  (:export :config-env-var
           :defconfig
           :config
           :env-config))
(in-package :envy)

(defvar *config-env-map* (make-hash-table :test 'eq))
(defvar *package-configurations* (make-hash-table :test 'eq))

(defun ensure-package (package)
  (etypecase package
    ((or string symbol) (find-package package))
    (package package)))

(defun config-env-var (&optional (package *package*))
  (gethash (ensure-package package) *config-env-map*))

(defun (setf config-env-var) (val &optional (package *package*))
  (setf (gethash (ensure-package package) *config-env-map*)
        val))

(defun env-config (env &optional (package *package*))
  (let ((config-map (gethash (ensure-package package) *package-configurations*)))
    (if config-map
        (append (gethash env config-map)
                (gethash :common config-map))
        nil)))

(defun (setf env-config) (configurations env &optional (package *package*))
  (let ((config-map (gethash (ensure-package package) *package-configurations*)))
    (unless config-map
      (setf config-map (make-hash-table :test 'equal))
      (setf (gethash (ensure-package package) *package-configurations*) config-map))

    (setf (gethash (if (eq env :common)
                       env
                       (princ-to-string env))
                   config-map)
          configurations)))

(defmacro defconfig (name configurations)
  `(progn
     ;; for backward-compatibility.
     ;; If the env name is a symbol, define a special variable.
     (when (and (symbolp ',name)
                (not (keywordp ',name)))
       (defparameter ,name ,configurations))
     (setf (env-config ',name) ,configurations)))

(defun config (package &optional key)
  (let* ((package (ensure-package package))
         (env-var (config-env-var package)))
    (unless env-var
      (error "Package \"~A\" is not configured. Set which environment variable to determine a configuration by using ~S."
             (package-name package)
             'config-env-var))
    (let ((configs (env-config (asdf::getenv env-var) package)))
      (if key
          (getf configs key)
          configs))))

(defun config* (&optional key)
  (config (package-name *package*) key))
