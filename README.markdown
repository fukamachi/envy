# Envy

Configuration switcher by an environment variable inspired by Perl's [Config::ENV](http://search.cpan.org/~satoh/Config-ENV/lib/Config/ENV.pm).

## Usage

```common-lisp
(defpackage :myapp.config
  (:use :cl
        :envy))
(in-package :myapp.config)

(setf (config-env-var) "APP_ENV")

(defconfig :common
  `(:application-root ,(asdf:component-pathname (asdf:find-system :myapp))))

(defconfig |development|
  '(:debug T
    :database-type :sqlite
    :database-connection-spec '()))

(defconfig |production|
  '(:database-type :mysql
    :database-connection-spec '()))

(defconfig |staging|
  `(:debug T
    ,@|production|))
```

```common-lisp
(defpackage :myapp
  (:use :cl)
  (:import-from :envy
                :config)
(in-package :myapp)

(getf (config :myapp.config) :database-type)
```

## Tips

```common-lisp
(defpackage myapp.config
  (:use :cl
        :envy)
  (:shadow :envy
           :config)
  (:export :config))
(in-package :myapp.config)

(defconfig :common
  ...)

;;
;; ... Configurations ...
;;

(defun config ()
  (envy:config #.(package-name *package*)))
```

## See Also

- [Config::ENV](http://search.cpan.org/~satoh/Config-ENV/lib/Config/ENV.pm)

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the BSD 2-Clause License.
