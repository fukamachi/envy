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
    :database-type :sqlite3
    :database-connection-spec (:database-name "sqlite3.db")))

(defconfig |production|
  '(:database-type :mysql
    :database-connection-spec (:database-name "test"
                               :usename "whoami"
                               :password "1234")))

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

###  Calling APP_ENV from the command line

For example, `APP_ENV=development sbcl`  runs a REPL process that uses development config values.

## Description

Envy is a configuration manager for various applications.

Envy uses an environment variable to determine a configuration to use. I'm not sure this is ideal even for Common Lisp applications, but this can separate configuration system from an implementation.

## Configurations

### Normal configurations

`ENVY:DEFCONFIG` is a macro for defining a named configuration.

Don't forget to set `(ENVY:CONFIG-ENV-VAR)` which is a name of environment variable to determine a configuration.

```common-lisp
(setf (config-env-var) "APP_ENV")

;; Use SQLite3 for development
(defconfig |development|
  '(:server :hunchentoot
    :database-type :sqlite3
    :database-connection-spec (:database-name "sqlite3.db")))

;; Use MySQL in production environment
(defconfig |production|
  '(:server :fcgi
    :database-type :mysql
    :database-connection-spec (:database-name "test"
                               :usename "whoami"
                               :password "1234")))
```

### Merging

Each configurations are represented as property lists. It means you can merge them by the default way of merging lists -- cons, append or/and splicing unquote.

```common-lisp
(defconfig |staging|
  `(:server :hunchentoot
    ,@|production|))
```

### Common configurations

You can also define a common configuration which will be used by all configurations.

```common-lisp
(defconfig :common
  `(:application-root ,(asdf:component-pathname (asdf:find-system :myapp))))
```

### Accessing to configuration

`ENVY:CONFIG` is an accessor to get the current configuration.

```common-lisp
(config :<configuration-package-name>)

(config :myapp.config)
;=> '(:server :hunchentoot
      :database-type :sqlite3
      :database-connection-spec (:database-name "sqlite3.db")
      :application-root #P"/path/to/application/")

(getf (config :myapp.config) :database-type)
;=> :sqlite3
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

Thank cho45 for the great product. I feel envy to you :)

- [Config::ENV](http://search.cpan.org/~satoh/Config-ENV/lib/Config/ENV.pm)

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the BSD 2-Clause License.
