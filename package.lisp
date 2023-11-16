(defpackage guri
  (:use #:cl #:alexandria)
  (:import-from #:glib
                #:uri-auth-params
                #:uri-fragment
                #:uri-host
                #:uri-password
                #:uri-path
                #:uri-query
                #:uri-scheme
                #:uri-user
                #:uri-to-string)
  (:export #:build
           #:validp
           #:escape
           #:join
           #:parse
           #:unescape
           #:resolve-relative
           #:split
           #:uri-auth-params
           #:uri-fragment
           #:uri-host
           #:uri-password
           #:uri-path
           #:uri-port
           #:uri-query
           #:uri-scheme
           #:uri-user
           #:uri-to-string))
