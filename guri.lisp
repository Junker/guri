(in-package #:guri)

(defvar *uri-flags*
  (list :parse-relaxed glib:+uri-flags-parse-relaxed+
        :has-password glib:+uri-flags-has-password+
        :has-auth-params glib:+uri-flags-has-auth-params+
        :encoded glib:+uri-flags-encoded+
        :non-dns glib:+uri-flags-non-dns+
        :encoded-query glib:+uri-flags-encoded-query+
        :encoded-path glib:+uri-flags-encoded-path+
        :encoded-fragment glib:+uri-flags-encoded-fragment+
        :scheme-normalize glib:+uri-flags-scheme-normalize+))

(defvar *default-parse-flags* '(:has-password :has-auth-params))

(defun prepare-flags (flags)
  (let ((flags (ensure-list flags)))
    (assert (subsetp flags *uri-flags*)
            (flags)
            "Unknown flags used: ~A" flags)
    (apply #'logior (mapcar (lambda (k)
                              (or (getf *uri-flags* k) 0))
                            flags))))

(defun port-out (port)
  (if (= port -1)
      nil
      port))

(defun port-in (port)
  (or port -1))


;; PUBLIC

(defun build (scheme host path &key port query fragment user password auth-params flags)
  (if user
      (glib:uri-build-with-user (prepare-flags flags) scheme user password
                                auth-params host (port-in port) (or path "") query fragment)
      (glib:uri-build (prepare-flags flags) scheme nil host (port-in port) (or path "") query fragment)))

(defun validp (uri &key flags)
  (glib:uri-is-valid uri (prepare-flags flags)))

(defun escape (unescaped-string &key allowed-chars allow-utf8)
  (glib:uri-escape-string unescaped-string allowed-chars allow-utf8))

(defun unescape (escaped-string &key illegal-chars)
  (glib:uri-unescape-string escaped-string illegal-chars))

(defun join (scheme host path &key port query fragment user password auth-params flags)
  (if user
      (glib:uri-join-with-user (prepare-flags flags) scheme user password
                               auth-params host (port-in port) (or path "") query fragment)
      (glib:uri-join (prepare-flags flags) scheme nil host (port-in port) (or path "") query fragment)))

(defun parse (uri-string &key flags)
  (let* ((flags (prepare-flags (append *default-parse-flags* (ensure-list flags)))))
    (glib:uri-parse uri-string flags)))

(defun split (uri-string &key flags)
  (let* ((flags (prepare-flags (append *default-parse-flags* (ensure-list flags))))
         (vals (multiple-value-list
                (glib:uri-split-with-user uri-string flags))))
    (setf (nth 6 vals)
          (port-out (nth 6 vals)))
    (apply #'values vals)))

(defun resolve-relative (base-uri-string uri-ref &key flags)
  (glib:uri-resolve-relative base-uri-string uri-ref flags))

(defun uri-port (instance)
  (port-out (glib:uri-port instance)))

;; (defun parse-params (params separators &key flags)
;;   (glib:uri-parse-params params -1 separators (prepare-flags flags)))
