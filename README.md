# GUri

URL Parser/Builder system for Common Lisp.

## Requirements

- [GLib](https://gitlab.gnome.org/GNOME/glib) installed

## Installation

This system can be installed from [UltraLisp](https://ultralisp.org/) like this:

```common-lisp
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload "guri")
```

## Usage

```common-lisp
(guri:join "https" "example.org" "/blog" :query "a=1" :fragment "comments")
;; "https://example.org/blog?a=1#comments"
(guri:join "https" "example.org" "/blog" :query "a=1" :fragment "comments" :user "user" :password "pass")
;; "https://user:pass@example.org/blog?a=1#comments"
(multiple-value-bind (result scheme user password auth-params host port path query fragment)
    (guri:split "https://example.org/blog?a=1#comments"))
;; T
;; "https"
;; NIL
;; NIL
;; NIL
;; "example.org"
;; NIL
;; "/blog"
;; "a=1"
;; "comments"

(let ((uri (guri:build "https" "example.org" "/blog")))
  (guri:uri-host uri)
  ;; "example.org"
  (guri:uri-port uri)
  ;; NIL
  (guri:uri-path uri)
  ;; "/blog"
  (guri:uri-to-string uri)
  ;; https://example.org/blog
  )

(let ((uri (guri:build "https" "example.org" "/blog" :flags '(:normalize-scheme))))
  (guri:uri-port uri)
  ;; 443
  )

(let ((uri (guri:parse "https://example.org/blog?a=1#comments")))
  (guri:uri-host uri)
  ;; "example.org"
  (guri:uri-port uri)
  ;; NIL
  (guri:uri-path uri)
  ;; "/blog"
  (guri:uri-to-string uri)
  ;; https://example.org/blog?a=1#comments
  )

```

## Flags

 - :RELAXED - Parse the URI more relaxedly than the RFC 3986 grammar specifies, fixing up or ignoring common mistakes in URIs coming from external sources. This is also needed for some obscure URI schemes where ; separates the host from the path. Don’t use this flag unless you need to.
 - :ENCODED - When parsing a URI, this indicates that %-encoded characters in the user, path, query, and fragment fields should not be decoded. (And likewise the host field if :NON-DNS flag is also set.) When building a URI, it indicates that you have already %-encoded the components, and so GUri should not do any encoding itself.
 - :NON-DNS - The host component should not be assumed to be a DNS hostname or IP address (for example, for smb URIs with NetBIOS hostnames).
 - :ENCODED-QUERY - Same as `:ENCODED`, for the query field only.
 - :ENCODED-PATH - Same as `:ENCODED`, for the path only.
 - :ENCODED-FRAGMENT - Same as `:ENCODED`, for the fragment field only.
 - :SCHEME-NORMALIZE - A scheme-based normalization will be applied. For example, when parsing an HTTP URI changing omitted path to / and omitted port to 80; and when building a URI, changing empty path to / and default port 80). This only supports a subset of known schemes.
