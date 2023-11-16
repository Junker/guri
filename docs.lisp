(in-package #:guri)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass flags-formatter (docs:documentation-formatter) ())

  (defmethod docs:format-documentation ((formatter flags-formatter) type var text)
    (concatenate 'string text "

FLAGS - one or list of:
   - :RELAXED - Parse the URI more relaxedly than the RFC 3986 grammar specifies, fixing up or ignoring common mistakes in URIs coming from external sources. This is also needed for some obscure URI schemes where ; separates the host from the path. Don’t use this flag unless you need to.
   - :ENCODED - When parsing a URI, this indicates that %-encoded characters in the user, path, query, and fragment fields should not be decoded. (And likewise the host field if :NON-DNS flag is also set.) When building a URI, it indicates that you have already %-encoded the components, and so GUri should not do any encoding itself.
   - :NON-DNS - The host component should not be assumed to be a DNS hostname or IP address (for example, for smb URIs with NetBIOS hostnames).
   - :ENCODED-QUERY - Same as `:ENCODED`, for the query field only.
   - :ENCODED-PATH - Same as `:ENCODED`, for the path only.
   - :ENCODED-FRAGMENT - Same as `:ENCODED`, for the fragment field only.
   - :SCHEME-NORMALIZE - A scheme-based normalization will be applied. For example, when parsing an HTTP URI changing omitted path to / and omitted port to 80; and when building a URI, changing empty path to / and default port 80). This only supports a subset of known schemes.
")))

(docs:define-docs
  :formatter flags-formatter
  (function build
    "Creates a new GURI instance from the given components according to FLAGS
e.g. (guri:join \"https\" \"example.org\" \"/blog\" :query \"a=1\" :fragment \"comments\" :user \"user\" :password \"pass\")")

  (function validp "Parses URI according to FLAGS, to determine whether it is a valid [absolute URI][relative-absolute-uris], i.e. it does not need to be resolved relative to another URI using PARSE-RELATIVE.
If it’s not a valid URI, an error is returned explaining how it’s invalid.")

  (function join "Joins the given components together according to FLAGS to create an absolute URI string.
When host is present, path must either be empty or begin with a slash (/) character. When host is not present, path cannot begin with two slash characters (//). See RFC 3986, section 3.")

  (function parse "Parses URI-STRING according to FLAGS. If the result is not a valid [absolute URI][relative-absolute-uris], it will be discarded, and an error returned.")

  (function resolve-relative "Parses URI-REF according to FLAGS and, if it is a [relative URI][relative-absolute-uris], resolves it relative to BASE-URI-STRING. If the result is not a valid absolute URI, it will be discarded, and an error returned.
(If BASE-URI-STRING is NIL, this just returns URI-REF, or NULL if URI-REF is invalid or not absolute.")

  (function split "Parses URI-STRING (which can be an [absolute or relative URI][relative-absolute-uris]) according to FLAGS, and returns the pieces. Any component that doesn’t appear in URI-STRING will be returned as NIL (but note that all URIs always have a path component, though it may be the empty string).

If FLAGS contains :ENCODED, then %-encoded characters in URI-STRING will remain encoded in the output strings. (If not, then all such characters will be decoded.) Note that decoding will only work if the URI components are ASCII or UTF-8, so you will need to use :ENCODED flag if they are not."))

(docs:define-docs
  (function escape "Escapes a string for use in a URI.
Normally all characters that are not “unreserved” (i.e. ASCII alphanumerical characters plus dash, dot, underscore and tilde) are escaped. But if you specify characters in ALLOWED-CHARS they are not escaped. This is useful for the “reserved” characters in the URI specification, since those are allowed unescaped in some portions of a URI.")

  (function unescape "Unescapes a whole escaped string.
If any of the characters in ILLEGAL-CHARS or the NUL character appears as an escaped character in ESCAPED-STRING, then that is an error and NULL will be returned. This is useful if you want to avoid for instance having a slash being expanded in an escaped path element, which might confuse pathname handling.")

  (function uri-fragment "Gets URI‘s fragment, which may contain %-encoding, depending on the FLAGS with which URI was created.")

  (function uri-host "Gets URI‘s host. This will never have %-encoded characters, unless it is non-UTF-8 (which can only be the case if URI was created with :NON-DNS flag).
If uri contained an IPv6 address literal, this value will be just that address, without the brackets around it that are necessary in the string form of the URI. Note that in this case there may also be a scope ID attached to the address. Eg, fe80::1234%``em1 (or fe80::1234%``25em1 if the string is still encoded).")

  (function uri-password "Gets URI‘s password, which may contain %-encoding, depending on the FLAGS with which URI was created")

  (function uri-path "Gets URI‘s path, which may contain %-encoding, depending on the FLAGS with which URI was created.")

  (function uri-port "Gets URI‘s port.")

  (function uri-query "Gets URI‘s query, which may contain %-encoding, depending on the FLAGS with which URI was created.")

  (function uri-scheme "Gets URI‘s scheme. Note that this will always be all-lowercase, regardless of the string or strings that URI was created from.")

  (function uri-user "Gets the ‘username’ component of URI‘s userinfo, which may contain %-encoding, depending on the FLAGS with which URI was created.")

  (function uri-to-string "Returns a string representing URI.
This is not guaranteed to return a string which is identical to the string that URI was parsed from. However, if the source URI was syntactically correct (according to RFC 3986), and it was parsed with :ENCODED flag, then URI-TO-STRING is guaranteed to return a string which is at least semantically equivalent to the source URI (according to RFC 3986)."))
