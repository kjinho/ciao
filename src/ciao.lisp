;;;; ciao.lisp
(defpackage #:ciao
  (:use #:cl)
  (:export
   :oauth2-auth-server
   :oauth2-client
   :oauth2
   :get-auth-request-url
   ;; :get-client-id
   ;; :get-scopes
   :get-access-token
   :get-refresh-token
   :headers
   :oauth2/auth-code
   :oauth2/refresh-token
   :oauth2/request-auth-code/browser
   :*OOB-uri*
   :*google-auth-server*
   :*clio-auth-server*
   ))
(in-package #:ciao)

(defclass oauth2-auth-server ()
  ((auth-url
    :initarg :auth-url
    :reader get-auth-url)
   (token-url
    :initarg :token-url
    :reader get-token-url)
   (tokeninfo-url
    :initarg :tokeninfo-url
    :reader get-tokeninfo-url)
   (revoke-url
    :initarg :revoke-url
    :reader revoke-url)
    ))

(defclass oauth2-client ()
  ((id
    :initarg :id
    :reader get-id)
   (secret
    :initarg :secret
    :reader get-secret)))

(defclass oauth2 ()
  ((client
    :initarg :client
    :reader get-client)
   (auth-server
    :initarg :auth-server
    :reader get-auth-server)
   (scopes
    :initarg :scopes
    :initform nil
    :reader get-scopes)
   (token-type
    :initarg :token-type
    :initform nil
    :reader get-token-type)
   (access-token
    :initarg :scopes
    :initform nil)
   (refresh-token
    :initarg :refresh-token
    :initform nil
    :reader get-refresh-token)
   (expiration
    :initarg expiration
    :initform 'inf+
    :reader get-expiration)))

(defparameter *OOB-uri* "urn:ietf:wg:oauth:2.0:oob"
  "From Google: \"This value indicates that Google's authorization server
should return the authorization code in the browser's title bar. This
option is useful if the client cannot listen on an HTTP port without
significant modifications to the client.\"")

(defparameter *google-auth-server*
  (make-instance 'oauth2-auth-server
                 :auth-url
                 "https://accounts.google.com/o/oauth2/auth"
                 :token-url
                 "https://accounts.google.com/o/oauth2/token"
                 :tokeninfo-url
                 "https://www.googleapis.com/oauth2/v1/tokeninfo"
                 :revoke-url
                 "https://accounts.google.com/o/oauth2/revoke")
  "An instance of an oauth2-auth-server object with information
relevant to access Google services.")

(defparameter *clio-auth-server*
  (make-instance 'oauth2-auth-server
                 :auth-url
                 "https://app.clio.com/oauth/authorize"
                 :token-url
                 "https://app.clio.com/oauth/token"
                 :revoke-url
                 "https://app.clio.com/oauth/deauthorize")
  "An instance of an oauth2-auth-server object with information
relevant to access Clio services")

;; oauth2-server functions

(defun get-auth-request-url (auth-server
                             &key
                               client
                               scopes                                 
                               (redirect-uri *OOB-URI*)
                               (state nil)
                               (extra-parameters nil))
  "Returns the QURI:URI to obtain the authentication code."
  (quri:make-uri
   :defaults
   (get-auth-url auth-server)
   :query
   (delete nil
           (apply #'list
                  (cons "response_type" "code")
                  (cons "client_id" (get-id client))
                  (cons "redirect_uri" redirect-uri)
                  (when (not (null scopes))
                    (cons "scope" (format nil "~{~A~^ ~}" scopes)))
                  (when (not (null state))
                    (cons "state" (or state "")))
                  extra-parameters))))

;; oauth2 object functions

(defun get-client-id (oauth)
  "Returns the client-id for an oauth2 object"
  (get-id (get-client oauth)))

(defun get-access-token (oauth
                         &key
                           (re-acquire? t)
                           (who #'get-access-token))
  "Returns the access-token for an oauth2 object"
  (cond ((or (eq 'inf+ (get-expiration oauth))
             (< (get-universal-time) (get-expiration oauth)))
         (slot-value oauth 'access-token))
        (re-acquire?
         (re-acquire-token! oauth :who who)
         (slot-value oauth 'access-token))
        (t nil)))

(defun re-acquire-token! (oauth &key (who #'re-acquire-token!))
  "Refreshes the token for the oauth2 object"
  (cond ((get-refresh-token oauth)
         (refresh-token! oauth :who #'re-acquire-token!))
        (t
         (error (format nil "~a: access token expired; no method of re-acquiring access" who)))))


(defun acquire-token/auth-code! (oauth
                                 auth-code
                                 &key
                                   (redirect-uri *OOB-URI*)
                                   (who #'acquire-token/auth-code!))
  "Updates the oauth2 object with a token using the auth-code"
  (let ((now (get-universal-time))
        (json (acquire-token/auth-code/json
               oauth
               :auth-code auth-code
               :redirect-uri redirect-uri)))
    (reset-from-json! oauth who now json)));; FIXME

(defun refresh-token! (oauth &key (who #'refresh-token!))
  "Updates the oauth2 object with a refreshed token"
  (unless (get-refresh-token oauth)
    (error (format nil "~a: refresh-token not available" who)))
  (let ((now (get-universal-time))
        (json (refresh-token/json oauth))); :who who)))
    (reset-from-json! oauth who now json)))

(defun string-split (substr mainstr)
  "Returns a list of strings that constitute mainstr split
wherever substr occurs"
  (loop with l = (length substr)
        for i = 0 then (+ j l)
        as j = (search substr mainstr :start2 i)
        collect (subseq mainstr i j)
        while j))

(defun reset-from-json! (oauth who now json-string)
  "Tool to update the oauth2 object"
  (let* ((json (json:decode-json-from-string json-string))
         (new-access-token (cdr (assoc :access--token json)))
         (new-token-type (cdr (assoc :token--type json)))
         (new-refresh-token (cdr (assoc :refresh--token json)))
         (new-scope (or (cdr (assoc :scope json))
                        ""))
         (new-expires-in (or (cdr (assoc :expires--in json))
                             'inf+)))
    (unless (string-equal new-token-type "Bearer")
      (error (format nil "~a: unsupported token type ~a"
                     who
                     new-token-type)))
    (setf (slot-value oauth 'access-token) new-access-token)
    (setf (slot-value oauth 'token-type) new-token-type)
    (when new-refresh-token
      (setf (slot-value oauth 'refresh-token) new-refresh-token))
    (when (and (numberp now) (numberp new-expires-in))
      (setf (slot-value oauth 'expiration)
            (+ now new-expires-in)))
    (unless (equal new-scope "")
      (setf (slot-value oauth 'scopes) (string-split " +" new-scope)))
    nil
    ))
                                
(defun acquire-token/auth-code/json
    (oauth &key auth-code
             redirect-uri)
             ;(who #'acquire-token/auth-code/json))
  "Using the given auth-code, pull the result from the oauth2-server"
  (dex:post (get-token-url (get-auth-server oauth))
            :headers (form-headers)
            :content (body/acquire-token oauth
                                         :auth-code auth-code
                                         :redirect-uri redirect-uri)))

(defun form-headers ()
  (list (cons "Content-Type" "application/x-www-form-urlencoded")))

(defun body/refresh-token (oauth)
  "helper function to generate the refresh-token request"
  (list (cons "grant_type" "refresh_token")
        (cons "client_id" (get-client-id oauth))
        (cons "client_secret" (get-secret (get-client oauth)))
        (cons "refresh_token" (get-refresh-token oauth))))
  
(defun body/acquire-token (oauth
                           &key auth-code redirect-uri)
  "helper function to generate the acquire-token request"
  (list (cons "grant_type" "authorization_code")
        (cons "client_id" (get-client-id oauth))
        (cons "client_secret" (or (get-secret (get-client oauth))
                                  ""))
        (cons "code" auth-code)
        (cons "redirect_uri" redirect-uri)))

(defun refresh-token/json (oauth); &key (who #'refresh-token/json))
  "Calls the server for an updated refresh-token info"
  (dex:post
   (get-token-url (get-auth-server oauth))
   :headers (form-headers)
   :content (body/refresh-token oauth)))

(defun headers (oauth)
  "Returns the authorization headers"
  (list (cons "Authorization"
              (format nil "Bearer ~a" (get-access-token oauth)))))
                      

(defun oauth2/auth-code
    (auth-server client auth-code &key
                                    (redirect-uri *oob-uri*))
  "Returns an oauth2 object using the given auth-code to request an access
token. The redirect-uri must match the auth-code redirect-uri."
  (let ((oauth (make-instance 'oauth2
                              :auth-server auth-server
                              :client client)))
    (acquire-token/auth-code!
     oauth
     auth-code
     :redirect-uri redirect-uri
     :who #'oauth2/auth-code)
    oauth))

(defun oauth2/refresh-token
    (auth-server client refresh-token)
  "Returns an oauth2 object using the given refresh-token to acquire a new
access token."
  (let ((oauth (make-instance 'oauth2
                              :auth-server auth-server
                              :client client)))
    (setf (slot-value oauth 'refresh-token) refresh-token)
    (refresh-token! oauth :who #'oauth2/refresh-token)
    oauth))


(defun servlet (&key (port 5000))
  "Start a blocking servlet that waits for code and state."
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                 :port port))
        (code-res nil)
        (state-res nil))
    (unwind-protect
         (progn 
           (hunchentoot:start acceptor)
           (hunchentoot:define-easy-handler
               (oauth :uri "/oauth") (code state)
             (setf (hunchentoot:content-type*) "text/plain")
             (setf code-res code)
             (setf state-res state)
             (format nil "Success"))
           (loop
             (when code-res
               (return (list (cons "code" code-res)
                             (cons "state" state-res))))))
      (progn
        (hunchentoot:stop acceptor)))))
    
   
(defun oauth2/request-auth-code/browser (auth-server client
                                         &key
                                           (scopes nil)
                                           (port 5000))
  "Given an auth-server definition (e.g., *google-auth-server*),
an auth-client object, and a list of strings defining the scope,
initiates the authentication process."
  (let* ((redirect-uri (format nil "http://127.0.0.1:~a/oauth" port))
         (auth-url (get-auth-request-url auth-server
                                         :client client
                                         :scopes scopes
                                         :redirect-uri
                                         redirect-uri)))
    (trivial-open-browser:open-browser
     (quri:render-uri auth-url))
    (let ((auth-code (cdr (assoc "code" (servlet :port port) :test #'string-equal))))
      (oauth2/auth-code auth-server client auth-code
                        :redirect-uri redirect-uri))))
