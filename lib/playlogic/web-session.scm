(define-module web-session
  (use srfi-98)                         ; get-environment-variable

  (use gauche.generator)
  (use data.random)
  (use rfc.sha)
  (use util.digest)

  (use makiki)

  (add-load-path "../../gosh-modules/net-twitter" :relative)
  (add-load-path "../../gosh-modules/net-oauth" :relative)

  (use net.oauth)
  (use net.twitter)
  (use net.twitter.account :prefix twitter-account:)
  (use net.twitter.auth)

  (add-load-path "." :relative)
  (use bulma-utils)
  (add-load-path ".." :relative)
  (use dbi-query)

  (export session-show-login-page
          session-verify-auth
          valid-session-id?
          session-add-cookie!))

(select-module web-session)

(define (valid-session-id? await dbi-conn session-id)
  (pair? (do-query await dbi-conn '(SELECT 1 FROM sessions
                                WHERE "session_id" = ?)
                 session-id)))

(define (session-add-cookie! req session-id)
  (response-cookie-add! req "sessionid" session-id
                        :expires (+ (sys-time) (* 24 60 60 180))
                        :max-age (* 24 60 60 180)
                        :path "/"))

(define (session-show-login-page await dbi-conn req)
  (let-params req ([session-id "c:sessionid"])
    (if (and session-id (valid-session-id? await dbi-conn session-id))
        (container/
         '(p "You are already logged in. "
             (a (@ (href "/")) "Back to Home")))

        (let ([session-id (new-session-id!)])
          (session-add-cookie! req session-id)
          (let* ((key (get-environment-variable "TWITTER_API_KEY"))
                 (secret (get-environment-variable "TWITTER_API_KEY_SECRET"))
                 (temp-cred #?=(twitter-authenticate-request key secret)))
            (set! (~ *twitter-creds* session-id) `((temp-cred . ,temp-cred)))
            (container/
             `(p (a (@ (href ,(twitter-authorize-url temp-cred))) "login"))
             ))))))

(define (session-verify-auth await dbi-conn req)
  (let-params req ([verifier "q:oauth_verifier"]
                   [session-id "c:sessionid"])
    (let* ((key (get-environment-variable "TWITTER_API_KEY"))
           (secret (get-environment-variable "TWITTER_API_KEY_SECRET"))
           (temp-cred (and session-id
                           (cdr (assoc 'temp-cred
                                       (~ *twitter-creds* session-id))))))
      (if temp-cred
          (begin
            (let* ((cred (twitter-authorize temp-cred verifier))
                   (acc (twitter-account:verify-credentials/json cred)))
              (let ((twitter-id (cdr (assoc "id" acc)))
                    (screen-name (cdr (assoc "screen_name" acc))))
                (do-query await dbi-conn
                          '(INSERT INTO "sessions" ("session_id" |,| "user_id")
                                   VALUES (? |,| ?)
                                   |;|
                                   INSERT OR REPLACE INTO "users"
                                   ("user_id" |,| "display_name")
                                   VALUES (? |,| ?))
                          session-id
                          #"twitter-~twitter-id"
                          #"twitter-~twitter-id"
                          screen-name
                          ))

              (set! (~ *twitter-creds* session-id) #f)
              (container/
               '(p "Done! "
                   (a (@ (href "/")) "Back to Home"))
               )))
          (error "No session")))))

(define *twitter-creds* (make-hash-table 'string=?))

(define twitter-authorize-url
  (oauth-authorize-constructor
   "https://api.twitter.com/oauth/authorize"))

(define (new-session-id!)
  (define algo (make <sha256>))
  (define random-bytes (generator->bytevector uint8s 16))

  (digest-update! algo random-bytes)

  (let ((key (digest-hexify (digest-final! algo))))
    #"sess-~key"))
