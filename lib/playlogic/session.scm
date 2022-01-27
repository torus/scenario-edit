(define-module playlogic.session
  (use srfi-27)                         ; random-integer
  (use srfi-98)                         ; get-environment-variable

  (use makiki)

  (add-load-path "../../gosh-modules/net-twitter" :relative)
  (add-load-path "../../gosh-modules/net-oauth" :relative)

  (use net.oauth)
  (use net.twitter)
  (use net.twitter.account :prefix twitter-account:)
  (use net.twitter.auth)

  (use playlogic.editor)
  (use playlogic.datastore)

  (export session-show-login-page
          session-verify-auth
          valid-session-id?))

(select-module playlogic.session)

(define (valid-session-id? await session-id)
  (pair? (query* await '(SELECT 1 FROM sessions
                                WHERE "session_id" = ?)
                 session-id)))

(define (session-show-login-page await req)
  (let-params req ([session-id "c:sessionid"])
    (if (and session-id (valid-session-id? await session-id))
        (container/
         '(p "You have been already logged in. "
             (a (@ (href "/")) "Back to Home")))

        (let ([session-id (new-session-id!)])
          (response-cookie-add! req "sessionid" session-id)
          (let* ((key (get-environment-variable "TWITTER_API_KEY"))
                 (secret (get-environment-variable "TWITTER_API_KEY_SECRET"))
                 (temp-cred #?=(twitter-authenticate-request key secret)))
            (set! (~ *twitter-creds* session-id) `((temp-cred . ,temp-cred)))
            (container/
             `(p (a (@ (href ,(twitter-authorize-url temp-cred))) "login"))
             ))))))

(define (session-verify-auth await req)
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
                (query* await
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
  (let ((rand (random-integer #x10000000000000000)))
    (format #f "sess-~8,'0X-~16,'0X" (sys-time) rand)))
