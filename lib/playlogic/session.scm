(define-module playlogic.session
  (use srfi-98)                         ; get-environment-variable

  (use makiki)

  (add-load-path "../gosh-modules/net-twitter" :relative)
  (add-load-path "../gosh-modules/net-twitter/Gauche-net-oauth" :relative)

  (use net.oauth)
  (use net.twitter)
  (use net.twitter.account :prefix twitter-account:)
  (use net.twitter.auth)



  (export session-show-login-page
          session-verify-auth))

(select-module playlogic.session)

(define (session-show-login-page await req)
  (let-params req ([sess "c:sessionid" :default (new-session-id!)])
    (response-cookie-add! req "sessionid" sess)
    (let* ((key (get-environment-variable "TWITTER_API_KEY"))
           (secret (get-environment-variable "TWITTER_API_KEY_SECRET"))
           (temp-cred #?=(twitter-authenticate-request key secret)))
      (set! (~ *twitter-creds* sess) temp-cred)
      `(container/
        (p (a (@ (href ,(twitter-authorize-url temp-cred))) "login"))
        ))))

(define (session-verify-auth await req)
  (let-params req ([verifier "q:oauth_verifier"]
                   [sess "c:sessionid"])
    (let* ((key (get-environment-variable "TWITTER_API_KEY"))
           (secret (get-environment-variable "TWITTER_API_KEY_SECRET"))
           (cred (and sess (~ *twitter-creds* sess))))
      (if cred
          (begin
            (let* ((cred #?=(twitter-authorize cred #?=verifier))
                   (acc (twitter-account:settings/json cred)))
              #?=acc
              #?=(assoc "screen_name" acc)
              `(container/
                (p "Done!")
                )))
          (error "No session")))))

(define *twitter-creds* (make-hash-table 'string=?))

(define twitter-authorize-url
  (oauth-authorize-constructor
   "https://api.twitter.com/oauth/authorize"))

(define *session-id* 0)
(define (new-session-id!)
  (inc! *session-id*)
  #"sess-,*session-id*")
