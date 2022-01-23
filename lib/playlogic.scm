(define-module playlogic
  (use srfi-98)                         ; get-environment-variable

  (use violet)
  (use makiki)

  (use sxml.tools)

  (use dbi)
  (add-load-path "../gosh-modules/dbd-sqlite" :relative)
  (use dbd.sqlite)

  (add-load-path "../gosh-modules/net-twitter" :relative)
  (add-load-path "../gosh-modules/net-twitter/Gauche-net-oauth" :relative)

  (use net.oauth)
  (use net.twitter)
  (use net.twitter.account :prefix twitter-account:)
  (use net.twitter.auth)

  (add-load-path "." :relative)
  (use playlogic.editor)
  (use playlogic.play)

  (export playlogic-start!
   ))

(select-module playlogic)

(define-syntax ok
  (syntax-rules ()
    [(_ req title elements ...)
     (guard (e [else (report-error e)
                     (respond/ng req 500 :body (create-error-page e))])
            (respond/ok req (cons "<!DOCTYPE html>"
                                  (sxml:sxml->html
                                   (create-page/title title elements ...)))))]))
(define-syntax ok*
  (syntax-rules ()
    [(_ req elements)
     (guard (e [else (report-error e)
                     (respond/ng req 500 :body (create-error-page e))])
            (respond/ok req (cons "<!DOCTYPE html>"
                                  (sxml:sxml->html
                                   (apply create-page/title elements)))))]))

(define (create-error-page e)
  (cons "<!DOCTYPE html>"
        (sxml:sxml->html
         (create-page/title
          "ERROR!"
          `((div (@ (class "container"))
                 (h1 (@ (class "title"))
                     "Something went wrong " ,(fas-icon "sad-tear"))
                 (pre ,(report-error e #f))))))))

(define (playlogic-start!)
  (define-http-handler "/"
    (^[req app]
      (violet-async
       (^[await]
         (ok req "PlayLogic Scenario Editor"
             `(container/
               (ul (li (a (@ (href "/scenarios/1")) "Scenario #1"))
                   (li (a (@ (href "/scenarios/2")) "Scenario #2"))
                   (li (a (@ (href "/admin/setup")) "Setup")))))))))

  (define-http-handler #/^\/scenarios\/(\d+)$/
    (^[req app]
      (violet-async
       (^[await]
         (let-params req ([id "p:1"])
                     (let ((rendered (read-and-render-scenario-file await id)))
                       (ok req #"Scenario #~id"
                           (scenario-page-header await id)
                           rendered)))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/insert\/(.*)/
    (^[req app]
      (violet-async
       (^[await]
         (let-params req ([id "p:1"]
                          [ord "p:2" :convert (cut string->number <> 16)])
                     (let ((rendered
                            (read-and-render-scenario-file/insert await id ord)))
                       (ok req "会話を追加"
                           (scenario-page-header await id)
                           rendered)))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/edit\/(.*)/
    (^[req app]
      (violet-async
       (^[await]
         (let-params req ([id "p:1"]
                          [label "p:2"])
                     (let ((rendered
                            (read-and-render-scenario-file/edit await id label)))
                       (ok req "会話を編集"
                           (scenario-page-header await id)
                           rendered)))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/update-csv/
    (^[req app]
      (violet-async
       (^[await]
         (let-params req ([data-id "p:1"])
                     (ok req "データを更新"
                         `(p (span ,(convert-json-to-csv await data-id))
                             " "
                             (span (a (@ (href ,#"/scenarios/~|data-id|"))
                                      "Back to Scenario"))))
                     )))))

  (define-http-handler #/^\/scenarios\/(\d+)\/submit\/(.*)/
    (with-post-parameters
     (^[req app]
       (violet-async
        (^[await]
          (let-params req ([id "p:1"]
                           [label "p:2"]
                           [json "q"])
                      (let ((result (update-with-json await id json)))
                        (respond/redirect
                         req #"/scenarios/~|id|#label-~label"))))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/delete/
    (with-post-parameters
     (^[req app]
       (violet-async
        (^[await]
          (let-params req ([id "p:1"]
                           [label "q"])
                      (let ((result (delete-existing-dialogs await id label)))
                        (respond/redirect req #"/scenarios/~|id|"))))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/locations\/([^\/]+)\/?$/
    (^[req app]
      (violet-async
       (^[await]
         (let-params req ([id "p:1"] [loc "p:2"])
                     (let ((rendered (render-location await id loc)))
                       (ok req #"場所：~loc"
                           (scenario-page-header await id)
                           rendered)))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/locations$/
    (^[req app]
      (violet-async
       (^[await]
         (let-params req ([id "p:1"])
                     (let ((rendered (render-location-list await id)))
                       (ok req "場所"
                           (scenario-page-header await id)
                           rendered)))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/update-ascii-name$/
    (with-post-parameters
     (^[req app]
       (violet-async
        (^[await]
          (let-params req ([id "p:1"]
                           [input-original "q"]
                           [input-ascii "q"])
                      (set-ascii-name await id input-original input-ascii)
                      (respond/redirect
                         req
                         #"/scenarios/~|id|/locations/~input-original")
                      ))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/location-graph$/
    (^[req app]
      (violet-async
       (^[await]
         (let-params req ([id "p:1"])
                     (ok* req (render-location-graph await id)))))))

  (define-http-handler #/^\/static\// (file-handler))

  (define-http-handler "/admin/setup"
    (^[req app]
      (violet-async
       (^[await]
         (await create-tables)
         (ok req "Setup" '(p "done")
             '(a (@ (href "/")) "Back Home"))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/convert$/
    (^[req app]
      (violet-async
       (^[await]
         (let-params req ([id "p:1"])
                     (let ((result (convert-scenario-file-to-relations await id)))
                       (ok req "データ変換"
                           result " " `(a (@ (href ,#"/scenarios/~id"))
                                          "Back to Scenario"))))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/play\/(\d+)$/
    (^[req app]
      (violet-async
       (^[await]
         (let-params req ([id "p:1"]
                          [session-id "p:2"])
                     (ok* req (play-game! await id session-id)))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/play\/(\d+)\/do\/(\d+)$/
    (^[req app]
      (violet-async
       (^[await]
         (let-params req ([id         "p:1"]
                          [session-id "p:2"]
                          [cont-id    "p:3"])
                     (play-game-cont! await id session-id cont-id)
                     (respond/redirect req #"/scenarios/~|id|/play/~session-id")))
       )))

  (define-http-handler #/^\/scenarios\/(\d+)\/play\/(\d+)\/dialogs\/(\d+)$/
    (^[req app]
      (violet-async
       (^[await]
         (let-params req ([id         "p:1"]
                          [session-id "p:2"]
                          [dialog-id  "p:3"])
                     (ok* req (play-game/dialog! await id session-id dialog-id))
                     )))))

  (define-http-handler #/^\/scenarios\/(\d+)\/play\/(\d+)\/session$/
    (^[req app]
      (violet-async
       (^[await]
         (let-params req ([id         "p:1"]
                          [session-id "p:2"])
                     (ok* req (play-show-session await id session-id))
                     )))))

  (define-http-handler #/^\/scenarios\/(\d+)\/play\/(\d+)\/session\/update$/
    (with-post-parameters
     (^[req app]
       (violet-async
        (^[await]
          (let-params req ([id         "p:1"]
                           [session-id "p:2"]
                           [session    "q"])
                      (play-update-session! await id session-id session)
                      (respond/redirect
                       req #"/scenarios/~|id|/play/~|session-id|/session")
                      ))))))

  (define-http-handler "/twitauth"
    (^[req app]
      (violet-async
       (^[await]
         (let-params req ([sess "c:sessionid" :default (new-session-id!)])
           (response-cookie-add! req "sessionid" sess)
           (ok req "Login with Twitter"
               (let* ((key (get-environment-variable "TWITTER_API_KEY"))
                      (secret (get-environment-variable "TWITTER_API_KEY_SECRET"))
                      (temp-cred #?=(twitter-authenticate-request key secret)))
                 (set! (~ *twitter-creds* sess) temp-cred)
                 `(container/
                   (p (a (@ (href ,(twitter-authorize-url temp-cred))) "login"))
                   ))))
         ))))

  (define-http-handler "/twcallback"
    (^[req app]
      (violet-async
       (^[await]
         (let-params req ([verifier "q:oauth_verifier"]
                          [sess "c:sessionid"])
           (ok req "Login with Twitter"
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
         ))))

;; http://localhost:2227/twcallback?oauth_token=dOrpuwAAAAABVaHdAAABfnuR3-I&oauth_verifier=7ZtYzHKtzeQiK1w1QM4K2qjjwqAicTMB

  (let ((conn (dbi-connect "dbi:sqlite:scenario-sqlite3.db")))
    (set! *sqlite-conn* conn)
    (print "Sqlite connected")
    (flush)))

(define *session-id* 0)
(define (new-session-id!)
  (inc! *session-id*)
  #"sess-,*session-id*")

(define *twitter-creds* (make-hash-table 'string=?))

(define twitter-authorize-url
  (oauth-authorize-constructor
   "https://api.twitter.com/oauth/authorize"))

;;;;;;;;;;;;;;;;;;;;;;
