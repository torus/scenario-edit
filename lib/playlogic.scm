(define-module playlogic
  (use srfi-98)                         ; get-environment-variable

  (use violet)
  (use makiki)

  (use data.random)
  (use file.util)
  (use sxml.tools)

  (add-load-path "." :relative)
  (use playlogic.editor)
  (use playlogic.play)
  (use playlogic.datastore)

  (add-load-path "./playlogic" :relative)
  (use bulma-utils)

  (export playlogic-start!))

(select-module playlogic)

(define (ok req title . elements)
  (respond/ok req (cons "<!DOCTYPE html>"
                        (sxml:sxml->html
                         (apply create-page/title title elements)))))

(define (ok* req title&elements)
  (respond/ok req (cons "<!DOCTYPE html>"
                        (sxml:sxml->html
                         (apply create-page/title title&elements)))))

(define (ok/bare req element)
  (respond/ok req element))

(define (create-error-page e)
  (cons "<!DOCTYPE html>"
        (sxml:sxml->html
         (create-page/title
          "ERROR!"
          `((div (@ (class "container"))
                 (h1 (@ (class "title"))
                     "Something went wrong " ,(fas-icon/ "sad-tear"))
                 (pre ,(report-error e #f))))))))

(define (handle-request proc)
  (handle-request/no-auth proc))

(define (handle-request/no-auth proc)
  (^[req app]
    (violet-async
     (^[await]
       (guard (e [else (report-error e)
                       (respond/ng req 500 :body (create-error-page e))])
              (proc await req app))))))

(define (handle-request/post proc)
  (with-post-parameters
   (handle-request proc)))

(define (playlogic-start!)
  (define-http-handler "/"
    (handle-request
     (^[await req app]
       (ok req "PlayLogic Scenario Editor"
           (container/
            `(ul (li (a (@ (href "/scenarios/1")) "Scenario #1"))
                 (li (a (@ (href "/scenarios/2")) "Scenario #2"))
                 (li (a (@ (href "/scenarios/3")) "Scenario #3"))
                 (li (a (@ (href "/admin/setup")) "Setup"))))))))

  (define-http-handler #/^\/scenarios\/(\d+)$/
    (handle-request
     (^[await req app]
       (let-params req ([id "p:1"])
         (let ((rendered (read-and-render-scenario-file await id)))
           (ok req #"Scenario #~id"
               (scenario-page-header await id)
               rendered))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/insert\/(.*)/
    (handle-request
     (^[await req app]
       (let-params req ([id "p:1"]
                        [ord "p:2" :convert (cut string->number <> 16)])
         (let ((rendered
                (read-and-render-scenario-file/insert await id ord)))
           (ok req "会話を追加"
               (scenario-page-header await id)
               rendered))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/edit\/(.*)/
    (handle-request
     (^[await req app]
       (let-params req ([id "p:1"]
                        [label "p:2"])
         (let ((rendered
                (read-and-render-scenario-file/edit await id label)))
           (ok req "会話を編集"
               (scenario-page-header await id)
               rendered))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/view\/$/
    (handle-request
     (^[await req app]
       (let-params req ([id "p:1"])
         (let ((rendered
                (read-and-render-scenario-file/view await id)))
           (ok req "会話"
               (scenario-page-header await id)
               rendered))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/update-csv/
    (handle-request
     (^[await req app]
       (let-params req ([data-id "p:1"])
         (ok req "データを更新"
             `(p (span ,(convert-json-to-csv await data-id))
                 " "
                 (span (a (@ (href ,#"/scenarios/~|data-id|"))
                          "Back to Scenario"))))
         ))))

  (define-http-handler (POST) #/^\/scenarios\/(\d+)\/submit\/(.*)/
    (handle-request/post
     (^[await req app]
       (let-params req ([id "p:1"]
                        [label "p:2"]
                        [json "q"])
         (let ((current-label (update-with-json await id json)))
           (respond/redirect
            req #"/scenarios/~|id|#label-~(escape-label current-label)"))))))

  (define-http-handler (POST) #/^\/scenarios\/(\d+)\/delete/
    (handle-request/post
     (^[await req app]
       (let-params req ([id "p:1"]
                        [label "q"])
         (let ((result (delete-existing-dialogs await id label)))
           (respond/redirect req #"/scenarios/~|id|"))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/locations\/([^\/]+)\/?$/
    (handle-request
     (^[await req app]
       (let-params req ([id "p:1"] [loc "p:2"])
         (let ((rendered (render-location await id loc)))
           (ok req #"場所：~loc"
               (scenario-page-header await id)
               rendered))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/locations$/
    (handle-request
     (^[await req app]
       (let-params req ([id "p:1"])
         (let ((rendered (render-location-list await id)))
           (ok req "場所"
               (scenario-page-header await id)
               rendered))))))

  (define-http-handler (POST) #/^\/scenarios\/(\d+)\/update-ascii-name$/
    (handle-request/post
     (^[await req app]
       (let-params req ([id "p:1"]
                        [input-original "q"]
                        [input-ascii "q"])
         (set-ascii-name await id input-original input-ascii)
         (respond/redirect
          req #"/scenarios/~|id|/locations/~input-original")))))

  (define-http-handler #/^\/scenarios\/(\d+)\/location-graph$/
    (handle-request
     (^[await req app]
       (let-params req ([id "p:1"])
         (ok* req (render-location-graph await id))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/dialog-details\/(\d+)$/
    (handle-request
     (^[await req app]
       (let-params req ([id "p:1"]
                        [dialog-id "p:2" :convert x->integer])
                   (ok/bare req
                            (sxml:sxml->html
                             (render-dialog-detail-by-dialog-id
                              await id dialog-id))
                            )))))

  (define-http-handler #/^\/static\/gameassets\/.*\.jpg$/
    (handle-request/no-auth
     (^[await req app]
       (let* ((root (document-root))
              (path (request-path req))
              (full-path (sys-normalize-pathname #"~|root|~|path|")))
         (await
          (^[]
            (if (file-is-readable? full-path)
                (respond/ok req `(file ,full-path))
                (respond/ok req `(file ,"static/404.jpg")))))))))

  (define-http-handler #/^\/static\//
    (let ((proc (file-handler :directory-index '("index.html"))))
      (handle-request/no-auth
       (^[await req app]
         (await (cut proc req app))))))

  (define-http-handler "/admin/setup"
    (handle-request
     (^[await req app]
       (await create-tables)
       (ok req "Setup" '(p "done")
           '(a (@ (href "/")) "Back Home")))))

  (define-http-handler #/^\/scenarios\/(\d+)\/convert$/
    (handle-request
     (^[await req app]
       (let-params req ([id "p:1"])
         (ok req "データ変換"
             (convert-scenario-file-to-relations await id)
             " " `(a (@ (href ,#"/scenarios/~id"))
                     "Back to Scenario"))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/play\/(\d+)$/
    (handle-request
     (^[await req app]
       (let-params req ([id "p:1"]
                        [session-id "p:2"])
         (ok* req (play-game! await id session-id))))))

  (define-http-handler #/^\/scenarios\/(\d+)\/play\/(\d+)\/do\/(\d+)$/
    (handle-request
     (^[await req app]
       (let-params req ([id         "p:1"]
                        [session-id "p:2"]
                        [cont-id    "p:3"])
         (let ((result (play-game-cont! await id session-id cont-id)))
           (if (and (pair? result) (eq? 'dialog-id (car result)))
               (let ((dialog-id (cdr result)))
                 (respond/redirect req #"/scenarios/~|id|/play/~|session-id|/dialogs/~dialog-id"))
               (respond/redirect req #"/scenarios/~|id|/play/~session-id"))
           )))))

  (define-http-handler #/^\/scenarios\/(\d+)\/play\/(\d+)\/dialogs\/(\d+)$/
    (handle-request
     (^[await req app]
       (let-params req ([id         "p:1"]
                        [session-id "p:2"]
                        [dialog-id  "p:3"])
         (ok* req (play-game/dialog! await id session-id dialog-id))
         ))))

  (define-http-handler #/^\/scenarios\/(\d+)\/play\/(\d+)\/session$/
    (handle-request
     (^[await req app]
       (let-params req ([id         "p:1"]
                        [session-id "p:2"])
         (ok* req (play-show-session await id session-id))
         ))))

  (define-http-handler (POST) #/^\/scenarios\/(\d+)\/play\/(\d+)\/session\/update$/
    (handle-request/post
     (^[await req app]
       (let-params req ([id         "p:1"]
                        [session-id "p:2"]
                        [session    "q"])
         (play-update-session! await id session-id session)
         (respond/redirect
          req #"/scenarios/~|id|/play/~|session-id|/session")
         ))))

  (set! (random-data-seed) (sys-time))
  (datastore-connect!))


;;;;;;;;;;;;;;;;;;;;;;
