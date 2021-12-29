(define-module playlogic
  (use violet)
  (use makiki)

  (use dbi)
  (add-load-path "../gosh-modules/dbd-sqlite" :relative)
  (use dbd.sqlite)

  (add-load-path "." :relative)
  (use playlogic.editor)
  (use playlogic.play)

  (export playlogic-start!
   ))

(select-module playlogic)

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

  (let ((conn (dbi-connect "dbi:sqlite:scenario-sqlite3.db")))
    (set! *sqlite-conn* conn)
    (print "Sqlite connected")
    (flush)))

;;;;;;;;;;;;;;;;;;;;;;
