(define-module playlogic.play
  (use scheme.set)
  (use gauche.collection)
  (use gauche.process)
  (use rfc.json)
  (use util.match)

  (use dbi)
  (add-load-path "../gosh-modules/dbd-sqlite" :relative)
  (use dbd.sqlite)

  (add-load-path "." :relative)
  (use json-match)
  (use query)
  (use playlogic.editor)

  (export play-game!
          play-game/dialog!
          play-game-cont!
          play-show-session
          play-update-session!
          ))

(select-module playlogic.play)

(define *session-table* (make-hash-table 'string=?))

(define (make-session await data-id)
  (define initial-loc #f)
  (with-query-result/tree
   await
   '("SELECT location FROM dialogs"
     " WHERE scenario_id = ? ORDER BY ord LIMIT 1")
   `(,data-id)
   (^[rset]
     (for-each
      (^[row]
        (await
         (^[]
           (let ((loc (vector-ref row 0)))
             (print #"location: ~loc")
             (set! initial-loc loc)
             ))))
      rset)
     ))
  (alist-copy `(("location" . ,initial-loc)
                ("flags" . #()))))

(define (play-page-header await data-id session-id)
  (navbar/
   await data-id
   (match
    (query* await '(SELECT "title" FROM "scenarios"
                           WHERE "scenario_id" = ?) data-id)
    ((#(title)) title)
    (() "Untitled Scenario"))
   `(div (@ (id "playlogic-navbar") (class "navbar-menu"))
         (div (@ (class "navbar-start"))
              (a (@ (class "navbar-item")
                    (href ,#`"/scenarios/,|data-id|"))
                 ,(fas-icon "home") (span "Back to Editor")))
         (div (@ (class "navbar-end"))
              (a (@ (class "navbar-item")
                    (href ,#`"/scenarios/,|data-id|/play/,|session-id|/session"))
                 ,(fas-icon "save") (span "ふっかつのじゅもん"))))))

(define (play-update-session! await data-id session-id session-string)
  (let ((session (parse-json-string session-string)))
    (hash-table-put! *session-table* (x->string session-id) session)))

(define (play-show-session await data-id session-id)
  (define session (hash-table-get *session-table* session-id #f))
  (define (json-string)
    (await
     (^[]
       (call-with-process-io
        "jq ." (^[iport oport]
                 (construct-json session oport)
                 (close-port oport)
                 (let ((formatted (port->string iport)))
                   formatted))))))

  (define (cancel-btn)
    `(div (@ (class "control"))
          (a (@ (class "button")
                (href ,#"/scenarios/~|data-id|/play/~session-id"))
             "キャンセル")))

  (define (session-form)
    `(form (@ (method "post")
              (action ,#"/scenarios/~|data-id|/play/~|session-id|/session/update"))
           (div (@ (class "field"))
                (textarea (@ (class "textarea")
                             (name  "session"))
                          ,(json-string)))
           (div (@ (class "field is-grouped"))
                ,(cancel-btn)
                (div (@ (class "control"))
                     (input (@ (type "submit")
                               (class "button is-primary")
                               (value "更新")))))))

  `("Session"
    ,(play-page-header await data-id session-id)
    ,(container/
      `(div (@ (class "columns"))
            (div (@ (class "column"))
                 (h2 (@ (class "title is-3")) "Current Session")
                 ,(if session
                      (session-form)
                      `((div (@ (class "notification is-danger"))
                             "No session found.")
                        ,(cancel-btn))))))))

(define (render-page await data-id session-id session cur-dialog-id . content)
  (let ((loc (json-query session '("location"))))
    `(,(play-page-header await data-id session-id)
      ,(container/
        `(div (@ (class "columns"))
              (div (@ (class "column is-3"))
                   (div (@ (class "box"))
                        (h2 (@ (class "title is-3")) ,loc))
                   ,(show-game-state await data-id session-id session
                                     cur-dialog-id))
              (div (@ (class "column"))
                   (div (@ (class "columns"))
                        (div (@ (class "column")) "")
                        (div (@ (class "column is-9")
                                (style "max-height: 800px"))
                             (img (@ (src ,(location-image-url
                                            await data-id loc)))))
                        (div (@ (class "column")) ""))
                   ,content
                   ))))))

(define (play-game! await data-id session-id)
  (cons
   "Playing!"
   (let ((session (hash-table-get *session-table* session-id #f)))
     (if session
         (render-page await data-id session-id session #f)
         (let ((new-session (make-session await data-id)))
           (hash-table-put! *session-table* (x->string session-id) new-session)
           (render-page await data-id session-id new-session #f)))
     )))

(define (render-dialog await conv data-id session-id)
  (define (get name)
    (cdr (assoc name conv)))

  (let ((label     (get "label"))
        (lines     (get "lines"))
        (loc       (get "location"))
        (type      (get "type"))
        (trigger   (get "trigger"))
        (ord       (get "ord"))
        (flags-req (get "flags-required"))
        (flags-exc (get "flags-exclusive"))
        (flags-set (get "flags-set")))

    `((div (@ (class "columns is-vcentered"))
           (div (@ (class "column is-6"))
                (h4 (@ (class "title is-4")) ,label))
           (div (@ (class "column"))
                (p (a (@ (href ,#"/scenarios/~|data-id|/locations/~loc"))
                      ,loc)
                   ,(fas-icon "caret-right") ,trigger)))
      (div (@ (class "columns"))
           (div (@ (class "column is-one-third"))
                ,(intersperse
                  " "
                  (map (^f `(span (@ (class "tag is-primary")) ,f))
                       flags-req)))
           (div (@ (class "column is-one-third"))
                ,(intersperse
                  " "
                  (map (^f `(span (@ (class "tag is-danger")) ,f))
                       flags-exc)))
           (div (@ (class "column is-one-third"))
                ,(intersperse
                  " "
                  (map (^f `(span (@ (class "tag is-info")) ,f))
                       flags-set))))

      ,@(render-lines await data-id session-id lines))
    ))

(define (safe-assoc-vec name alist)
  (or (assoc name alist) (cons #f #())))

(define (render-line await data-id session-id char text options)
  (define (render-jump label)
    (let ((dialog-id
           (with-query-result/tree
            await
            (build-query *sqlite-conn* `(SELECT "dialog_id" FROM "dialogs"
                                                WHERE "label" = ,label))
            `()
            (^[rset]
              (car (map (cut vector-ref <> 0) rset))))))

      `((a (@ (href
               ,#"/scenarios/~|data-id|/play/~|session-id|/dialogs/~dialog-id"))
           (span (@ (class "tag is-info"))
                 ,(fas-icon "arrow-circle-right") " "
                 ,label
                 )
           ))
      )
    )

  (define (render-option o)
    `(li ,(fas-icon "angle-right")
         ,(cdr (assoc "text" o)) " "
         ,@(intersperse " " (map (^f `(span (@ (class "tag is-primary")) ,f))
                                 (cdr (safe-assoc-vec "flags-required" o))))
         " "
         ,@(let ((jump (cdr (safe-assoc-vec "jump-to" o))))
             (if (> (vector-length jump) 0)
                 (render-jump (vector-ref jump 0))

                 ()))
         ))

  `(div (@ (class "columns"))
        (div (@ (class "column is-2 has-text-right"))
             ,char)
        (div (@ (class "column"))
             ,text
             ,(if (null? options)
                  ()
                  `(ul
                    ,@(map render-option
                           options))))))

(define (render-lines await data-id session-id lines)
  (reverse
   (fold (^[line rest]
           (let ((char (cdr (assoc "character" line)))
                 (text (cdr (assoc "text" line)))
                 (options (let ((opt (assoc "options" line)))
                            (if opt (cdr opt) ()))))
             (cons (render-line await data-id session-id char text options)
                   rest)))
         () lines)))

(define (play-game/dialog! await data-id session-id dialog-id)
  (define (get-new-flags)
    (set-union
     (with-query-result/tree
      await
      (build-query *sqlite-conn* `(SELECT "label" FROM "dialogs"
                                          WHERE "dialog_id" = ,dialog-id))
      `()
      (^[rset]
        (list->set string-comparator (map (cut vector-ref <> 0) rset))))

     (with-query-result/tree
      await
      (build-query *sqlite-conn* `(SELECT "flag" FROM "flags_set"
                                          WHERE "dialog_id" = ,dialog-id))
      `()
      (^[rset]
        (list->set string-comparator (map (cut vector-ref <> 0) rset))))))

  (define (content)
    (let ((conv (read-dialog-from-db await dialog-id)))
      (if (pair? conv)
          (render-dialog await (car conv) data-id session-id)
          "Dialog not found."))
    )

  (cons
   "Playing!"
   (let ((session (hash-table-get *session-table* session-id #f)))
     (if (not session)
         `(p "ERROR: no session. "
             (a (@ (href ,#"/scenarios/~|data-id|"))
                "Back to the Editor"))
         (let ((flags (list->set string-comparator
                                 (vector->list (json-query session '("flags"))))))
           (assoc-set! session "flags"
                       (coerce-to <vector>
                                  (set-union flags (get-new-flags))))
           (render-page await data-id session-id session dialog-id (content)))))))

(define (cont-table-add proc)
  (inc! *cont-id*)
  (hash-table-put! *cont-table* (x->string *cont-id*) proc)
  *cont-id*)

(define (get-location-for-portal await dialog-id)
  (let ((dest (with-query-result/tree
               await
               '("SELECT d.location"
                 " FROM dialogs d, portals p"
                 " WHERE p.dialog_id = ?"
                 "   AND d.trigger = p.destination"
                 " LIMIT 1")
               `(,dialog-id)
               (^[rset] (map (cut vector-ref <> 0) rset)))))
    (car dest)))

(define (read-dialog-from-db await dialog-id)
  (with-query-result/tree
   await
   '("SELECT dialog_id, label, location, type, ord, trigger"
     " FROM dialogs"
     " WHERE dialog_id = ?"
     " ORDER BY ord")
   `(,dialog-id)
   (^[rset]
     (map
      (^[row]
        (read-dialog-detail-from-db await row))
      rset))))

(define (show-game-state await data-id session-id session cur-dialog-id)
  (define (show-portal dialog-id trigger)
    (let* ((new-location (get-location-for-portal
                          await dialog-id))
           (cont-id
            (cont-table-add
             (^[]
               (hash-table-put!
                *session-table* session-id
                (assoc-set! session "location" new-location))))))
      `(li (a (@ (href ,#"/scenarios/~|data-id|/play/~|session-id|/do/~cont-id"))
              ,(fas-icon "walking")
              ,#" ~new-location へ"))))

  (define (show-option dialog-id trigger icon)
    `(li
      (a (@ (href
             ,#"/scenarios/~|data-id|/play/~|session-id|/dialogs/~dialog-id")
            ,@(if (string=? (x->string dialog-id) (x->string cur-dialog-id))
                  `((class "is-active"))
                  ())
            (trigger ,trigger))
         ,icon ,#" ~trigger")))

  (define (query loc flags)
    (build-query
     *sqlite-conn*
     `(SELECT "d" |.| "dialog_id"
              |,| "d" |.| "type"
              |,| "d" |.| "trigger"
              FROM "dialogs" "d"
              LEFT OUTER JOIN
              (SELECT * FROM "flags_required" WHERE "flag" NOT IN
                      ,(intersperse '|,| flags))
              "fr"
              ON "d" |.| "dialog_id" = "fr" |.| "dialog_id"
              LEFT OUTER JOIN
              (SELECT * FROM "flags_exclusive" WHERE "flag" IN
                      ,(intersperse '|,| flags))
              "fe"
              ON "d" |.| "dialog_id" = "fe" |.| "dialog_id"
              WHERE "d" |.| "location" = ,loc
              AND "fr" |.| "flag" IS NULL
              AND "fe" |.| "flag" IS NULL
              ORDER BY "type")))

  (define (show-options loc flags)
    (with-query-result/tree
     await
     (query loc (vector->list (json-query session '("flags"))))
     `()
     (^[rset]
       `(aside (@ (class "menu"))
               (p (@ (class "menu-label"))
                  "Options")
               (ul (@ (class "menu-list"))
                   ,@(map
                      (^[row]
                        (await
                         (^[]
                           (let ((dialog-id (vector-ref row 0))
                                 (type      (vector-ref row 1))
                                 (trigger   (vector-ref row 2)))
                             (case (string->symbol type)
                               ((portal)
                                (show-portal dialog-id trigger))
                               (else
                                (show-option dialog-id trigger
                                             (icon-for-type type))))
                             ))))
                      rset))
               (p (@ (class "menu-label")) "Flags")
               (ul (@ (class "menu-list"))
                   ,@(map (^f `(li ,f)) flags))))))

  (let ((loc (json-query session '("location")))
        (flags (json-query session '("flags"))))
    (show-options loc flags)))

(define *cont-id* 0)
(define *cont-table* (make-hash-table 'string=?))

(define (play-game-cont! await data-id session-id cont-id)
  (let ((proc (hash-table-get *cont-table* cont-id #f)))
    (hash-table-delete! *cont-table* cont-id)
    (if proc
        (proc)
        (print #"Continuation not found: ~cont-id"))
    ))
