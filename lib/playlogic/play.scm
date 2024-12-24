(define-module playlogic.play
  (use scheme.set)
  (use gauche.collection)
  (use gauche.process)
  (use rfc.json)
  (use util.match)

  (use dbi)

  (add-load-path "." :relative)
  (use json-match)
  (use bulma-utils)
  (use playlogic.editor)
  (use playlogic.datastore)

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
  (let ((rset
         (query* await
                 '(SELECT "location" FROM "dialogs"
                          WHERE "scenario_id" = ? ORDER BY "ord" LIMIT 1)
                 data-id)))
    (for-each
     (^[row]
       (await
        (^[]
          (let ((loc (vector-ref row 0)))
            (print #"location: ~loc")
            (set! initial-loc loc)
            ))))
     rset))
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
                 ,(fas-icon/ "home") (span "Back to Editor")))
         (div (@ (class "navbar-end"))
              (a (@ (class "navbar-item")
                    (href ,#`"/scenarios/,|data-id|/play/,|session-id|/session"))
                 ,(fas-icon/ "save") (span "ふっかつのじゅもん"))))))

(define (play-update-session! await data-id session-id session-string)
  (let ((session (parse-json-string session-string)))
    (hash-table-put! *session-table* (x->string session-id) session)))

(define (session-json-string await session)
  (await
   (^[]
     (call-with-process-io
      "jq ." (^[iport oport]
               (construct-json session oport)
               (close-port oport)
               (let ((formatted (port->string iport)))
                 formatted))))))

(define (get-session session-id)
  (hash-table-get *session-table* session-id #f))

(define (play-show-session await data-id session-id)
  (define session (get-session session-id))

  (define (cancel-btn)
    `(div (@ (class "control"))
          (a (@ (class "button")
                (href ,#"/scenarios/~|data-id|/play/~session-id"))
             "もどる")))

  (define (session-form)
    `(form (@ (method "post")
              (action ,#"/scenarios/~|data-id|/play/~|session-id|/session/update"))
           (div (@ (class "field"))
                (textarea (@ (class "textarea")
                             (name  "session"))
                          ,(session-json-string await session)))
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
                   ,content

		   (pre (@ (id "play-session-json"))
			,(session-json-string await (get-session session-id)))
                   ))))))

(define (play-game! await data-id session-id)
  (cons
   "Playing!"
   (let ((session (hash-table-get *session-table* session-id #f)))
     (if session
         (let ((content (render-empty-dialog await data-id session-id)))
           (render-page await data-id session-id session #f content))
         (let ((new-session (make-session await data-id)))
           (hash-table-put! *session-table* (x->string session-id) new-session)
           (let ((content (render-empty-dialog await data-id session-id)))
             (render-page await data-id session-id new-session #f content))))
     )))

(define (render-empty-dialog  await data-id session-id)
  (let* ((session (hash-table-get *session-table* session-id #f))
         (loc (json-query session '("location"))))
    (render-options await data-id session-id loc #f))
  )

(define (render-dialog await conv data-id session-id)
  (define (get name)
    (cdr (assoc name conv)))

  (define (last-line-has-no-jump-options? dialog-id)
    (let ((non-jumps
           (query* await
                   '(SELECT COUNT (*) FROM
                            "options" "o" |,| "lines" "l" |,| "dialogs" "d"
                            LEFT OUTER JOIN "option_jumps" "j"
                            ON "j" |.| "option_id" = "o" |.| "option_id"
                            WHERE "o" |.| "line_id" = "l" |.| "line_id"
                            AND "l" |.| "dialog_id" = "d" |.| "dialog_id"
                            AND "l" |.| "line_id" =
                            (SELECT "line_id" FROM "lines" "l" |,| "dialogs" "d"
                                    WHERE "l" |.| "dialog_id"
                                    = "d" |.| "dialog_id"
                                    AND "d" |.| "dialog_id" = ?
                                    ORDER BY "l" |.| "ord" DESC LIMIT 1)
                            AND "j" |.| "destination" IS NULL
                            )
                   dialog-id))
          (jumps
           (query* await
                   '(SELECT COUNT (*) FROM
                            "options" "o" |,| "lines" "l" |,| "dialogs" "d"
                            LEFT OUTER JOIN "option_jumps" "j"
                            ON "j" |.| "option_id" = "o" |.| "option_id"
                            WHERE "o" |.| "line_id" = "l" |.| "line_id"
                            AND "l" |.| "dialog_id" = "d" |.| "dialog_id"
                            AND "l" |.| "line_id" =
                            (SELECT "line_id" FROM "lines" "l" |,| "dialogs" "d"
                                    WHERE "l" |.| "dialog_id"
                                    = "d" |.| "dialog_id"
                                    AND "d" |.| "dialog_id" = ?
                                    ORDER BY "l" |.| "ord" DESC LIMIT 1)
                            AND "j" |.| "destination" IS NOT NULL
                            )
                   dialog-id)
           ))

      (and (> (vector-ref (car jumps) 0) 0)
           (= (vector-ref (car non-jumps) 0) 0))
      ))

  (let ((dialog-id (get "id"))
        (label     (get "label"))
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
                   ,(fas-icon/ "caret-right") ,trigger)))
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

      ,@(render-lines await data-id session-id lines)

      ,@(if (last-line-has-no-jump-options? dialog-id)
            ()
            (list (render-options await data-id session-id loc dialog-id))))
    ))

(define (render-options await data-id session-id loc cur-dialog-id)
  (define (get-data loc flags)
    (query* await
            `(SELECT "d" |.| "dialog_id"
                     |,| "d" |.| "type"
                     |,| "d" |.| "trigger"
                     |,| "d" |.| "label"
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
                     AND "d" |.| "scenario_id" = ,data-id
                     AND "fr" |.| "flag" IS NULL
                     AND "fe" |.| "flag" IS NULL
                     ORDER BY "type")))

  (define session (hash-table-get *session-table* session-id #f))

  (define (show-portal dialog-id trigger)
    (let* ((new-location (get-location-for-portal
                          await dialog-id))
           (cont-id
            (cont-table-add
             (^[]
               (hash-table-put!
                *session-table* session-id
                (assoc-set! session "location" new-location))
               #f))))
      `(a (@ (class "button")
             (href ,#"/scenarios/~|data-id|/play/~|session-id|/do/~cont-id"))
          ,(fas-icon/ "walking")
          (span ,#" ~new-location へ"))))

  (define (show-option dialog-id trigger icon label)
    `(a (@ (class "button is-primary")
           (href
            ,#"/scenarios/~|data-id|/play/~|session-id|/dialogs/~dialog-id")
           ,@(if (string=? (x->string dialog-id) (x->string cur-dialog-id))
                 `((class "is-active"))
                 ())
           (trigger ,trigger))
         ,icon (strong (@ (class "mr-2")) ,trigger) (span ,label)))

  (define (render-general-options)
    (let loop ((data (get-data loc (vector->list (json-query session '("flags")))))
               (options ())
               (moves ()))
      (if (null? data)
          `(,@(if (pair? options)
                  `((h3 (@ (class "title is-4")) "アクション")
                    (div (@ (class "columns"))
                         (div (@ (class "column"))
                              (p (@ (class "buttons"))
                                 ,@options))))
                  ())
            (,@(if (pair? moves)
                 `((h3 (@ (class "title is-4")) "移動")
                   (div (@ (class "columns"))
                        (div (@ (class "column"))
                             (p (@ (class "buttons"))
                                ,@moves))))
                 ())))
          (let ((row (car data)))
            (await
             (^[]
               (let ((dialog-id (vector-ref row 0))
                     (type      (vector-ref row 1))
                     (trigger   (vector-ref row 2))
                     (label     (vector-ref row 3)))
                 (case (string->symbol type)
                   ((portal)
                    (loop (cdr data)
                          options
                          (cons (show-portal dialog-id trigger) moves)))
                   (else
                    (loop (cdr data)
                          (cons
                           (show-option dialog-id trigger
                                        (icon-for-type type) label) options)
                          moves))))))))))

  (render-general-options))

(define (safe-assoc-vec name alist)
  (or (assoc name alist) (cons #f #())))

(define (render-line await data-id session-id char text options)
  (define (jump-url dialog-id)
    (let* ((new-location (get-location-for-dialog await dialog-id))
           (cont-id
            (cont-table-add
             (^[]
               (let ((session (hash-table-get *session-table* session-id #f)))
                 (hash-table-put!
                  *session-table* session-id
                  (assoc-set! session "location" new-location)))
               (cons 'dialog-id dialog-id)))))
      #"/scenarios/~|data-id|/play/~|session-id|/do/~cont-id"))

  (define (render-jump label)
    (let ((dialog-id
           (let ((dialogs (query* await
                                  `(SELECT "dialog_id" FROM "dialogs"
                                           WHERE "label" = ,label))))
             (if (null? dialogs)
                 #f
                 (car (map (cut vector-ref <> 0)
                           dialogs))))))
      (if dialog-id
          `((a (@ (href ,(jump-url dialog-id)))
               ,(fas-icon/ "arrow-circle-right")
               (span ,#" ~label")))
          `(,(fas-icon/ "arrow-circle-right") (span ,#" ~label")))))

  (define (render-option o)
    (let* ((jump (cdr (safe-assoc-vec "jump-to" o)))
           (has-jump? (> (vector-length jump) 0)))
      `(li ,(fas-icon/ "angle-right")
           ,(cdr (assoc "text" o)) " "
           ,@(intersperse " " (map (^f `(span (@ (class "tag is-primary")) ,f))
                                   (cdr (safe-assoc-vec "flags-required" o))))
           " "
           ,@(if has-jump?
                 (render-jump (vector-ref jump 0))
                 ()))))

  (define (render-text text)
    (define (render-payload event payload)
      (case event
        ((jump jumpBlack)
         (if (string? payload)
             (render-jump payload)
             "ERROR"))
        (else (render-json payload)
         #;`(code ,(construct-json-string payload))))
      )
    (let ((m (rxmatch #/^#ev:(\w+)(:(.*))?/ text)))
      (if m
          `((span (@ (class "tag is-primary")) (strong "#ev"))
            " "
            (code ,(m 1))
            ,(if (m 3)
                 `(" " ,(render-payload (string->symbol (m 1))
                                        (parse-json-string (m 3))))
                 ()))
          text)))

  `(div (@ (class "columns"))
        (div (@ (class "column is-2 has-text-right"))
             ,char)
        (div (@ (class "column"))
             ,(render-text text)
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
     (list->set string-comparator
                (map (cut vector-ref <> 0)
                     (query* await
                             `(SELECT "label" FROM "dialogs"
                                      WHERE "dialog_id" = ,dialog-id))))
     (list->set string-comparator
                (map (cut vector-ref <> 0)
                     (query* await
                             `(SELECT "flag" FROM "flags_set"
                                      WHERE "dialog_id" = ,dialog-id))))))

  (define (content)
    (let ((conv (read-dialog-from-db await dialog-id)))
      (if (pair? conv)
          (render-dialog await (car conv) data-id session-id)
          "Dialog not found.")))

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
  (let ((dest (let ((rset
                     (query* await
                             '(SELECT "d" |.| "location"
                                      FROM "dialogs" "d" |,| "portals" "p"
                                      WHERE "p" |.| "dialog_id" = ?
                                      AND "d" |.| "trigger" = "p" |.| "destination"
                                      LIMIT 1)
                             dialog-id)))
                (map (cut vector-ref <> 0) rset))))
    (car dest)))

(define (get-location-for-dialog await dialog-id)
  (let ((dest (let ((rset
                     (query* await
                             '(SELECT "location"
                                      FROM "dialogs"
                                      WHERE "dialog_id" = ?
                                      LIMIT 1)
                             dialog-id)))
                (map (cut vector-ref <> 0) rset))))
    (car dest)))

(define (read-dialog-from-db await dialog-id)
  (let ((rset
         (query*
          await
          '(SELECT "dialog_id" |,| "label" |,| "location"
                   |,| "type" |,| "ord" |,| "trigger"
                   FROM "dialogs"
                   WHERE "dialog_id" = ?
                   ORDER BY "ord")
          dialog-id)))
    (map
     (^[row]
       (read-dialog-detail-from-db/full await row))
     rset)))

(define (show-game-state await data-id session-id session cur-dialog-id)
  (define (show-portal dialog-id trigger)
    (let* ((new-location (get-location-for-portal
                          await dialog-id))
           (cont-id
            (cont-table-add
             (^[]
               (hash-table-put!
                *session-table* session-id
                (assoc-set! session "location" new-location))
               #f))))
      `(li (a (@ (href ,#"/scenarios/~|data-id|/play/~|session-id|/do/~cont-id"))
              ,(fas-icon/ "walking")
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

  (define (get-data loc flags)
    (query* await
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
                     AND "d" |.| "scenario_id" = ,data-id
                     AND "fr" |.| "flag" IS NULL
                     AND "fe" |.| "flag" IS NULL
                     ORDER BY "type")))

  (define (show-state loc flags)
    `(aside (@ (class "menu"))
            (p (@ (class "menu-label")) "Flags")
            (ul (@ (class "menu-list"))
                ,@(map (^f `(li ,f)) flags))))

  (let ((loc (json-query session '("location")))
        (flags (json-query session '("flags"))))
    (show-state loc flags)))

(define *cont-id* 0)
(define *cont-table* (make-hash-table 'string=?))

(define (play-game-cont! await data-id session-id cont-id)
  (let ((proc (hash-table-get *cont-table* cont-id #f)))
    (hash-table-delete! *cont-table* cont-id)
    (if proc
        (proc)
        (print #"Continuation not found: ~cont-id"))
    ))
