(define-module playlogic.play
  (use scheme.set)
  (use gauche.collection)
  (use rfc.json)

  (use dbi)
  (add-load-path "../gosh-modules/dbd-sqlite3" :relative)
  (use dbd.sqlite3)

  (add-load-path "." :relative)
  (use json-match)
  (use query)
  (use playlogic.editor)

  (export play-game!
          play-game/dialog!
          play-game-cont!
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

(define (render-page await data-id session-id session . content)
  (let ((loc (json-query session '("location"))))
    `((nav (@ (class "breadcrumb") (aria-label "breadcrumbs"))
           (ul
            (li (a (@ (href ,#`"/scenarios/,|data-id|"))
                   ,(fas-icon "home") (span "Back to Editor")))))
      (div (@ (class "container"))
           (h2 (@ (class "title is-2")) ,loc)
           (div (@ (class "columns"))
                (div (@ (class "column is-3"))
                     ,(show-game-state await data-id session-id session)
                     )
                (div (@ (class "column"))
                     ,content))))))

(define (play-game! await data-id session-id)
  (values
   "Playing!"
   (let ((session (hash-table-get *session-table* session-id #f)))
     (if session
         (render-page await data-id session-id session)
         (let ((new-session (make-session await data-id)))
           (hash-table-put! *session-table* (x->string session-id) new-session)
           (render-page await data-id session-id new-session)))
     )))

(define (render-dialog conv data-id)
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

    `(
                   (div (@ (class "columns is-vcentered"))
                        #;(div (@ (class "column is-2"))
                             (span (@ (class "tag is-info"))
                                   ,type))
                        (div (@ (class "column is-one-third"))
                             (h4 (@ (class "title is-4")) ,label))
                        (div (@ (class "column"))
                             (p (a (@ (href ,#"/scenarios/~|data-id|/locations/~loc"))
                                   ,loc)
                                ,(fas-icon "caret-right") ,trigger))
                        (div (@ (class "column is-1"))
                             (p (@ (class "has-text-grey"))
                                "0x" ,(number->string ord 16))))
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

                   ,@(render-lines lines))
))

(define (safe-assoc-vec name alist)
  (or (assoc name alist) (cons #f #())))

(define (render-line char text options)
  (define (render-option o)
    `(li ,(fas-icon "angle-right")
         ,(cdr (assoc "text" o)) " "
         ,@(intersperse " " (map (^f `(span (@ (class "tag is-primary")) ,f))
                                 (cdr (safe-assoc-vec "flags-required" o))))
         " "
         ,@(let ((jump (cdr (safe-assoc-vec "jump-to" o))))
            (if (> (vector-length jump) 0)
                `((span (@ (class "tag is-info"))
                        ,(fas-icon "arrow-circle-right") " "
                        ,(vector-ref jump 0)))
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

(define (render-lines lines)
  (reverse
   (fold (^[line rest]
           (let ((char (cdr (assoc "character" line)))
                 (text (cdr (assoc "text" line)))
                 (options (let ((opt (assoc "options" line)))
                            (if opt (cdr opt) ()))))
             (cons (render-line char text options) rest)))
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
          (render-dialog (car conv) data-id)
          "Dialog not found."))
    )

  (values
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
           (render-page await data-id session-id session (content)))))))

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

(define (show-game-state await data-id session-id session)
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
              ,#" ~new-location"))))

  (define (show-inspection dialog-id trigger)
    `(li
      (a (@ (href
             ,#"/scenarios/~|data-id|/play/~|session-id|/dialogs/~dialog-id"))
         ,(fas-icon "search") ,#" ~trigger")))

  (define (show-conversation dialog-id trigger)
    `(li
      (a (@ (href
             ,#"/scenarios/~|data-id|/play/~|session-id|/dialogs/~dialog-id"))
         ,(fas-icon "comment") ,#" ~trigger")))

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
              AND "fe" |.| "flag" IS NULL)))

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
                               ((inspection)
                                (show-inspection dialog-id trigger))
                               ((conversation)
                                (show-conversation dialog-id trigger))
                               (else #"~trigger ~type"))
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
