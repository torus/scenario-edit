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
                (div (@ (class "column is-one-third"))
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

  (values
   "Playing!"
   (let ((session (hash-table-get *session-table* session-id #f)))
     (if (not session)
         `(p "ERROR: no session. "
             (a (@ (href ,#"/scenarios/~|data-id|"))
                "Back to the Editor"))
         (let ((flags (list->set string-comparator (vector->list (json-query session '("flags"))))))
           (assoc-set! session "flags"
                       (coerce-to <vector>
                                  (set-union flags (get-new-flags))))
           (render-page await data-id session-id session #"Dialog ~dialog-id"))))))

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
