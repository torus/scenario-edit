(define-module playlogic.play
  (use gauche.collection)
  (use rfc.json)

  (use dbi)
  (add-load-path "../gosh-modules/dbd-sqlite3" :relative)
  (use dbd.sqlite3)

  (add-load-path "." :relative)
  (use json-match)
  (use playlogic.editor)

  (export play-game!
   ))

(select-module playlogic.play)

(define *session-table* (make-hash-table))

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

(define (play-game! await data-id session-id)
  (values
   "Playing!"
   (let ((session (hash-table-get *session-table* session-id #f)))
     (if session
         (show-game-state session)
         (let ((new-session (make-session await data-id)))
           (hash-table-put! *session-table* session-id new-session)
           (show-game-state new-session))))))

(define (show-game-state session)
  (construct-json-string session)
  )
