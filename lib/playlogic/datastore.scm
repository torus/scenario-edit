(define-module playlogic.datastore
  ;; (use srfi-98)                         ; get-environment-variable

  ;; (use violet)
  ;; (use makiki)

  ;; (use sxml.tools)

  (use text.tree)

  (use dbi)
  (add-load-path "../gosh-modules/dbd-sqlite" :relative)
  (use dbd.sqlite)

  (add-load-path "." :relative)
  (use query)

  ;; (use playlogic.editor)
  ;; (use playlogic.play)
  ;; (use playlogic.session)

  (export *sqlite-conn*
          query*
          with-query-result
          with-query-result/tree
          with-query-result/tree*
          execute-query
          execute-query-tree
          create-tables))

(select-module playlogic.datastore)



;; SQLite

(define *sqlite-conn* #f)

(define (query* await query . params)
  (with-query-result/tree
   await (build-query *sqlite-conn* query) params
   (^[rset]
     (if (is-a? rset <relation>)
         (relation-rows rset)
         rset))))

(define (with-query-result await str args proc)
  (let ((rset (await (^[] (apply dbi-do *sqlite-conn* str '() args)))))
    (let ((result (proc rset)))
      (dbi-close rset)
      result)))

(define (with-query-result/tree await tree args proc)
  (with-query-result await (tree->string tree) args proc))

(define (with-query-result/tree* await tree args proc)
  (let ((query-str (tree->string (build-query *sqlite-conn* tree))))
    (with-query-result await query-str args proc)))

(define (execute-query str args)
  (guard (e [else (report-error e)])
         (apply dbi-do *sqlite-conn* str '() args)))

(define (execute-query-tree tree . args)
  (execute-query (tree->string tree) args))

(define (create-tables)
  (execute-query-tree '("CREATE TABLE IF NOT EXISTS scenarios ("
                        "  scenario_id INTEGER PRIMARY KEY"
                        ", title       TEXT NOT NULL"
                        ")"))
  (execute-query-tree '("CREATE TABLE IF NOT EXISTS dialogs ("
                        "  dialog_id   INTEGER PRIMARY KEY"
                        ", scenario_id INTEGER NOT NULL"
                        ", ord         INTEGER NOT NULL"
                        ", label       TEXT NOT NULL"
                        ", location    TEXT NOT NULL"
                        ", type        TEXT NOT NULL"
                        ", trigger     TEXT NOT NULL"
                        ")"))
  (execute-query-tree '("CREATE TABLE IF NOT EXISTS lines ("
                        "  line_id     INTEGER PRIMARY KEY"
                        ", dialog_id   INTEGER NOT NULL"
                        ", ord         INTEGER NOT NULL"
                        ", character   TEXT NOT NULL"
                        ", text        TEXT NOT NULL"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS flags_required ("
                        "  dialog_id   INTEGER NOT NULL"
                        ", flag        TEXT NOT NULL"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS flags_exclusive ("
                        "  dialog_id   INTEGER NOT NULL"
                        ", flag        TEXT NOT NULL"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS flags_set ("
                        "  dialog_id   INTEGER NOT NULL"
                        ", flag        TEXT NOT NULL"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS options ("
                        "  option_id   INTEGER PRIMARY KEY"
                        ", line_id     INTEGER NOT NULL"
                        ", ord         INTEGER NOT NULL"
                        ", text        TEXT NOT NULL"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS option_flags_required ("
                        "  option_id   INTEGER NOT NULL"
                        ", flag        TEXT NOT NULL"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS option_jumps ("
                        "  option_id   INTEGER NOT NULL"
                        ", destination TEXT NOT NULL"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS portals ("
                        "  dialog_id   INTEGER PRIMARY KEY"
                        ", destination INTEGER NOT NULL"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS ascii_names ("
                        "  original    TEXT PRIMARY KEY"
                        ", scenario_id INTEGER NOT NULL"
                        ", ascii       TEXT NOT NULL"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS sessions ("
                        "  session_id TEXT PRIMARY KEY"
                        ", user_id    TEXT NOT NULL"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS users ("
                        "  user_id      TEXT PRIMARY KEY"
                        ", display_name TEXT NOT NULL"
                        ")"))
  )
