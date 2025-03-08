(define-module playlogic.datastore
  (use text.tree)

  (use dbi)

  (use rheingau)
  (rheingau-use dbd.sqlite)

  (add-load-path "." :relative)
  (use dbi-query)

  (export create-tables
          query*
          *sqlite-conn*
          datastore-connect!))

(select-module playlogic.datastore)

;; SQLite

(define *sqlite-conn* #f)

(define (query* await query . params)
  (apply do-query await *sqlite-conn* query params))

(define (datastore-connect!)
  (let ((conn (dbi-connect "dbi:sqlite:scenario-sqlite3.db")))
    (set! *sqlite-conn* conn)
    (print "Sqlite connected")
    (flush)))

(define (execute-query str args)
  (guard (e [else (report-error e)])
         (apply dbi-do *sqlite-conn* str '() args)))

(define (execute-query-tree tree . args)
  (execute-query (tree->string tree) args))

(define (create-tables)
  (execute-query-tree '("CREATE TABLE IF NOT EXISTS scenarios ("
                        "  scenario_id INTEGER PRIMARY KEY"
                        ", title       TEXT NOT NULL"
                        ", language    TEXT NOT NULL"
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
                        "  original    TEXT"
                        ", scenario_id INTEGER NOT NULL"
                        ", ascii       TEXT NOT NULL"
                        ", UNIQUE(original, scenario_id)"
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
