(use gauche.test)
(use text.tree)
(use util.relation)

(test-start "SQLite")

(use gauche.collection)
(use dbi)
(add-load-path "../gosh-modules/dbd-sqlite" :relative)
(use dbd.sqlite)

(load "./testlib/test-utils")

(test-section "dbd-sqlite")

(test-behavior "simple query"
               (^[]
                 (let ((conn (dbi-connect "dbi:sqlite:dummy.db")))
                   (assert-equal
                    '(#(1234))
                    (map id (dbi-do conn "SELECT 1234"))))))

(test-behavior "simple query 2"
               (^[]
                 (let ((conn (dbi-connect "dbi:sqlite:dummy.db")))
                   (assert-equal
                    '(#(1234 5678))
                   (map id (dbi-do conn "SELECT ?, ?" () 1234 5678))))))

(test-section "query")

;; SELECT d.dialog_id FROM dialogs d
;; LEFT OUTER JOIN (SELECT * FROM flags_required WHERE flag not IN ('能力：寅')) fr
;; ON d.dialog_id=fr.dialog_id
;; LEFT OUTER JOIN (SELECT * FROM flags_exclusive WHERE flag IN ('能力：寅')) fe
;; ON d.dialog_id=fe.dialog_id
;; WHERE d.location = '大端末B'
;; AND fr.flag IS NULL
;; AND fe.flag IS NULL;

#;(write `(SELECT "d" |.| "dialog_id" FROM "dialogs" "d"
         LEFT OUTER JOIN
         (SELECT * FROM "flags_required" WHERE "flag" NOT IN ("能力：寅")) "fr"
         ON "d" |.| "dialog_id" = "fr" |.| "dialog_id"
         LEFT OUTER JOIN
         (SELECT * FROM "flags_exclusive" WHERE "flag" IN ("能力：寅")) "fe"
         ON "d" |.| "dialog_id" = "fe" |.| "dialog_id"
         WHERE "d" |.| "location" = "大端末B"
         AND "fr" |.| "flag" IS NULL
         AND "fe" |.| "flag" IS NULL))

(load "./lib/query.scm")
(import query)

(test-module 'query)

(test-behavior "simple SELECT"
               (^[]
                 (let ((conn (dbi-connect "dbi:sqlite:dummy.db")))
                   (assert-equal
                    '("SELECT" " " "\"dialog_id\"" " " "FROM" " " "\"dialogs\"")
                    (build-query conn '(SELECT "dialog_id" FROM "dialogs"))
                    ))
                 ))

(test-behavior "subquery"
               (^[]
                 (let ((conn (dbi-connect "dbi:sqlite:dummy.db")))
                   (assert-equal
                    "(SELECT \"dialog_id\" FROM \"dialogs\")"
                    (tree->string
                     (build-query conn '((SELECT "dialog_id" FROM "dialogs"))))
                    ))
                 ))

(define (q str)
  (string-append "\"" str "\""))

(test-behavior "SELECT with a subquery"
               (^[]
                 (let ((conn (dbi-connect "dbi:sqlite:dummy.db")))
                   (assert-equal
                    (intersperse
                     " " `("SELECT" ,(q "d") "." ,(q "dialog_id")
                           "FROM" ,(q "dialogs") ,(q "d")
                           "LEFT" "OUTER" "JOIN"
                           ("(" ,(intersperse
                                  " " `("SELECT" "*" "FROM"
                                        ,(q "flags_required")
                                        "WHERE" ,(q "flag")
                                        "NOT" "IN"
                                        ("(" (,(q "能力：寅")) ")"))) ")")
                           ,(q "fr")))
                    (build-query
                     conn
                     '(SELECT "d" |.| "dialog_id" FROM "dialogs" "d"
                              LEFT OUTER JOIN
                              (SELECT * FROM "flags_required"
                                      WHERE "flag" NOT IN ("能力：寅")) "fr")
                     ))
                   )))

(test-behavior "empty list"
               (^[]
                 (let ((conn (dbi-connect "dbi:sqlite:dummy.db")))
                   (assert-equal
                    '(("(" ")"))
                    (build-query conn '(()))
                    ))
                 ))


(test-behavior "last_insert_rowid"
               (^[]
                 (let* ((conn (dbi-connect "dbi:sqlite:dummy.db"))
                        (_ (dbi-do conn
                                   (tree->string
                                    (build-query
                                     conn
                                     '(CREATE TABLE IF NOT EXISTS "lines"
                                              (id INTEGER PRIMARY KEY
                                                  |,| text TEXT))
                                     ))))
                        (query (tree->string
                           (build-query
                            conn
                            '(INSERT INTO "lines" ("text") VALUES (?)
                                     |;| SELECT last_insert_rowid()))))
                        (rset
                         (dbi-do
                          conn
                          query
                          ()
                          "hello")))
                   (let ((line-id (vector-ref (car
                                               (relation-rows rset)) 0)))
                     (assert-equal #t (number? line-id))))))

(test-end :exit-on-failure #t)
