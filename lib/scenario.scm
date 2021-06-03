(define-module scenario
  (use gauche.threads)
  (use gauche.collection)

  (use file.util)
  (use rfc.http)
  (use rfc.json)
  (use sxml.tools)
  (use text.csv)
  (use text.tree)

  (use violet)
  (use makiki)

  (use dbi)
  (add-load-path "../gosh-modules/dbd-sqlite3" :relative)
  (use dbd.sqlite3)

  (add-load-path "." :relative)
  (use json-match)

  (export scenario-start!
   ))

(select-module scenario)

;;
;; Application
;;

(define (create-page . children)
  `(html
    (@ (lang "en"))
    (head
     (meta (@ (charset "utf-8")))
     (meta (@ (name "viewport") (content "width=device-width, initial-scale=1")))
     (title "Scenario Edit")
     (link (@ (rel "stylesheet")
              (href "https://cdn.jsdelivr.net/npm/bulma@0.9.0/css/bulma.min.css")))
     (script (@ (src "https://kit.fontawesome.com/515fd4f349.js")
                (crossorigin "anonymous")) ""))
    (body
     ,@children
     (script (@ (src "/static/script.js")) "")))
  )

(define (ok req . elements)
  (respond/ok req (cons "<!DOCTYPE html>"
                        (sxml:sxml->html
                         (apply create-page elements)))))

(define-http-handler "/"
  (^[req app]
    (violet-async
     (^[await]
       (ok req `(ul (li (a (@ (href "/scenarios/1")) "Scenario #1"))
                    (li (a (@ (href "/admin/setup")) "Setup"))))))))

(define (fas-icon name)
  `(span (@ (class "icon"))
         (i (@ (class ,#"fas fa-~name")) "")))

(define (render-line char text options)
  `(div (@ (class "columns"))
        (div (@ (class "column is-one-fifth has-text-right"))
             ,char)
        (div (@ (class "column"))
             ,text
             ,(if (null? options)
                  ()
                  `(ul
                    ,@(map (^o `(li ,(fas-icon "angle-right")
                                    ,(cdr (assoc "text" o))))
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

(define (edit-buttons data-id label)
  `((div (@ (class "column is-1"))
         (form (@ (method "post")
                  (action ,#"/scenarios/~|data-id|/delete"))
               (input (@ (type "hidden")
                         (name "label")
                         (value ,label)))
               (button (@ (class "button is-danger"))
                       ,(fas-icon "trash-alt"))))
    (div (@ (class "column is-1"))
         (a (@ (class "button")
               (href ,#"/scenarios/~|data-id|/edit/~|label|#form"))
            ,(fas-icon "edit")))))

(define (render-conversation conv data-id . additioanl-elements)
  (define (get name)
    (cdr (assoc name conv)))

  (let ((label (get "label"))
        (lines (get "lines"))
        (loc (get "location"))
        (ord (get "ord")))
    `(section (@ (class "section") (id ,#"label-~label"))
              (div (@ (class "container"))
                   (div (@ (class "columns is-vcentered"))
                        ,@additioanl-elements
                        (div (@ (class "column is-2"))
                             (span (@ (class "tag is-info"))
                                   ,(get "type")))
                        (div (@ (class "column is-one-third"))
                             (h4 (@ (class "title is-4")) ,label))
                        (div (@ (class "column"))
                             (p (a (@ (href ,#"/scenarios/~|data-id|/locations/~loc"))
                                   ,loc)))
                        (div (@ (class "column"))
                             (p (@ (class "has-text-grey"))
                                "0x" ,(number->string ord 16))))
                   ,@(render-lines lines)))))

(define (render-conversation/edit-button conv data-id)
  (render-conversation conv data-id
                       (edit-buttons data-id (cdr (assoc "label" conv)))))

(define (not-empty? str) (and str (not (zero? (string-length str)))))

(define (form-field label help input)
  `(div (@ (class "field"))
        ,(if (not-empty? label) `(label (@ (class "label")) ,label) ())
        (div (@ (class "control")) ,input)
        ,(if (not-empty? help) `(p (@ (class "help")) ,help) ())))

(define (render-option-form option)
  `(div (@ (class "columns is-vcentered"))
        (div (@ (class "column is-one-fifth has-text-right"))
             ,(fas-icon "angle-right"))
        (div (@ (class "column"))
             (input (@ (class "input option-input") (type "text")
                       (placeholder "選択肢")
                       (value ,option))))))

(define (render-line-form char text options)
  `((div (@ (class "line-form"))
         (div (@ (class "columns"))
              (div (@ (class "column is-one-fifth"))
                   (input (@ (class "input character-input")
                             (type "text")
                             (placeholder "キャラクター")
                             (value ,char))))
              (div (@ (class "column"))
                   (input (@ (class "input line-input") (type "text") (placeholder "セリフ")
                             (value ,text)))))

         (div (@ (class "option-fields"))
              ""
              ,@(reverse
                 (fold (^[option rest]
                         (cons
                          (render-option-form option)
                          rest))
                       () options)))

         (div (@ (class "columns"))
              (div (@ (class "column"))
                   (div (@ (class "field has-text-centered"))
                        (a (@ (class "button add-option-button"))
                           ,(fas-icon "angle-right")
                           (span (@ (style "margin-left: 0.5ex"))"選択肢を追加"))))))))

(define (render-conversation-form conv data-id hidden-inputs)
  (define (get name default)
    (if conv
        (cdr (assoc name conv))
        default))
  (define (value-and-selected selected type)
    (if (string=? type selected)
        `((value ,type) (selected "selected"))
        `((value ,type))))

  (define label (get "label" ""))
  (define lines (get "lines" #()))
  (define type (get "type" "conversation"))

  (define (conv-form)
    `(div (@ (class "columns"))
          (div (@ (class "column is-2"))
               ,(form-field
                 "タイプ" #f
                 `(div (@ (class "select"))
                       (select (@ (id "type-input"))
                               (option (@ ,@(value-and-selected type "conversation")) "会話")
                               (option (@ ,@(value-and-selected type "message")) "メッセージ")))))
          (div (@ (class "column is-one-third"))
               ,(form-field "ラベル"
                            "他の会話とラベルが重複しないようにしてください。"
                            `(input (@ (class "input")
                                       (id "label-input")
                                       (type "text")
                                       (placeholder "会話 ID")
                                       (value ,label)))))
          (div (@ (class "column"))
               ,(form-field "場所" #f
                            `(input (@ (class "input") (type "text")
                                       (id "location-input")
                                       (placeholder "場所")
                                       (value ,(get "location" ""))))))))

  (define (line-fields lines)
    (reverse
     (fold (^[line rest]
             (let ((char (cdr (assoc "character" line)))
                   (text (cdr (assoc "text" line)))
                   (options (let ((opt (assoc "options" line)))
                              (if opt
				  (map (^o (cdr (assoc "text" o)))(cdr opt))
				  ()))))
               (append (render-line-form char text options) rest)))
           () lines)))

  `(section (@ (class "section")
               (id "form"))
            (div (@ (class "container"))
                 (form (@ (id "edit-form")
                          (action ,#"/scenarios/~|data-id|/submit/~label")
                          (method "post"))
                       ,hidden-inputs
                       ,(conv-form)
                       (div (@ (id "line-fields"))
                            ""
                            ,@(line-fields lines))

                       (div (@ (class "columns"))
                            (div (@ (class "column"))
                                 (div (@ (class "field has-text-centered"))
                                      (a (@ (class "button")
                                            (id "add-line-button"))
                                         ,(fas-icon "comment")
                                         (span (@ (style "margin-left: 0.5ex"))"セリフを追加")))))

                       (div (@ (class "columns"))
                            (div (@ (class "column"))
                                 (div (@ (class "field is-grouped is-grouped-right"))
                                      (p (@ (class "control"))
                                         (button (@ (class "button is-primary")) "更新"))
                                      (p (@ (class "control"))
                                         (a (@ (class "button is-light")
                                               (href ,#"/scenarios/~data-id"))
                                            "キャンセル")))))))
            (div (@ (style "display: none")
                    (id "hidden-field"))
                 ,@(render-line-form "" "" ()))
            (div (@ (style "display: none")
                    (id "hidden-option-field"))
                 ,@(render-option-form ""))))

(define (add-conversation-button data-id prev-label ord)
  (let ((ord-hex (number->string ord 16)))
    `(div (@ (class "columns"))
          (div (@ (class "column has-text-centered"))
               (a (@ (class "button")
                     (href ,#"/scenarios/~|data-id|/insert/~|ord-hex|#form"))
                  ,(fas-icon "comments")
                  (span (@ (style "margin-left: 0.5ex"))"会話を追加"))))))

(define (read-scenario-file await id)
  (await (^[] (with-input-from-file (json-file-path id) parse-json))))

(define (read-scenario-from-db await id)
  (with-query-result/tree
   await
   '("SELECT dialog_id, label, location, type, ord"
     " FROM dialogs"
     " WHERE scenario_id = ?"
     " ORDER BY ord")
   `(,id)
   (^[rset]
     (map-to <vector>
      (^[row]
        (let ((dialog-id (vector-ref row 0))
              (label (vector-ref row 1))
              (loc (vector-ref row 2))
              (typ (vector-ref row 3))
              (ord (vector-ref row 4)))
          `(("id" . ,dialog-id)
            ("label" . ,label)
            ("type" . ,typ)
            ("location" . ,loc)
            ("ord" . ,ord)
            ("lines" .
             ,(with-query-result/tree
               await
               '("SELECT line_id, character, text, ord"
                 " FROM lines"
                 " WHERE dialog_id = ?"
                 " ORDER by ord")
               `(,dialog-id)
               (^[rset]
                 (map-to <vector>
                  (^[row]
                    (let ((line-id (vector-ref row 0))
                          (char (vector-ref row 1))
                          (text (vector-ref row 2))
                          (ord (vector-ref row 3)))
                      `(("id" . ,line-id)
                        ("character" . ,char)
                        ("ord" . ,ord)
                        ("text" . ,text)
                        ("options" .
                         ,(with-query-result/tree
                           await
                           '("SELECT text, ord FROM options"
                             " WHERE line_id = ? ORDER BY ord")
                           `(,line-id)
                           (^[rset]
                             (map-to <vector>
                              (^[row]
                                (let ((text (vector-ref row 0))
                                      (ord (vector-ref row 1)))
                                  `(("text" . ,text)
                                    ("ord" . ,ord))
                                ))
                              rset)))))))
                  rset)))))))
      rset))))

(define (read-and-render-scenario-file await id)
  (define (label-of conv)
    (cdr (assoc "label" conv)))
  (define (ord-of conv)
    (cdr (assoc "ord" conv)))
  (define (elems content)
    (fold2 (^[conv rest prev-conv]
             (let ((prev-label (if prev-conv (label-of prev-conv) ""))
                   (ord (if prev-conv
                            (/ (+ (ord-of prev-conv) (ord-of conv)) 2)
                            (- (ord-of conv) 1024))))
               (values (cons (render-conversation/edit-button conv id)
                             (cons (add-conversation-button id prev-label ord)
                                   rest))
                       conv)))
           () #f
           content))
  (let ((content (read-scenario-from-db await id)))
    (let-values (((convs prev-conv)
                  (elems content)))
      (append (reverse convs)
              (list (add-conversation-button id (label-of prev-conv)
                                             (+ 1024 (ord-of prev-conv))))))))

(define (json-file-path data-id)
  #"json/~|data-id|.json")

(define (read-and-render-scenario-file/insert await id ord)
  (define (new-form ord)
    (render-conversation-form #f id
                              `(input (@ (id "ord-input")
                                         (type "hidden")
                                         (value ,ord)))))
  (let ((content (read-scenario-from-db await id)))
          (reverse
           (fold2 (^[conv rest inserted?]
                   (let ((next-ord (cdr (assoc "ord" conv))))
                     (if (and (not inserted?) (> next-ord ord))
                         (values (cons (render-conversation conv id)
                                       (cons (new-form ord) rest))
                                 #t)
                         (values (cons (render-conversation conv id) rest)
                                 inserted?))))
                  () #f
                  content))))

(define (read-and-render-scenario-file/edit await id label-to-edit)
  (define (new-form conv label)
    (render-conversation-form conv id
                              `(input (@ (id "original-label-input")
                                         (type "hidden")
                                         (value ,label)))))

  (let ((content (read-scenario-from-db await id)))
          (reverse
           (fold (^[conv rest]
                   (let ((label (cdr (assoc "label" conv))))
                     (cons (if (string=? label label-to-edit)
                               (new-form conv label)
                               (render-conversation conv id))
                           rest)))
                 ()
                 content))))

(define (scenario-page-header await id)
  `(p (a (@ (class "button is-danger") (href ,#`"/scenarios/,|id|/convert"))
         ,(fas-icon "skull-crossbones") (span "Convert from JSON"))
      " "
      (a (@ (class "button") (href ,#`"/scenarios/,|id|/update-csv"))
         ,(fas-icon "save") (span "Update CSV/JSON"))))

(define-http-handler #/^\/scenarios\/(\d+)$/
  (^[req app]
    (violet-async
     (^[await]
       (let-params req ([id "p:1"])
                   (let ((rendered (read-and-render-scenario-file await id)))
                     (ok req (scenario-page-header await id) rendered)))))))

(define-http-handler #/^\/scenarios\/(\d+)\/insert\/(.*)/
  (^[req app]
    (violet-async
     (^[await]
       (let-params req ([id "p:1"]
                        [ord "p:2" :convert (cut string->number <> 16)])
                   (let ((rendered
                          (read-and-render-scenario-file/insert await id ord)))
                     (ok req rendered)))))))

(define-http-handler #/^\/scenarios\/(\d+)\/edit\/(.*)/
  (^[req app]
    (violet-async
     (^[await]
       (let-params req ([id "p:1"]
                        [label "p:2"])
                   (let ((rendered
                          (read-and-render-scenario-file/edit await id label)))
                     (ok req rendered)))))))

(define (overwrite-json-file await json filename)
  (await (^[]
           (let ((new-json json))
             (call-with-temporary-file
              (^[port tmpfile]
                (with-output-to-port port
                  (^[]
                    (guard (e [else (report-error e)])
                           (construct-json new-json))
                    (flush)))
                (sys-system #"jq . < ~tmpfile > ~filename"))
              :directory "json")
             'done
             ))))

(define (update-existing-conversation await data-id form-data)
  (define (get name)
    (let ((val (assoc name form-data)))
      (if val
          (cdr val)
          (error #"paramter not specified: ~|name|"))))

  (let ((orig-label (get "original-label")))
    (with-query-result/tree
     await
     '("SELECT dialog_id, ord FROM dialogs"
       " WHERE label = ? AND scenario_id = ?")
     `(,orig-label ,data-id)
     (^[rset]
       (for-each
        (^[row]
          (let ((dialog-id (vector-ref row 0))
                (ord (vector-ref row 1)))
            (print #"Found dialog ~|dialog-id| to delete.")

            (execute-query-tree '("DELETE FROM options "
                                  " WHERE line_id ="
                                  " (SELECT line_id FROM lines WHERE dialog_id = ?)")
                                dialog-id)

            (execute-query-tree '("DELETE FROM lines WHERE dialog_id = ?")
                                dialog-id)

            (execute-query-tree '("DELETE FROM dialogs WHERE dialog_id = ?")
                                dialog-id)

            (convert-dialog-to-relations await data-id form-data ord)

            ))
        rset)))
    'done))

(define (delete-conversation await data-id label)
  (with-query-result/tree
   await
   '("SELECT dialog_id, ord FROM dialogs"
     " WHERE label = ? AND scenario_id = ?")
   `(,label ,data-id)
   (^[rset]
     (for-each
      (^[row]
        (let ((dialog-id (vector-ref row 0))
              (ord (vector-ref row 1)))
          (print #"Found dialog ~|dialog-id| to delete.")

          (execute-query-tree '("DELETE FROM options "
                                " WHERE line_id ="
                                " (SELECT line_id FROM lines WHERE dialog_id = ?)")
                              dialog-id)

          (execute-query-tree '("DELETE FROM lines WHERE dialog_id = ?")
                              dialog-id)

          (execute-query-tree '("DELETE FROM dialogs WHERE dialog_id = ?")
                              dialog-id)


          ))
      rset)))

  'done)

(define (insert-conversation await data-id form-data)
  (define (get name)
    (let ((val (assoc name form-data)))
      (if val
          (cdr val)
          (error #"paramter not specified: ~|name|"))))

  (let ((ord (get "ord")))
    (convert-dialog-to-relations await data-id form-data ord)))

(define (update-with-json await data-id json)
  (define form-data (parse-json-string json))
  (cond ((assoc "ord" form-data)
         (insert-conversation await data-id form-data))
        ((assoc "original-label" form-data)
         (update-existing-conversation await data-id form-data))))

(define (convert-json-to-csv await data-id)
  (let ((dialog-writer (make-csv-writer ","))
        (option-writer (make-csv-writer ","))
        (meta-writer (make-csv-writer ","))
        (json (read-scenario-from-db await data-id)))
    #?=(overwrite-json-file await json (json-file-path data-id))
    (call-with-output-file "csv/Dialog_meta.csv"
      (^[meta-port]
        (call-with-output-file "csv/Dialog.csv"
          (^[dialog-port]
            (call-with-output-file "csv/Dialog_options.csv"
              (^[option-port]
                (dialog-writer dialog-port '("" "character" "text" "options"))
                (option-writer option-port '("" "option"))
                (meta-writer meta-port '("" "type" "location" "count"))
                (json-match
                 json
                 (write-with-csv-writers dialog-port dialog-writer
                                         option-port option-writer
                                         meta-port meta-writer)
                 )))))))))

(define (write-with-csv-writers dialog-port dialog-writer
                                option-port option-writer
                                meta-port meta-writer)
  (^[% @]
    (@ (^d
        (let ((label #f) (location #f) (type #f) (linecount 0))
          ((% "label" (cut set! label <>)) d)
          ((% "location" (cut set! location <>)) d)
          ((% "type" (cut set! type <>)) d)
          ((% "lines"
              (^d
               (set! linecount (vector-length d))
               (let ((char #f) (text #f) (num 0) (optcount 0))
                 ((@ (^j
                      ((% "character" (cut set! char <>)) j)
                      ((% "text" (cut set! text <>)) j)
                      (when (assoc "options" j)
                        ((% "options"
                            (^d
                             (set! optcount (vector-length d))
                             (let ((optnum 0))
                               ((@
                                 (^[option]
                                   (option-writer option-port
                                                  `(,#"~|label|_~|num|_~|optnum|"
                                                    ,(cdr (assoc "text" option))))
                                   (inc! optnum)
                                   ))
                                d))))
                         j))
                      (dialog-writer dialog-port
                                     `(,#"~|label|_~num" ,char ,text
                                       ,(x->string optcount)))
                      (inc! num)
                      #t
                      j))
                  d))))
           d)
          (meta-writer meta-port
                       `(,label ,type ,location
                                ,(x->string linecount))))))))

(define-http-handler #/^\/scenarios\/(\d+)\/update-csv/
  (^[req app]
    (violet-async
     (^[await]
       (let-params req ([data-id "p:1"])
                   (convert-json-to-csv await data-id)
                   (ok req "ok")
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
                      (respond/redirect req #"/scenarios/~|id|#label-~label"))))))))

(define-http-handler #/^\/scenarios\/(\d+)\/delete/
  (with-post-parameters
   (^[req app]
     (violet-async
      (^[await]
        (let-params req ([id "p:1"]
                         [label "q"])
                    (let ((result (delete-conversation await id label)))
                      (respond/redirect req #"/scenarios/~|id|"))))))))

(define (render-location await id loc)
  (define (get name conv)
    (cdr (assoc name conv)))
  (let ((content (read-scenario-from-db await id)))
    `(div (@ (class "container"))
          ((p (a (@ (href ,#"/scenarios/~id"))
                 ,(fas-icon "chevron-left")
                 "戻る"))
           (div (@ (class "block"))
                (h2 (@ (class "title is-2")) ,loc)
                (table (@ (class "table"))
                       ,(map
                         (^[conv]
                           (let ((label (get "label" conv)))
                             `(tr
                               (td (span (@ (class "tag is-info"))
                                         ,(get "type" conv)))
                               (td (a (@ (href ,#"/scenarios/~|id|#label-~label"))
                                      ,label)))))
                         (filter
                          (^[conv]
                            (string=? loc (get "location" conv)))
                          content))))))))

(define-http-handler #/^\/scenarios\/(\d+)\/locations\/([^\/]+)\/?$/
  (^[req app]
     (violet-async
      (^[await]
        (let-params req ([id "p:1"] [loc "p:2"])
                    (let ((rendered (render-location await id loc)))
                      (ok req rendered)))))))

(define-http-handler #/^\/static\// (file-handler))

(define *sqlite-conn* #f)

(define (with-query-result await str args proc)
  (let ((rset (await (^[] (apply dbi-do *sqlite-conn* str '() args)))))
    (let ((result (proc rset)))
      (dbi-close rset)
      result)))

(define (with-query-result/tree await tree args proc)
  (with-query-result await (tree->string tree) args proc))

(define (execute-query str args)
  (guard (e [else (report-error e)])
         (apply dbi-do *sqlite-conn* str '() args)))

(define (execute-query-tree tree . args)
  (execute-query (tree->string tree) args))

(define-http-handler "/admin/setup"
  (^[req app]
    (violet-async
     (^[await]
       (await create-tables)
       (ok req '(p "done")
           '(a (@ (href "/")) "Back Home"))))))

(define (convert-dialog-to-relations await id dialog dialog-order)
  (let ((label (cdr (assoc "label" dialog)))
        (location (cdr (assoc "location" dialog)))
        (type (cdr (assoc "type" dialog)))
        (lines (cdr (assoc "lines" dialog))))
    (execute-query-tree '("INSERT INTO dialogs"
                          " (scenario_id, ord, label, location, type)"
                          " VALUES (?, ?, ?, ?, ?)")
                        id dialog-order label location type)
    (let ((dialog-id (sqlite3-last-id *sqlite-conn*))
          (line-order 0))
      (for-each
       (^[line]
         (let ((character (cdr (assoc "character" line)))
               (text (cdr (assoc "text" line)))
               (options (let1 opt (assoc "options" line)
                          (if opt (cdr opt) ()))))
           (execute-query-tree '("INSERT INTO lines (dialog_id, ord, character, text)"
                                 " VALUES (?, ?, ?, ?)")
                               dialog-id line-order character text)
           (let ((line-id (sqlite3-last-id *sqlite-conn*))
                 (option-order 0))
             (for-each
              (^[option]
                (execute-query-tree '("INSERT INTO options (line_id, ord, text)"
                                      " VALUES (?, ?, ?)")
                                    line-id option-order option)
                (set! option-order (+ option-order 1024)))
              options))
           (set! line-order (+ line-order 1024))
           ))
       lines))
    ))

(define (convert-scenario-file-to-relations await id)
  (let ((json (read-scenario-file await id))
        (dialog-order 0))
    (for-each
     (^[dialog]
       (convert-dialog-to-relations await id dialog dialog-order)
       (set! dialog-order (+ dialog-order 1024)))
     json)
    "OK"
    ))

(define-http-handler #/^\/scenarios\/(\d+)\/convert$/
  (^[req app]
     (violet-async
      (^[await]
        (let-params req ([id "p:1"])
                    (let ((result (convert-scenario-file-to-relations await id)))
                      (ok req result " " '(a (@ (href "/")) "Back Home"))))))))

(define (create-tables)
  (execute-query-tree '("CREATE TABLE IF NOT EXISTS scenarios ("
                        "  scenario_id INTEGER PRIMARY KEY"
                        ", title       TEXT NOT NULL"
                        ", initial_location INTEGER"
                        ")"))
  (execute-query-tree '("CREATE TABLE IF NOT EXISTS dialogs ("
                        "  dialog_id   INTEGER PRIMARY KEY"
                        ", scenario_id INTEGER NOT NULL"
                        ", ord         INTEGER NOT NULL"
                        ", label       TEXT NOT NULL"
                        ", location    TEXT NOT NULL"
                        ", type        TEXT NOT NULL"
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

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS options ("
                        "  option_id   INTEGER PRIMARY KEY"
                        ", line_id     INTEGER NOT NULL"
                        ", ord         INTEGER NOT NULL"
                        ", text        TEXT NOT NULL"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS option_jumps ("
                        "  option_id   INTEGER NOT NULL"
                        ", destination TEXT NOT NULL"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS option_flags ("
                        "  option_id   INTEGER NOT NULL"
                        ", flag        TEXT NOT NULL"
                        ")"))
  )

(define (scenario-start!)
  (let ((conn (dbi-connect "dbi:sqlite3:scenario-sqlite3.db")))
    (set! *sqlite-conn* conn)
    (print "Sqlite connected")
    (flush)))
