(define-module scenario
  (use gauche.threads)
  (use gauche.collection)

  (use file.util)
  (use rfc.http)
  (use rfc.json)
  (use sxml.tools)
  (use text.csv)

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
     (title "Starter Template · Bootstrap")
     (link (@ (rel "stylesheet")
              (href "https://cdn.jsdelivr.net/npm/bulma@0.9.0/css/bulma.min.css")))
     (script (@ (src "https://kit.fontawesome.com/515fd4f349.js")
                (crossorigin "anonymous")) ""))
    (body
     ,@children
     (script (@ (src "/static/script.js")) "")))
  )


(define-http-handler "/"
  (^[req app]
    (violet-async
     (^[await]
       (respond/ok req (cons "<!DOCTYPE html>"
                               (sxml:sxml->html
                                (create-page
                                 `(ul (li (a (@ (href "/scenarios/1")) "Scenario #1")))
                                 ))))
       ))))

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
									,o)) options))))))

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
		(loc (get "location")))
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
								   ,loc))))
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
							  (if opt (cdr opt) ()))))
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

(define (add-conversation-button data-id prev-label)
  `(div (@ (class "columns"))
        (div (@ (class "column has-text-centered"))
             (a (@ (class "button")
                   (href ,#"/scenarios/~|data-id|/insert/~|prev-label|#form"))
                ,(fas-icon "comments")
				(span (@ (style "margin-left: 0.5ex"))"会話を追加")))))

(define (read-scenario-file await id)
  (await (^[] (with-input-from-file (json-file-path id) parse-json))))

(define (read-and-render-scenario-file await id)
  (let ((content (read-scenario-file await id))
        (prev-label ""))
    (append
	 (reverse
      (fold (^[conv rest]
              (let ((prev-label-bak prev-label))
                (set! prev-label (cdr (assoc "label" conv)))
				(cons (render-conversation/edit-button conv id)
                      (cons (add-conversation-button id prev-label-bak)
							rest))))
            ()
            content))
     (list (add-conversation-button id prev-label)))))

(define (json-file-path data-id)
  #"json/~|data-id|.json")

(define (read-and-render-scenario-file/insert await id prev-label)
  (define (new-form label)
	(render-conversation-form #f id
                              `(input (@ (id "prev-label-input")
                                         (type "hidden")
                                         (value ,label)))))
  (let ((content (read-scenario-file await id)))
          (reverse
           (fold (^[conv rest]
                   (let ((label (cdr (assoc "label" conv))))
                     (if (string=? label prev-label)
                         (cons (new-form label)
                               (cons (render-conversation conv id) rest))
                         (cons (render-conversation conv id) rest))))
                 (if (string=? prev-label "")
                     (list (new-form ""))
                     ())
                 content))))

(define (read-and-render-scenario-file/edit await id label-to-edit)
  (define (new-form conv label)
	(render-conversation-form conv id
                              `(input (@ (id "original-label-input")
                                         (type "hidden")
                                         (value ,label)))))

  (let ((content (read-scenario-file await id)))
          (reverse
           (fold (^[conv rest]
                   (let ((label (cdr (assoc "label" conv))))
                     (cons (if (string=? label label-to-edit)
                               (new-form conv label)
                               (render-conversation conv id))
                           rest)))
                 ()
                 content))))

(define-http-handler #/^\/scenarios\/(\d+)$/
  (^[req app]
    (violet-async
     (^[await]
       (let-params req ([id "p:1"])
                   (let ((rendered (read-and-render-scenario-file await id)))
                     (respond/ok req (cons "<!DOCTYPE html>"
                                           (sxml:sxml->html
                                            (create-page
                                             rendered
                                             ))))))))))

(define-http-handler #/^\/scenarios\/(\d+)\/insert\/(.*)/
  (^[req app]
    (violet-async
     (^[await]
       (let-params req ([id "p:1"]
                        [label "p:2"])
                   (let ((rendered (read-and-render-scenario-file/insert await id label)))
                     (respond/ok req (cons "<!DOCTYPE html>"
                                           (sxml:sxml->html
                                            (create-page
                                             rendered
                                             ))))))))))

(define-http-handler #/^\/scenarios\/(\d+)\/edit\/(.*)/
  (^[req app]
    (violet-async
     (^[await]
       (let-params req ([id "p:1"]
                        [label "p:2"])
                   (let ((rendered (read-and-render-scenario-file/edit await id label)))
                     (respond/ok req (cons "<!DOCTYPE html>"
                                           (sxml:sxml->html
                                            (create-page
                                             rendered
                                             ))))))))))

(define (overwrite-json-file await json filename)
  (await (^[]
           (let ((new-json (list->vector json)))
             (call-with-temporary-file
              (^[port tmpfile]
                (with-output-to-port port
                  (^[]
                    (construct-json new-json)
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

  (let ((type (get "type"))
        (orig-label (get "original-label"))
        (label (get "label"))
        (location (get "location"))
        (lines (get "lines")))
    (let ((filename (json-file-path data-id))
          (modified
		   (let ((content (read-scenario-file await data-id)))
             (reverse
              (fold (^[conv rest]
                      (cons
                       (if (string=? orig-label (cdr (assoc "label" conv)))
                           `((label . ,label)
                             (location . ,location)
                             (type . ,type)
                             (lines . ,lines))
                           conv)
                       rest))
                    ()
                    content)))))
      (overwrite-json-file await modified filename))))

(define (delete-conversation await data-id label)
(let* ((filename (json-file-path data-id))
           (modified (let ((content (read-scenario-file await data-id)))
                              (reverse
                               (fold (^[conv rest]
                                        (if (string=? label (cdr (assoc "label" conv)))
                                            rest
                                            (cons conv rest)))
                                     ()
                                     content)))))
  (overwrite-json-file await modified filename)))

(define (insert-conversation await data-id form-data)
  (define (get name)
    (let ((val (assoc name form-data)))
      (if val
          (cdr val)
          (error #"paramter not specified: ~|name|"))))

  (let ((type (get "type"))
        (prev-label (get "previous-label"))
        (label (get "label"))
        (location (get "location"))
        (lines (get "lines")))
    (let* ((filename (json-file-path data-id))
           (modified
			(let ((content (read-scenario-file await data-id)))
			  (if (string=? prev-label "")
                  (cons
				   `((label . ,label)
                     (location . ,location)
                     (type . ,type)
                     (lines . ,lines))
				   (vector->list content))
				  (reverse
				   (fold (^[conv rest]
                           (if (string=? prev-label (cdr (assoc "label" conv)))
                               (cons `((label . ,label)
                                       (location . ,location)
                                       (type . ,type)
                                       (lines . ,lines))
									 (cons conv rest))
                               (cons conv rest)))
						 ()
						 content))))))
      (overwrite-json-file await modified filename))))

(define (update-with-json await data-id json)
  (define form-data (parse-json-string json))
  (cond ((assoc "original-label" form-data)
         (update-existing-conversation await data-id form-data))
        ((assoc "previous-label" form-data)
         (insert-conversation await data-id form-data))))

(define (convert-json-to-csv await data-id)
  (let ((dialog-writer (make-csv-writer ","))
        (option-writer (make-csv-writer ","))
        (meta-writer (make-csv-writer ",")))
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
				 (read-scenario-file await data-id)
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
																   ,option))
												  (inc! optnum)
												  ))
											   d))))
										j))
									 (dialog-writer dialog-port
													`(,#"~|label|_~num" ,char ,text ,(x->string optcount)))
									 (inc! num)
									 #t
									 j))
								 d))))
						  d)
						 (meta-writer meta-port
									  `(,label ,type ,location
											   ,(x->string linecount))))))))))))))))

(define-http-handler #/^\/scenarios\/(\d+)\/update-csv/
  (^[req app]
    (violet-async
     (^[await]
       (let-params req ([data-id "p:1"])
                   (convert-json-to-csv await data-id)
                   (respond/ok req "ok")
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
  (let ((content (read-scenario-file await id)))
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
						  content))))))
	))

(define-http-handler #/^\/scenarios\/(\d+)\/locations\/([^\/]+)\/?$/
  (^[req app]
     (violet-async
      (^[await]
        (let-params req ([id "p:1"] [loc "p:2"])
					(let ((rendered (render-location await id loc)))
                      (respond/ok req (cons "<!DOCTYPE html>"
											(sxml:sxml->html
                                             (create-page
                                              rendered
                                              )))))))))
)

(define-http-handler #/^\/static\// (file-handler))

(define *sqlite-conn* #f)

(define (execute-query str)
  (dbi-execute (dbi-prepare *sqlite-conn* str)))

(define (execute-query-tree tree)
  (execute-query (tree->string tree)))

(define-http-handler "/admin/setup"
  (^[req app]
    (violet-async
     (^[await]
       (await create-tables)
       (respond/ok req (cons "<!DOCTYPE html>"
                             (sxml:sxml->html
                              (create-page
                               "初期設定"
                               '(p "done")
                               ))))))))

(define (create-tables)
  (execute-query-tree '("CREATE TABLE IF NOT EXISTS scenarios ("
						"  scenario_id INTEGER PRIMARY KEY"
                        ", title       TEXT NOT NULL"
                        ", initial_location INTEGER"
                        ")"))
  (execute-query-tree '("CREATE TABLE IF NOT EXISTS dialogs ("
						"  dialog_id   INTEGER PRIMARY KEY"
						", scenario_id INTEGER NOT NULL"
						", order       INTEGER NOT NULL"
                        ", label       TEXT NOT NULL"
						", location    TEXT NOT NULL"
						", type        TEXT NOT NULL"
                        ")"))
  (execute-query-tree '("CREATE TABLE IF NOT EXISTS lines ("
						"  line_id     INTEGER PRIMARY KEY"
						", dialog_id   INTEGER NOT NULL"
						", order       INTEGER NOT NULL"
                        ", label       TEXT NOT NULL"
						", location    TEXT NOT NULL"
						", type        TEXT NOT NULL"
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
						"  line_id     INTEGER NOT NULL"
						", text        TEXT NOT NULL"
                        ")"))
  )

(define (scenario-start!)
  (let ((conn (dbi-connect "dbi:sqlite3:scenario-sqlite3.db")))
    (set! *sqlite-conn* conn)
    (print "Sqlite connected")
	(flush)))
