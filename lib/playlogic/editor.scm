(define-module playlogic.editor
  (use gauche.collection)

  (use file.util)
  (use rfc.json)
  (use sxml.tools)
  (use text.csv)
  (use text.tree)

  (use makiki)

  (use dbi)
  (add-load-path "../gosh-modules/dbd-sqlite3" :relative)
  (use dbd.sqlite3)

  (add-load-path "." :relative)
  (use json-match)

  (export *sqlite-conn*
          read-and-render-scenario-file
          read-and-render-scenario-file/edit
          read-and-render-scenario-file/insert
          convert-json-to-csv
          update-with-json
          delete-existing-dialogs
          render-location
          create-tables
          convert-scenario-file-to-relations
          scenario-page-header
          ok
          ok*

          read-dialog-detail-from-db

          fas-icon

          ;; query
          with-query-result/tree
   ))

(select-module playlogic.editor)

;;
;; Application
;;

(define *session-id* 0)

(define (create-page . children)
  (apply create-page/title "Scenario Edit" children))

(define (create-page/title title . children)
  (inc! *session-id*)
  `(html
    (@ (lang "en"))
    (head
     (meta (@ (charset "utf-8")))
     (meta (@ (name "viewport") (content "width=device-width, initial-scale=1")))
     (title ,title)
     (link (@ (rel "stylesheet")
              (href "https://cdn.jsdelivr.net/npm/bulma@0.9.0/css/bulma.min.css")))
     (script (@ (src "https://kit.fontawesome.com/515fd4f349.js")
                (crossorigin "anonymous")) ""))
    (body
     ,@children
     (script (@ (src "/static/script.js")) "")))
  )

(define (create-error-page e)
  (cons "<!DOCTYPE html>"
        (sxml:sxml->html
         (create-page/title
          "ERROR!"
          `((div (@ (class "container"))
                 (h1 (@ (class "title"))
                     "Something went wrong " ,(fas-icon "sad-tear"))
                 (pre ,(report-error e #f))))))))

(define-syntax ok
  (syntax-rules ()
    [(_ req title elements ...)
     (guard (e [else (report-error e)
                     (respond/ng req 500 :body (create-error-page e))])
            (respond/ok req (cons "<!DOCTYPE html>"
                                  (sxml:sxml->html
                                   (create-page/title title elements ...)))))]))
(define-syntax ok*
  (syntax-rules ()
    [(_ req elements)
     (guard (e [else (report-error e)
                     (respond/ng req 500 :body (create-error-page e))])
            (respond/ok req (cons "<!DOCTYPE html>"
                                  (sxml:sxml->html
                                   (apply create-page/title elements)))))]))

(define (container/ . children)
  `(div (@ (class "container"))
        ,@children))

(define (fas-icon name)
  `(span (@ (class "icon"))
         (i (@ (class ,#"fas fa-~name")) "")))

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
        (div (@ (class "column is-one-fifth has-text-right"))
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

(define (render-dialog conv data-id . additioanl-elements)
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
    `(section (@ (class "section") (id ,#"label-~label"))
              (div (@ (class "container"))
                   (div (@ (class "columns is-vcentered"))
                        ,@additioanl-elements
                        (div (@ (class "column is-2"))
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

                   ,(if (string=? type "portal")
                        `(div (@ (class "columns"))
                              (div (@ (class "column"))
                                   ,(fas-icon "walking")
                                   " "
                                   ,(get "portal-destination")))
                        ())
                   ,@(render-lines lines)))))

(define (render-dialog/edit-button conv data-id)
  (render-dialog conv data-id
                       (edit-buttons data-id (cdr (assoc "label" conv)))))

(define (not-empty? str) (and str (not (zero? (string-length str)))))

(define (form-field label help input)
  `(div (@ (class "field"))
        ,(if (not-empty? label) `(label (@ (class "label")) ,label) ())
        (div (@ (class "control")) ,input)
        ,(if (not-empty? help) `(p (@ (class "help")) ,help) ())))

(define (render-option-form option)
  (define (get slot o)
    (let ((val (assoc slot o)))
      (if val
          (cdr val)
          #f)))

  (define (flags key)
    (apply string-append
           (intersperse
            " " (vector->list
                 (or (get key option) #())))))

  `(div (@ (class "option-input-group"))
    (div (@ (class "columns is-vcentered"))
         (div (@ (class "column is-one-fifth has-text-right"))
              ,(fas-icon "angle-right"))
         (div (@ (class "column"))
              (input (@ (class "input option-input") (type "text")
                        (placeholder "選択肢")
                        (value ,(or (get "text" option) ""))))))

    (div (@ (class "columns is-vcentered"))
         (div (@ (class "column is-one-quarter has-text-right"))
              ,(fas-icon "angle-right"))
         (div (@ (class "column"))
              (p (span (@ (class "tag is-primary"))
                       "要求フラグ"))
              (input (@ (class "input option-flags-req-input") (type "text")
                        (placeholder "フラグ1 フラグ2 ...")
                        (value ,(flags "flags-required")))))
         (div (@ (class "column"))
              (p (span (@ (class "tag is-warning"))
                       "ジャンプ先"))
              (input (@ (class "input option-jump-input") (type "text")
                        (placeholder "会話 ID")
                        (value ,(flags "jump-to"))))))
    ))

(define (render-line-form char text options)
  `((div (@ (class "line-form"))
         (div (@ (class "columns"))
              (div (@ (class "column is-one-fifth"))
                   (input (@ (class "input character-input")
                             (type "text")
                             (placeholder "キャラクター")
                             (value ,char))))
              (div (@ (class "column"))
                   (input (@ (class "input line-input") (type "text")
                             (placeholder "セリフ")
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
                           (span (@ (style "margin-left: 0.5ex"))
                                 "選択肢を追加"))))))))

(define (render-dialog-form conv data-id hidden-inputs)
  (define (get name default)
    (if conv
        (cdr (assoc name conv))
        default))
  (define (get-optional name default)
    (if conv
        (let ((found (assoc name conv)))
          (if found (cdr found) default))
        default))
  (define (value-and-selected selected type)
    (if (string=? type selected)
        `((value ,type) (selected "selected"))
        `((value ,type))))

  (define label (get "label" ""))
  (define lines (get "lines" #()))
  (define type (get "type" "conversation"))

  (define (type-selector)
    `(select (@ (id "type-input"))
             (option (@ ,@(value-and-selected type "conversation")) "会話")
             (option (@ ,@(value-and-selected type "inspection"))   "調べる")
             (option (@ ,@(value-and-selected type "area"))         "エリア")
             (option (@ ,@(value-and-selected type "message"))      "メッセージ")
             (option (@ ,@(value-and-selected type "portal"))       "ポータル")))

  (define (conv-form)
    (define (flags key)
      (apply string-append
             (intersperse
              " " (vector->list
                   (or (get key conv) #())))))

    `(div
      (div (@ (class "columns"))
          (div (@ (class "column is-2"))
               ,(form-field
                 "タイプ" #f
                 `(div (@ (class "select"))
                       ,(type-selector))))
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
                                       (value ,(get "location" ""))))))
          (div (@ (class "column"))
               ,(form-field "トリガー" #f
                            `(input (@ (class "input") (type "text")
                                       (id "trigger-input")
                                       (placeholder "トリガー")
                                       (value ,(get "trigger" "")))))))

      (div (@ (class "columns is-vcentered"))
           (div (@ (class "column is-one-third"))
                (p (span (@ (class "tag is-primary"))
                         "要求フラグ"))
                (input (@ (class "input")
                          (id "flags-req-input")
                          (type "text")
                          (placeholder "フラグ1 フラグ2 ...")
                          (value ,(flags "flags-required")))))
           (div (@ (class "column is-one-third"))
                (p (span (@ (class "tag is-danger"))
                         "排他的フラグ"))
                (input (@ (class "input")
                          (id "flags-exc-input")
                          (type "text")
                          (placeholder "フラグ1 フラグ2 ...")
                          (value ,(flags "flags-exclusive")))))
           (div (@ (class "column is-one-third"))
                (p (span (@ (class "tag is-info"))
                         "セットするフラグ"))
                (input (@ (class "input")
                          (id "flags-set-input")
                          (type "text")
                          (placeholder "フラグ1 フラグ2 ...")
                          (value ,(flags "flags-set"))))))

      (div (@ (id "portal-destination-input")
              (class "columns is-vcentered"))
           (div (@ (class "column is-one-third"))
                (p (span (@ (class "tag is-primary"))
                         "行先ポータルのラベル"))
                (input (@ (class "input")
                          (id "portal-destination")
                          (type "text")
                          (placeholder "行先ポータル")
                          (value ,(get-optional "portal-destination" ""))))))))

  (define (line-fields lines)
    (reverse
     (fold (^[line rest]
             (let ((char (cdr (assoc "character" line)))
                   (text (cdr (assoc "text" line)))
                   (options (let ((opt (assoc "options" line)))
                              (if opt
                                  (cdr opt)
                                  #()))))
               (append (render-line-form char text options) rest)))
           () lines)))

  (define (buttons)
    `(div (@ (class "columns"))
          (div (@ (class "column"))
               (div (@ (class "field is-grouped is-grouped-right"))
                    (p (@ (class "control"))
                       (button (@ (class "button is-primary")) "更新"))
                    (p (@ (class "control"))
                       (a (@ (class "button is-light")
                             (href ,#"/scenarios/~|data-id|#label-~label"))
                          "キャンセル"))))))

  (define (add-line-button)
    `(div (@ (class "columns"))
         (div (@ (class "column"))
              (div (@ (class "field has-text-centered"))
                   (a (@ (class "button")
                         (id "add-line-button"))
                      ,(fas-icon "comment")
                      (span (@ (style "margin-left: 0.5ex"))"セリフを追加"))))))

  `(section
    (@ (class "section")
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
               ,(add-line-button)
               ,(buttons)))
    (div (@ (style "display: none")
            (id "hidden-field"))
         ,@(render-line-form "" "" ()))
    (div (@ (style "display: none")
            (id "hidden-option-field"))
         ,@(render-option-form ()))))

(define (add-dialog-button data-id prev-label ord)
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
   '("SELECT dialog_id, label, location, type, ord, trigger"
     " FROM dialogs"
     " WHERE scenario_id = ?"
     " ORDER BY ord")
   `(,id)
   (^[rset]
     (map-to <vector>
      (^[row]
        (read-dialog-detail-from-db await row))
      rset))))

(define (read-dialog-detail-from-db await row)
  (let ((dialog-id (vector-ref row 0))
        (label   (vector-ref row 1))
        (loc     (vector-ref row 2))
        (typ     (vector-ref row 3))
        (ord     (vector-ref row 4))
        (trigger (vector-ref row 5)))
    `(("id" . ,dialog-id)
      ("label" . ,label)
      ("type" . ,typ)
      ("location" . ,loc)
      ("trigger" . ,trigger)
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
                     (read-line-from-db await row))
                   rset))))
      ("flags-required" .
       ,(with-query-result/tree
         await
         '("SELECT flag"
           " FROM flags_required"
           " WHERE dialog_id = ?")
         `(,dialog-id)
         (^[rset]
           (map-to <vector> (^[row] (read-flags-from-db await row)) rset))))
      ("flags-exclusive" .
       ,(with-query-result/tree
         await
         '("SELECT flag"
           " FROM flags_exclusive"
           " WHERE dialog_id = ?")
         `(,dialog-id)
         (^[rset]
           (map-to <vector> (^[row] (read-flags-from-db await row)) rset))))
      ("flags-set" .
       ,(with-query-result/tree
         await
         '("SELECT flag"
           " FROM flags_set"
           " WHERE dialog_id = ?")
         `(,dialog-id)
         (^[rset]
           (map-to <vector> (^[row] (read-flags-from-db await row)) rset))))

      ,@(if (string=? typ "portal")
            `(("portal-destination" .
               ,(with-query-result/tree
                 await
                 '("SELECT destination"
                   " FROM portals"
                   " WHERE dialog_id = ?")
                 `(,dialog-id)
                 (^[rset]
                   (car (map (^[row] (vector-ref row 0)) rset))))))
            ()))))

(define (read-flags-from-db await row)
  (let ((flag (vector-ref row 0)))
    flag))

(define (read-line-from-db await row)
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
         '("SELECT option_id, text FROM options"
           " WHERE line_id = ? ORDER BY ord")
         `(,line-id)
         (^[rset]
           (map-to <vector>
                   (^[row]
                     (read-option-from-db await row))
                   rset)))))))

(define (read-option-from-db await row)
  (let ((option-id (vector-ref row 0))
        (text (vector-ref row 1)))
    `(("text" . ,text)
      ("flags-required" .
       ,(with-query-result/tree
         await
         '("SELECT flag FROM option_flags_required"
           " WHERE option_id = ?")
         `(,option-id)
         (^[rset]
           (map-to <vector> (^[row] (vector-ref row 0)) rset))))
      ("jump-to" .
       ,(with-query-result/tree
         await
         '("SELECT destination FROM option_jumps"
           " WHERE option_id = ?")
         `(,option-id)
         (^[rset]
           (map-to <vector> (^[row] (vector-ref row 0)) rset)))))))

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
               (values (cons (render-dialog/edit-button conv id)
                             (cons (add-dialog-button id prev-label ord)
                                   rest))
                       conv)))
           () #f
           content))
  (let ((content (read-scenario-from-db await id)))
    (if (zero? (vector-length content))
	`(,(add-dialog-button id #f 1024))
	(let-values (((convs prev-conv)
                      (elems content)))
	  `(,@(reverse convs)
            ,(add-dialog-button id (label-of prev-conv)
				(+ 1024 (ord-of prev-conv))))))
    ))

(define (json-file-path data-id)
  #"json/~|data-id|.json")

(define (read-and-render-scenario-file/insert await id ord)
  (define (new-form ord)
    (render-dialog-form #f id
                        `(input (@ (id "ord-input")
                                   (type "hidden")
                                   (value ,ord)))))
  (let ((content (read-scenario-from-db await id)))
    (reverse
     (let-values (((result inserted?)
                   (fold2 (^[conv rest inserted?]
                            (let ((next-ord (cdr (assoc "ord" conv))))
                              (if (and (not inserted?) (> next-ord ord))
                                  (values (cons (render-dialog conv id)
                                                (cons (new-form ord) rest))
                                          #t)
                                  (values (cons (render-dialog conv id) rest)
                                          inserted?))))
                          () #f
                          content)))
       (if inserted?
           result
           (cons (new-form ord) result))
       ))))

(define (read-and-render-scenario-file/edit await id label-to-edit)
  (define (new-form conv label)
    (render-dialog-form conv id
                              `(input (@ (id "original-label-input")
                                         (type "hidden")
                                         (value ,label)))))

  (let ((content (read-scenario-from-db await id)))
          (reverse
           (fold (^[conv rest]
                   (let ((label (cdr (assoc "label" conv))))
                     (cons (if (string=? label label-to-edit)
                               (new-form conv label)
                               (render-dialog conv id))
                           rest)))
                 ()
                 content))))

(define (scenario-page-header await id)
  (container/
   `(h1 (@ (class "title")) ,#"Scenario #~id")
   `(p (a (@ (class "button is-danger") (href ,#`"/scenarios/,|id|/convert"))
          ,(fas-icon "skull-crossbones") (span "Convert from JSON"))
       " "
       (a (@ (class "button") (href ,#`"/scenarios/,|id|/update-csv"))
          ,(fas-icon "save") (span "Update CSV/JSON"))
       " "
       (a (@ (class "button is-primary")
             (href ,#`"/scenarios/,|id|/play/,*session-id*"))
          ,(fas-icon "gamepad") (span "Play!")))))

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
                (sys-system #"jq '[.[] | del(.id) | del(.ord) | del(.lines[].id) | del(.lines[].ord) ]' < ~tmpfile > ~filename"))
              :directory "json")
             'done
             ))))

(define (delete-dialog dialog-id)
  (execute-query-tree '("DELETE FROM option_flags_required "
                        " WHERE option_id in "
                        " (SELECT option_id FROM options WHERE line_id in"
                        "   (SELECT line_id FROM lines WHERE dialog_id = ?))")
                      dialog-id)

  (execute-query-tree '("DELETE FROM option_jumps "
                        " WHERE option_id in "
                        " (SELECT option_id FROM options WHERE line_id in"
                        "   (SELECT line_id FROM lines WHERE dialog_id = ?))")
                      dialog-id)

  (execute-query-tree '("DELETE FROM options "
                        " WHERE line_id in "
                        " (SELECT line_id FROM lines WHERE dialog_id = ?)")
                      dialog-id)

  (execute-query-tree '("DELETE FROM lines WHERE dialog_id = ?")
                      dialog-id)

  (execute-query-tree '("DELETE FROM flags_required WHERE dialog_id = ?")
                      dialog-id)
  (execute-query-tree '("DELETE FROM flags_exclusive WHERE dialog_id = ?")
                      dialog-id)
  (execute-query-tree '("DELETE FROM flags_set WHERE dialog_id = ?")
                      dialog-id)

  (execute-query-tree '("DELETE FROM dialogs WHERE dialog_id = ?")
                      dialog-id))

(define (delete-existing-dialogs await data-id label)
  (define last-order 0)

  (with-query-result/tree
   await
   '("SELECT dialog_id, ord FROM dialogs"
     " WHERE label = ? AND scenario_id = ?")
   `(,label ,data-id)
   (^[rset]
     (for-each
      (^[row]
        (await
         (^[]
           (let ((dialog-id (vector-ref row 0))
                 (ord (vector-ref row 1)))
             (print #"Found dialog ~|dialog-id| to delete.")

             (delete-dialog dialog-id)

             (set! last-order ord)

             ))))
      rset)
     ))
  last-order)

(define (update-existing-dialog await data-id form-data)
  (define (get name)
    (let ((val (assoc name form-data)))
      (if val
          (cdr val)
          (error #"paramter not specified: ~|name|"))))

  (let ((orig-label (get "original-label")))
    (execute-query-tree '("BEGIN TRANSACTION"))
    (guard (e [else (report-error e)
                    (execute-query-tree '("ROLLBACK"))
                    (print #"Transaction failed!")])
           (let ((ord (delete-existing-dialogs await data-id orig-label)))
             (convert-dialog-to-relations await data-id form-data ord))
           (execute-query-tree '("COMMIT"))
           (print #"Transaction done!"))
    'done))

(define (insert-dialog await data-id form-data)
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
         (insert-dialog await data-id form-data))
        ((assoc "original-label" form-data)
         (update-existing-dialog await data-id form-data))))

(define (convert-json-to-csv await data-id)
  (let ((json (read-scenario-from-db await data-id)))
    (overwrite-json-file await json (json-file-path data-id))
    (call-with-output-file "csv/Dialogs.csv"
      (^[dialog-port]
        (call-with-output-file "csv/Dialogs_lines.csv"
          (^[line-port]
            (call-with-output-file "csv/Dialogs_options.csv"
              (^[option-port]
                (call-with-output-file "csv/Dialogs_flags.csv"
                  (^[flag-port]
                    (call-with-output-file "csv/Dialogs_triggermap.csv"
                      (^[triggermap-port]
                        (call-with-output-file "csv/Dialogs_portals.csv"
                          (^[portal-port]
                            (json-match
                             json
                             (write-with-csv-writers line-port
                                                     option-port
                                                     dialog-port
                                                     flag-port
                                                     triggermap-port
                                                     portal-port)))
                          )))))))))))
    "Conversion done!"))

(define (safe-assoc-vec name alist)
  (or (assoc name alist) (cons #f #())))

(define (safe-assoc name alist)
  (or (assoc name alist) (cons #f "")))

(define (write-with-csv-writers line-port option-port dialog-port
                                flag-port triggermap-port portal-port)

  (define csv-writer (make-csv-writer ","))
  (define w-option  (cut csv-writer option-port     <>))
  (define w-line    (cut csv-writer line-port       <>))
  (define w-flag    (cut csv-writer flag-port       <>))
  (define w-dialog  (cut csv-writer dialog-port     <>))
  (define w-trigger (cut csv-writer triggermap-port <>))
  (define w-portal  (cut csv-writer portal-port     <>))

  (define trigger-table (make-hash-table string-comparator))

  (define (write-option % @ label num optnum option)
    (let ((flags-req (cdr (safe-assoc-vec "flags-required"  option)))
          (jump-to   (cdr (safe-assoc-vec "jump-to"         option))))
      (let ((opt-label #"~|label|_~|num|_~|optnum|")
            (flags-req-len (vector-length flags-req)))
      (w-option
       `(,opt-label
         ,(cdr (assoc "text" option))
         ,(x->string flags-req-len)
         ,(if (> (vector-length jump-to) 0)
              (ref jump-to 0)
              "")))
      (vector-for-each-with-index
       (^[i f] (w-flag `(,#"~|opt-label|_~i" "required" ,f)))
       flags-req))))

  (w-dialog  '("" "type" "location" "trigger" "count" "flagcount"))
  (w-option  '("" "option" "flagcount" "jump"))
  (w-line    '("" "character" "text" "options"))
  (w-flag    '("" "type" "flag"))
  (w-trigger '("" "dialog"))
  (w-portal  '("" "destination"))

  (^[% @]
    (@ (^d
        (let ((label     (cdr (safe-assoc "label" d)))
              (location  (cdr (safe-assoc "location" d)))
              (trigger   (cdr (safe-assoc "trigger" d)))
              (type      (cdr (safe-assoc "type" d)))
              (linecount (vector-length (cdr (safe-assoc-vec "lines" d))))
              (flagcount 0))
          (when (and (> (string-length trigger) 0)
                     (or (string=? type "message")
                         (> (string-length location) 0)))
            (let* ((key #"~|location|/~trigger")
                   (index (hash-table-get trigger-table key 0)))
              (w-trigger `(,#"~|key|_~index" ,label))
              (hash-table-put! trigger-table key (+ index 1))))
          ((% "flags-required"
              (@
                (^d
                 (w-flag `(,#"~|label|_~|flagcount|" "required" ,d))
                 (inc! flagcount)))) d)
          ((% "flags-exclusive"
              (@
                (^d
                 (w-flag `(,#"~|label|_~|flagcount|" "exclusive" ,d))
                 (inc! flagcount)))) d)
          ((% "flags-set"
              (@
                (^d
                 (w-flag `(,#"~|label|_~|flagcount|" "set" ,d))
                 (inc! flagcount)))) d)

          (w-dialog `(,label ,type ,location ,trigger
                             ,(x->string linecount) ,(x->string flagcount)))

          ((% "lines"
              (^d
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
                                 (cut write-option % @ label num optnum <>)
                                 (^x (inc! optnum)))
                                d))))
                         j))
                      (w-line `(,#"~|label|_~num" ,char ,text
                                ,(x->string optcount)))
                      (inc! num)
                      #t
                      j))
                  d))))
           d)

          ((% "portal-destination"
             (^d
              (w-portal `(,label ,d)))) d)

          )))))

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

;; SQLite

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

(define (convert-dialog-to-relations await id dialog dialog-order)
  (define (add-line line dialog-id line-order)
    (let ((character (cdr (assoc "character" line)))
          (text (cdr (assoc "text" line)))
          (options (let1 opt (assoc "options" line)
                         (if opt (cdr opt) ()))))
      (execute-query-tree '("INSERT INTO lines (dialog_id, ord, character, text)"
                            " VALUES (?, ?, ?, ?)")
                          dialog-id line-order character text)
      (let ((line-id (sqlite3-last-id *sqlite-conn*))
            (option-order 0))
        (add-line-details line-id option-order line options))
      (+ line-order 1024)
      ))

  (define (add-line-details line-id option-order line options)
    (for-each
     (^[option]
       (execute-query-tree '("INSERT INTO options (line_id, ord, text)"
                             " VALUES (?, ?, ?)")
                           line-id option-order (cdr (assoc "text" option)))
       (let ((option-id (sqlite3-last-id *sqlite-conn*)))
         (add-option-details option-id option))
       (set! option-order (+ option-order 1024)))
     options))

  (define (add-option-details option-id option)
    (for-each
     (^[flag]
       (execute-query-tree '("INSERT INTO option_flags_required (option_id, flag)"
                             " VALUES (?, ?)")
                           option-id flag))
     (cdr (or (assoc "flags-required" option) (cons 'x #()))))
    (for-each
     (^[dest]
       (execute-query-tree '("INSERT INTO option_jumps (option_id, destination)"
                             " VALUES (?, ?)")
                           option-id dest))
     (cdr (or (assoc "jump-to" option) (cons 'x #())))))

  (define (add-dialog-flags dialog-id flags-req flags-exc flags-set)
    (for-each
     (^[flag]
      (execute-query-tree '("INSERT INTO flags_required (dialog_id, flag)"
                            " VALUES (?, ?)")
                          dialog-id flag))
     flags-req)
    (for-each
     (^[flag]
      (execute-query-tree '("INSERT INTO flags_exclusive (dialog_id, flag)"
                            " VALUES (?, ?)")
                          dialog-id flag))
     flags-exc)
    (for-each
     (^[flag]
      (execute-query-tree '("INSERT INTO flags_set (dialog_id, flag)"
                            " VALUES (?, ?)")
                          dialog-id flag))
     flags-set))

  (define (add-portal dialog-id destination)
    (execute-query-tree '("INSERT OR REPLACE INTO portals (dialog_id, destination)"
                          " VALUES (?, ?)")
                        dialog-id destination))

  (let ((label (cdr (assoc "label" dialog)))
        (location (cdr (assoc "location" dialog)))
        (trigger (cdr (assoc "trigger" dialog)))
        (type (cdr (assoc "type" dialog)))
        (lines (cdr (assoc "lines" dialog)))
        (flags-req (cdr (or (assoc "flags-required" dialog) (cons 'x #()))))
        (flags-exc (cdr (or (assoc "flags-exclusive" dialog) (cons 'x #()))))
        (flags-set (cdr (or (assoc "flags-set" dialog) (cons 'x #())))))
    (execute-query-tree '("INSERT INTO dialogs"
                          " (scenario_id, ord, label, location, type, trigger)"
                          " VALUES (?, ?, ?, ?, ?, ?)")
                        id dialog-order label location type trigger)
    (let ((dialog-id (sqlite3-last-id *sqlite-conn*))
          (line-order 0))
      (add-dialog-flags dialog-id flags-req flags-exc flags-set)
      (when (string=? type "portal")
        (let ((portal-destination (cdr (assoc "portal-destination" dialog))))
          (add-portal dialog-id portal-destination)))
      (for-each (^[line]
                  (set! line-order (add-line line dialog-id line-order)))
                lines))
    ))

(define (convert-scenario-file-to-relations await id)
  (execute-query-tree '("BEGIN TRANSACTION"))
  (guard (e [else (report-error e)
                  (execute-query-tree '("ROLLBACK"))
                  (print #"Transaction failed!")
                  "FAIL!"])

         (with-query-result/tree
          await
          '("SELECT dialog_id FROM dialogs"
            " WHERE scenario_id = ?")
          `(,id)
          (^[rset]
            (for-each
             (^[row]
               (await
                (^[]
                  (let ((dialog-id (vector-ref row 0)))
                    (print #"Found dialog ~|dialog-id| to delete.")
                    (delete-dialog dialog-id)
                    ))))
             rset)))

         (let ((json (read-scenario-file await id))
               (dialog-order 0))
           (for-each
            (^[dialog]
              (convert-dialog-to-relations await id dialog dialog-order)
              (set! dialog-order (+ dialog-order 1024)))
            json)
           )

         (execute-query-tree '("COMMIT"))
         (print #"Transaction done!")
         "DONE AND DONE!"))

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
  )
