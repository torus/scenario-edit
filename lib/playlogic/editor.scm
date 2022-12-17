(define-module playlogic.editor
  (use gauche.collection)

  (use srfi-98)                         ; get-environment-variable

  (use file.util)
  (use rfc.json)
  (use text.csv)
  (use text.tree)
  (use util.match)
  (use rfc.uri)

  (use makiki)

  (use dbi)

  (add-load-path "." :relative)
  (use json-match)
  (use bulma-utils)
  (use playlogic.datastore)

  (export read-and-render-scenario-file
          read-and-render-scenario-file/edit
          read-and-render-scenario-file/insert
          read-and-render-scenario-file/view
          convert-json-to-csv
          update-with-json
          delete-existing-dialogs
          render-location
          render-location-list
          render-location-graph
          create-tables
          convert-scenario-file-to-relations
          scenario-page-header
          location-image-url
          set-ascii-name
          create-page/title
          escape-label

          read-dialog-detail-from-db
          render-dialog-detail-by-dialog-id

          icon-for-type
          ))

(select-module playlogic.editor)

;;
;; Application
;;

(define *session-id* 0)

(define (create-page/title title . children)
  (inc! *session-id*)
  `(html
    (@ (lang "en") (class "has-navbar-fixed-top"))
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

(define (render-line char text options)
  (define (render-option o)
    `(li ,(fas-icon/ "angle-right")
         ,(cdr (assoc "text" o)) " "
         ,@(intersperse " " (map (^f `(span (@ (class "tag is-primary")) ,f))
                                 (cdr (safe-assoc-vec "flags-required" o))))
         " "
         ,@(let ((jump (cdr (safe-assoc-vec "jump-to" o))))
             (if (> (vector-length jump) 0)
                 `((span (@ (class "tag is-info"))
                         ,(fas-icon/ "arrow-circle-right") " "
                         ,(vector-ref jump 0)))
                 ()))
         ))

  `(div (@ (class "columns"))
        (div (@ (class "column is-one-fifth has-text-right pt-0"))
             ,(if (> (string-length char) 0)
                  `(strong ,char)
                  ""))
        (div (@ (class "column pt-0"))
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

(define (escape-label label)
  (uri-encode-string label))

(define (edit-buttons data-id label)
  (let ((escaped-label (escape-label label)))
    `((div (@ (class "column is-1"))
           (form (@ (method "post")
                    (action ,#"/scenarios/~|data-id|/delete"))
                 (input (@ (type "hidden")
                           (name "label")
                           (value ,label)))
                 (button (@ (class "button is-danger"))
                         ,(fas-icon/ "trash-alt"))))
      (div (@ (class "column is-1"))
           (a (@ (class "button")
                 (href ,#"/scenarios/~|data-id|/edit/~|escaped-label|#form"))
              ,(fas-icon/ "edit"))))))

(define (render-dialog-detail-by-dialog-id await dialog-id)
  (let ((detail (read-dialog-detail-by-dialog-id await dialog-id "")))
    (render-dialog-details (cdr (assoc "flags-required" detail))
                           (cdr (assoc "flags-exclusive" detail))
                           (cdr (assoc "flags-set" detail))
                           (cdr (assoc "lines" detail)))))

(define (render-dialog-details flags-req flags-exc flags-set lines)
  `((div (@ (class "columns"))
         (div (@ (class "column is-one-third pt-0"))
              ,(intersperse
                " "
                (map (^f `(span (@ (class "tag is-primary")) ,f))
                     flags-req)))
         (div (@ (class "column is-one-third pt-0"))
              ,(intersperse
                " "
                (map (^f `(span (@ (class "tag is-danger")) ,f))
                     flags-exc)))
         (div (@ (class "column is-one-third pt-0"))
              ,(intersperse
                " "
                (map (^f `(span (@ (class "tag is-info")) ,f))
                     flags-set))))

    ,(render-lines lines)))

(define (render-dialog await conv data-id . additioanl-elements)
  (define (get name)
    (cdr (assoc name conv)))

  (define (show-portal)
    (define dest (get "portal-destination"))
    (render-portal await data-id dest))

  (let ((dialog-id (get "id"))
        (label     (get "label"))
        (loc       (get "location"))
        (type      (get "type"))
        (trigger   (get "trigger"))
        (ord       (get "ord")))
    `((section (@ (class "section") (id ,#"label-~label"))
               ,(container/
                 (render-dialog-header data-id label loc type trigger ord
                                       additioanl-elements)

                 `(div (@ (id ,#"dialog-detail-container-~dialog-id"))
                       (button (@ (class "dialog-detail-button button")
                                  (id ,#"dialog-detail-~|data-id|-~dialog-id"))
                               "読み込み"))

                 (if (string=? type "portal") (show-portal) ())
                 )))))

(define (render-portal await data-id dest)
  `(div (@ (class "columns"))
        (div (@ (class "column"))
             ,(fas-icon/ "walking") " " ,dest " "
             ,(map
               (^[row]
                 (let ((loc (vector-ref row 0)))
                   `(,(fas-icon/ "map-marker-alt")
                     (a (@ (href ,#"/scenarios/~|data-id|/locations/~loc"))
                        ,loc))))
               (query* await
                       '(SELECT DISTINCT "location" FROM "dialogs"
                                WHERE "type" = "portal"
                                AND "trigger" = ?)
                       dest)))))

(define (render-dialog-header data-id label loc type trigger ord
                              additioanl-elements)
  `(div (@ (class "columns is-vcentered"))
        ,@additioanl-elements
        (div (@ (class "column is-1 pt-0"))
             (a (@ (class "button is-white anchor")
                   (href ,#"#label-~label"))
                ,(fas-icon/ "anchor")))
        (div (@ (class "column pt-0"))
             (h4 (@ (class "title is-4"))
                 ,(icon-for-type type) " " ,label))
        (div (@ (class "column is-3 pt-0"))
             (p (a (@ (href ,#"/scenarios/~|data-id|/locations/~loc"))
                   ,loc)
                ,(fas-icon/ "caret-right") ,trigger))
        (div (@ (class "column is-1 pt-0"))
             (p (@ (class "has-text-grey"))
                "0x" ,(number->string ord 16)))))

(define (render-dialog/full await conv data-id . additioanl-elements)
  (define (get name)
    (cdr (assoc name conv)))

  (define (show-portal)
    (define dest (get "portal-destination"))
    (render-portal await data-id dest))

  (let ((dialog-id (get "id"))
        (label     (get "label"))
        (loc       (get "location"))
        (type      (get "type"))
        (trigger   (get "trigger"))
        (ord       (get "ord")))
    `((section (@ (class "section") (id ,#"label-~label"))
               ,(container/
                 (render-dialog-header data-id label loc type trigger ord
                                       additioanl-elements)
                 (render-dialog-detail-by-dialog-id await dialog-id)
                 (if (string=? type "portal") (show-portal) ())
                 )))))

(define (render-dialog/markdown await conv data-id)
  (define (get name)
    (cdr (assoc name conv)))

  (define (show-portal)
    (define dest (get "portal-destination"))
    `(":walking: " ,dest
      ,(map
        (^[row]
          (let ((loc (vector-ref row 0)))
            `(":round_pushpin: " ,loc)))
        (query* await
                '(SELECT DISTINCT "location" FROM "dialogs"
                         WHERE "type" = "portal"
                         AND "trigger" = ?)
                dest))
      "\n"))

  (define (icon-for-type/markdown type)
    (case (string->symbol type)
      ((portal)       " :walking: ")
      ((inspection)   " :mag: ")
      ((conversation) " :speech_balloon: ")
      ((area)         " :black_square_button: ")
      ((message)      " :pager: ")
      (else           " :question: ")))

  (define (render-lines/markdown lines)
    (reverse
     (fold (^[line rest]
             (let ((char (cdr (assoc "character" line)))
                   (text (cdr (assoc "text" line)))
                   (options (let ((opt (assoc "options" line)))
                              (if opt (cdr opt) ()))))
               (cons `("**" ,(if (= 0 (string-length char))
                                 "(blank)"
                                 char)
                       "** "
                       ,text "\n\n"
                       ,(if (zero? (vector-length options))
                            ()
                            `(,(map
                                (^[option]
                                  `("*   :question: "
                                    ,(cdr (assoc "text" option)) "\n"))
                                options)
                              "\n")
                            ))

                     rest)))
           () lines)))

  (define markdown-content
    (let ((label     (get "label"))
          (lines     (get "lines"))
          (loc       (get "location"))
          (type      (get "type"))
          (trigger   (get "trigger"))
          (ord       (get "ord"))
          (flags-req (get "flags-required"))
          (flags-exc (get "flags-exclusive"))
          (flags-set (get "flags-set")))
      `("### " ,(icon-for-type/markdown type) " " ,label " ("
        ,loc " > " ,trigger ")\n\n"

        ,(if (> (vector-length flags-req) 0)
             `(
               "**要求フラグ** "
               ,(intersperse " " (vector->list flags-req))
               "\n"
               )
             ())

        ,(if (> (vector-length flags-exc) 0)
             `(
               "**排他フラグ** "
               ,(intersperse " " (vector->list flags-exc))
               "\n"
               )
             ())

        ,(if (> (vector-length flags-set) 0)
             `(
               "**設定フラグ** "
               ,(intersperse " " (vector->list flags-set))
               "\n"
               )
             ())

        "\n"

        ,(if (string=? type "portal") (show-portal) ())
        ,(render-lines/markdown lines)
        "\n\n"
        )))
  markdown-content
  )

(define (overwrite-markdown-file await id)
  (let ((content (read-dialogs-from-db/full await id)))
    (with-output-to-file (markdown-file-path id)
      (^[]
        (write-tree
         (reverse
          (fold (^[conv rest]
                  (cons (render-dialog/markdown await conv id)
                        rest))
                ()
                content)))))))

(define (render-dialog/edit-button await conv data-id)
  (render-dialog await conv data-id
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
                  ,(fas-icon/ "angle-right"))
             (div (@ (class "column"))
                  (input (@ (class "input option-input") (type "text")
                            (placeholder "選択肢")
                            (value ,(or (get "text" option) ""))))))

        (div (@ (class "columns is-vcentered"))
             (div (@ (class "column is-one-quarter has-text-right"))
                  ,(fas-icon/ "angle-right"))
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
                           ,(fas-icon/ "angle-right")
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
                       ,(fas-icon/ "comment")
                       (span (@ (style "margin-left: 0.5ex"))"セリフを追加"))))))

  `(section
    (@ (class "section")
       (id "form"))
    ,(container/
      `(form (@ (id "edit-form")
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
                  ,(fas-icon/ "comments")
                  (span (@ (style "margin-left: 0.5ex"))"会話を追加"))))))

(define (read-scenario-file await id)
  (await (^[] (with-input-from-file (json-file-path id) parse-json))))

(define (read-dialogs-from-db await id . additional-filter)
  (let ((results (query* await
                         `(SELECT "dialog_id" |,| "label" |,| "location"
                                  |,| "type" |,| "ord" |,| "trigger"
                                  FROM "dialogs"
                                  WHERE "scenario_id" = ?
                                  ,@(if (pair? additional-filter)
                                        `(AND ,additional-filter)
                                        ())
                                  ORDER BY "ord")
                         id)))
    (map-to <vector>
            (cut read-dialog-detail-from-db await <>)
            results)))

(define (read-dialogs-from-db/full await id . additional-filter)
  (let ((results (query* await
                         `(SELECT "dialog_id" |,| "label" |,| "location"
                                  |,| "type" |,| "ord" |,| "trigger"
                                  FROM "dialogs"
                                  WHERE "scenario_id" = ?
                                  ,@(if (pair? additional-filter)
                                        `(AND ,additional-filter)
                                        ())
                                  ORDER BY "ord")
                         id)))
    (map-to <vector>
            (cut read-dialog-detail-from-db/full await <>)
            results)))

(define (read-dialog-detail-by-dialog-id await dialog-id type)
  `(("lines" .
     ,(map-to <vector>
              (^[row]
                (read-line-from-db await row))
              (query* await
                      '(SELECT "line_id" |,| "character" |,| "text" |,| "ord"
                               FROM "lines"
                               WHERE "dialog_id" = ?
                               ORDER BY "ord")
                      dialog-id)))
    ("flags-required" .
     ,(map-to <vector> (^[row] (read-flags-from-db await row))
              (query* await
                      '(SELECT "flag"
                               FROM "flags_required"
                               WHERE "dialog_id" = ?)
                      dialog-id)))
    ("flags-exclusive" .
     ,(map-to <vector> (^[row] (read-flags-from-db await row))
              (query* await
                      '(SELECT "flag"
                               FROM "flags_exclusive"
                               WHERE "dialog_id" = ?)
                      dialog-id)))
    ("flags-set" .
     ,(map-to <vector> (^[row] (read-flags-from-db await row))
              (query* await
                      '(SELECT "flag"
                               FROM "flags_set"
                               WHERE "dialog_id" = ?)
                      dialog-id)))

    )
  )

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
      ,@(if (string=? typ "portal")
            `(("portal-destination" .
               ,(car (map (^[row] (vector-ref row 0))
                          (query* await
                                  '(SELECT "destination"
                                           FROM "portals"
                                           WHERE "dialog_id" = ?)
                                  dialog-id)))))
            ())
      )))

(define (read-dialog-detail-from-db/full await row)
  (append (read-dialog-detail-from-db await row)
          (read-dialog-detail-by-dialog-id await
                                           (vector-ref row 0)
                                           (vector-ref row 3))))

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
       ,(map-to <vector>
                (^[row]
                  (read-option-from-db await row))
                (query* await
                        '(SELECT "option_id" |,| "text" FROM "options"
                                 WHERE "line_id" = ?
                                 ORDER BY "ord")
                        line-id))))))

(define (read-option-from-db await row)
  (let ((option-id (vector-ref row 0))
        (text (vector-ref row 1)))
    `(("text" . ,text)
      ("flags-required" .
       ,(map-to <vector> (^[row] (vector-ref row 0))
                (query* await
                        '(SELECT "flag" FROM "option_flags_required"
                                 WHERE "option_id" = ?)
                        option-id)))
      ("jump-to" .
       ,(map-to <vector> (^[row] (vector-ref row 0))
                (query* await
                        '(SELECT "destination" FROM "option_jumps"
                                 WHERE "option_id" = ?)
                        option-id))))))

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
               (values (cons (render-dialog/edit-button await conv id)
                             (cons (add-dialog-button id prev-label ord)
                                   rest))
                       conv)))
           () #f
           content))
  (let ((content (read-dialogs-from-db await id)))
    (if (zero? (vector-length content))
        `(,(add-dialog-button id #f 1024))
        (let-values (((convs prev-conv)
                      (elems content)))
          `(,@(reverse convs)
            ,(add-dialog-button id (label-of prev-conv)
                                (+ 1024 (ord-of prev-conv))))))
    ))

(define (read-and-render-scenario-file/view await id)
  (let ((content (read-dialogs-from-db await id)))
    (reverse
     (fold (^[conv rest]
             (let ((label (cdr (assoc "label" conv))))
               (cons (render-dialog/full await conv id)
                     rest)))
           ()
           content))))

(define (json-file-path data-id)
  #"json/~|data-id|.json")

(define (dialog-json-file-path data-id)
  #"json/~|data-id|-dialogs.json")

(define (triggermap-json-file-path data-id)
  #"json/~|data-id|-triggermap.json")

(define (markdown-file-path data-id)
  #"markdown/~|data-id|.md")

(define (read-and-render-scenario-file/insert await id ord)
  (define (new-form ord)
    (render-dialog-form #f id
                        `(input (@ (id "ord-input")
                                   (type "hidden")
                                   (value ,ord)))))
  (let ((content (read-dialogs-from-db await id)))
    (reverse
     (let-values (((result inserted?)
                   (fold2 (^[conv rest inserted?]
                            (let ((next-ord (cdr (assoc "ord" conv))))
                              (if (and (not inserted?) (> next-ord ord))
                                  (values (cons (render-dialog await conv id)
                                                (cons (new-form ord) rest))
                                          #t)
                                  (values (cons (render-dialog await conv id) rest)
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

  (let ((content (read-dialogs-from-db/full await id)))
    (reverse
     (fold (^[conv rest]
             (let ((label (cdr (assoc "label" conv))))
               (cons (if (string=? label label-to-edit)
                         (new-form conv label)
                         (render-dialog await conv id))
                     rest)))
           ()
           content))))

(define (scenario-page-header await id)
  (let ((title (match
                (query* await '(SELECT "title" FROM "scenarios"
                                       WHERE "scenario_id" = ?) id)
                ((#(title)) title)
                (() "Untitled Scenario"))))
    (navbar/
     await id
     title
     `(div (@ (id "playlogic-navbar") (class "navbar-menu"))
           (div (@ (class "navbar-start"))
                (a (@ (class "navbar-item")
                      (href ,#`"/scenarios/,|id|"))
                   ,(fas-icon/ "home") (span "Home"))
                (a (@ (class "navbar-item")
                      (href ,#`"/scenarios/,|id|/locations"))
                   ,(fas-icon/ "map-marked-alt") (span "Locations"))
                (a (@ (class "navbar-item")
                      (href ,#`"/scenarios/,|id|/view/"))
                   ,(fas-icon/ "eye") (span "View Mode"))
                (a (@ (class "navbar-item"))
                   (div (@ (class "buttons"))
                        (a (@ (class "button is-primary")
                              (href ,#`"/scenarios/,|id|/play/,*session-id*"))
                           ,(fas-icon/ "gamepad") (span "Play")))))

           (div (@ (class "navbar-end"))
                (div (@ (class "navbar-item has-dropdown is-hoverable"))
                     (a (@ (class "navbar-link"))
                        ,(fas-icon/ "hammer")
                        (span "Admin"))
                     (div (@ (class "navbar-dropdown is-right"))
                          (a (@ (class "navbar-item")
                                (href ,#`"/scenarios/,|id|/update-csv"))
                             ,(fas-icon/ "save")
                             (span "Update CSV/JSON/Markdown"))
                          (a (@ (class "navbar-item")
                                (href ,#`"/scenarios/,|id|/convert"))
                             ,(fas-icon/ "skull-crossbones")
                             (span "Convert from JSON"))
                          )))))))

(define (overwrite-json-file await new-json filename)
  (await (^[]
           (call-with-temporary-file
               (^[port tmpfile]
                 (with-output-to-port port
                   (^[]
                     (guard (e [else (report-error e)])
                            (construct-json new-json))
                     (flush)))
                 (sys-system #"jq '. | del(.dialogs[].id) | del(.dialogs[].ord) | del(.dialogs[].lines[].id) | del(.dialogs[].lines[].ord)' < ~tmpfile > ~filename"))
             :directory "json")
           'done)))

(define (overwrite-dialog-json-file await dialogs-json filename)
  (let ((modified-for-unreal
         (map-to <vector>
                 (^e (acons "Name" (cdr (assoc "label" e)) e)) dialogs-json)))
    (await (^[]
             (call-with-temporary-file
                 (^[port tmpfile]
                   (with-output-to-port port
                     (^[]
                       (guard (e [else (report-error e)])
                              (construct-json modified-for-unreal))
                       (flush)))
                   (sys-system #"jq '[.[] | {Name,\"label\",type,location,trigger,\"flags-required\",\"flags-exclusive\",\"flags-set\",lines:[.lines[]|{character,text,options}]} ]' < ~tmpfile > ~filename"))
               :directory "json")
             'done))))

(define (delete-dialog await dialog-id)
  (query* await
          '(DELETE FROM "option_flags_required"
                   WHERE "option_id" IN
                   (SELECT "option_id" FROM "options" WHERE "line_id" IN
                           (SELECT "line_id" FROM "lines"
                                   WHERE "dialog_id" = ?)))
          dialog-id)

  (query* await
          '(DELETE FROM "option_jumps"
                   WHERE "option_id" IN
                   (SELECT "option_id" FROM "options" WHERE "line_id" IN
                           (SELECT "line_id" FROM "lines"
                                   WHERE "dialog_id" = ?)))
          dialog-id)

  (query* await
          '(DELETE FROM "options"
                   WHERE "line_id" IN
                   (SELECT "line_id" FROM "lines" WHERE "dialog_id" = ?))
          dialog-id)

  (query* await '(DELETE FROM "lines" WHERE "dialog_id" = ?) dialog-id)

  (query* await '(DELETE FROM "flags_required" WHERE "dialog_id" = ?) dialog-id)
  (query* await '(DELETE FROM "flags_exclusive" WHERE "dialog_id" = ?) dialog-id)
  (query* await '(DELETE FROM "flags_set" WHERE "dialog_id" = ?) dialog-id)
  (query* await '(DELETE FROM "dialogs" WHERE "dialog_id" = ?) dialog-id))

(define (delete-existing-dialogs await data-id label)
  (define last-order 0)
  (for-each
   (^[row]
     (await
      (^[]
        (let ((dialog-id (vector-ref row 0))
              (ord (vector-ref row 1)))
          (print #"Found dialog ~|dialog-id| to delete.")

          (delete-dialog await dialog-id)

          (set! last-order ord)

          ))))
   (query* await
           '(SELECT "dialog_id" |,| "ord" FROM "dialogs"
                    WHERE "label" = ? AND "scenario_id" = ?)
           label data-id))
  last-order)

(define (update-existing-dialog await data-id form-data)
  (define (get name)
    (let ((val (assoc name form-data)))
      (if val
          (cdr val)
          (error #"paramter not specified: ~|name|"))))

  (let ((orig-label (get "original-label")))
    (query* await '(BEGIN TRANSACTION))
    (guard (e [else (report-error e)
                    (query* await '(ROLLBACK))
                    (print #"Transaction failed!")])
           (let ((ord (delete-existing-dialogs await data-id orig-label)))
             (convert-dialog-to-relations await data-id form-data ord))
           (query* await '(COMMIT))
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
         (update-existing-dialog await data-id form-data)))
  (cdr (assoc "label" form-data)))

(define (convert-json-to-csv await data-id)
  (define (get-location-names)
    (let ((results
           (query* await
                   '(SELECT DISTINCT "d" |.| "location" |,| "a" |.| "ascii"
                            FROM  "dialogs" "d" |,| "ascii_names" "a"
                            WHERE "d" |.| "scenario_id" = ?
                            AND   "a" |.| "scenario_id" = "d" |.| "scenario_id"
                            AND   "d" |.| "location" = "a" |.| "original"
                            ORDER BY "a" |.| "ascii")
                   data-id)))
      (map-to <vector>
              (match-lambda (#(loc ascii)
                             `(("label" . ,loc)
                               ("ascii" . ,ascii))))
              results)))

  (define (get-title)
    (let ((results
           (query* await
                   '(SELECT "title"
                            FROM  "scenarios"
                            WHERE "scenario_id" = ?
                            LIMIT 1)
                   data-id)))
      (match results
             ((#(title)) title)
             (()         "Untitled"))))

  (let ((dialogs-json (read-dialogs-from-db/full await data-id))
        (locations-json (get-location-names))
        (title (get-title)))
    (overwrite-json-file await
                         `(("version" . "1.0")
                           ("title" . ,title)
                           ("locations" . ,locations-json)
                           ("dialogs" . ,dialogs-json))
                         (json-file-path data-id))

    (overwrite-markdown-file await data-id)

    (overwrite-dialog-json-file await dialogs-json
                                (dialog-json-file-path data-id))

    (let ((dialog-port (open-output-file "csv/Dialogs.csv"))
          (line-port (open-output-file "csv/Dialogs_lines.csv"))
          (option-port (open-output-file "csv/Dialogs_options.csv"))
          (flag-port (open-output-file "csv/Dialogs_flags.csv"))
          (triggermap-port (open-output-file "csv/Dialogs_triggermap.csv"))
          (portal-port (open-output-file "csv/Dialogs_portals.csv")))
      (json-match dialogs-json
                  (write-with-csv-writers line-port option-port
                                          dialog-port flag-port
                                          triggermap-port portal-port))

      (for-each close-port (list line-port option-port dialog-port
                                 flag-port triggermap-port portal-port)))
    "Conversion done!")

  (let ((trigger-map (make-triggermap-from-db await data-id))
        (filename (triggermap-json-file-path data-id)))
    (call-with-temporary-file
        (^[port tmpfile]
          (with-output-to-port port
            (^[]
              (guard (e [else (report-error e)])
                     (construct-json trigger-map))
              (flush)))
          (sys-system #"jq '.' < ~tmpfile > ~filename"))
      :directory "json"))

  "Done!")

(define (make-triggermap-from-db await data-id)
  (let ((dialog-ht (make-hash-table string-comparator))
        (results
         (query* await
                 `(SELECT "label" |,| "location" |,| "trigger"
                          FROM "dialogs"
                          WHERE "scenario_id" = ,data-id))))

    (for-each
     (match-lambda
      (#(label loc trig)
       (unless (hash-table-exists? dialog-ht loc)
         (hash-table-put! dialog-ht loc (make-hash-table string-comparator))
         (hash-table-put! (hash-table-get dialog-ht loc) "Name" loc)
         (hash-table-put! (hash-table-get dialog-ht loc)
                          "triggers" (make-hash-table string-comparator)))

       (let ((loc-ht (hash-table-get (hash-table-get dialog-ht loc)
                                     "triggers")))
         (unless (hash-table-exists? loc-ht trig)
           (hash-table-put! loc-ht trig (make-hash-table string-comparator))
           (hash-table-put! (hash-table-get loc-ht trig) "labels" #()))
         (hash-table-put! (hash-table-get loc-ht trig)
                          "labels"
                          (vector-append
                           (hash-table-get
                            (hash-table-get loc-ht trig) "labels")
                           `#(,label))))))
     results)

    (list->vector (hash-table-map dialog-ht (^[key val] val)))))


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

  (define (process-lines % @ label)
    (% "lines"
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
           d)))))

  (define (process-dialog % @)
    (^d
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
           (@ (^d (w-flag `(,#"~|label|_~|flagcount|" "required" ,d))
                  (inc! flagcount)))) d)
       ((% "flags-exclusive"
           (@ (^d (w-flag `(,#"~|label|_~|flagcount|" "exclusive" ,d))
                  (inc! flagcount)))) d)
       ((% "flags-set"
           (@ (^d (w-flag `(,#"~|label|_~|flagcount|" "set" ,d))
                  (inc! flagcount)))) d)

       (w-dialog `(,label ,type ,location ,trigger
                          ,(x->string linecount) ,(x->string flagcount)))

       ((process-lines % @ label) d)

       ((% "portal-destination"
           (^d (w-portal `(,label ,d)))) d)

       )))

  (define (write-header)
    (w-dialog  '("" "type" "location" "trigger" "count" "flagcount"))
    (w-option  '("" "option" "flagcount" "jump"))
    (w-line    '("" "character" "text" "options"))
    (w-flag    '("" "type" "flag"))
    (w-trigger '("" "dialog"))
    (w-portal  '("" "destination")))

  (write-header)

  (^[% @] (@ (process-dialog % @))))

(define (icon-for-type type)
  (fas-icon/
   (case (string->symbol type)
     ((portal)       "walking")
     ((inspection)   "search")
     ((conversation) "comment")
     ((area)         "sign-in-alt")
     ((message)      "pager")
     (else           "question"))))

(define (get-ascii-name await data-id loc)
  (let ((results (query* await `(SELECT "ascii" FROM "ascii_names"
                                        WHERE "original" = ?
                                        AND "scenario_id" = ?)
                         loc data-id)))
    (if (pair? results)
        (vector-ref (car results) 0)
        "")))

(define (set-ascii-name await data-id original ascii)
  (query* await
          `(INSERT OR REPLACE INTO "ascii_names"
                   ("original" |,| "scenario_id" |,| "ascii")
                   VALUES (? |,| ? |,| ?))
          original data-id ascii))

(define (render-location await data-id loc)
  (define (get name conv)
    (cdr (assoc name conv)))

  (define (show-portal-destination label)
    (let ((results
           (query*
            await
            `(SELECT DISTINCT "d2" |.| "location"
                     FROM "dialogs" "d1" |,| "dialogs" "d2" |,| "portals" "p"
                     WHERE "d1" |.| "label" = ?
                     AND "p" |.| "dialog_id" = "d1" |.| "dialog_id"
                     AND "p" |.| "destination" = "d2" |.| "trigger")
            label)))
      (map
       (^[row]
         (let ((loc (vector-ref row 0)))
           `(" "
             ,(fas-icon/ "arrow-right")
             (a (@ (href ,#"/scenarios/~|data-id|/locations/~loc"))
                ,loc
                ))))
       results)))

  (define (show-record conv)
    (let ((label     (get "label" conv))
          (flags-req (get "flags-required" conv))
          (flags-exc (get "flags-exclusive" conv))
          (flags-set (get "flags-set" conv))
          (type      (get "type" conv))
          (trigger   (get "trigger" conv)))
      `(tr
        (td ,(icon-for-type (get "type" conv))
            " " ,trigger)
        (td (a (@ (href
                   ,#"/scenarios/~|data-id|#label-~label"))
               ,label)

            ,@(if (string=? type "portal")
                  (show-portal-destination label)
                  '())

            )
        (td
         ,(intersperse
           " "
           (append
            (map (^f `(span (@ (class "tag is-primary")) ,f))
                 flags-req)
            (map (^f `(span (@ (class "tag is-danger")) ,f))
                 flags-exc)
            (map (^f `(span (@ (class "tag is-info")) ,f))
                 flags-set)))))))

  (define (ascii-name-form ascii)
    `(form (@ (method "post")
              (action ,#"/scenarios/~|data-id|/update-ascii-name"))
           (div (@ (class "field is-horizontal"))
                (div (@ (class "field-label is-normal"))
                     (label (@ (class "label")) "ASCII name"))
                (div (@ (class "field-body"))
                     (div (@ (class "field"))
                          (div (@ (class "control"))
                               (input (@ (name "input-original")
                                         (type "hidden")
                                         (value ,loc)))
                               (input (@ (name "input-ascii")
                                         (class "input")
                                         (type "text")
                                         (placeholder "name")
                                         (value ,ascii)))))
                     (div (@ (class "field"))
                          (div (@ (class "control"))
                               (button (@ (class
                                           "button is-primary"))
                                       "更新")))))))

  (define (trigger-list data-id loc)
    (let ((triggers (query* await
                            `(SELECT DISTINCT "type" |,| "trigger" FROM "dialogs"
                                     WHERE "scenario_id" = ,data-id
                                     AND "location" = ,loc
                                     ORDER BY "type"))))
      (map (^[row]
             (match row
                    (#(typ trg)
                     `(button (@ (class "button"))
                              ,(icon-for-type typ) " " (span ,trg)))))
           triggers)))

  (let ((content (read-dialogs-from-db await data-id "location" '= loc))
        (ascii   (get-ascii-name await data-id loc)))
    `(div (@ (class "container"))
          ((div (@ (class "block"))
                (h2 (@ (class "title is-4"))
                    ,(fas-icon/ "map-marker-alt") " " ,loc)
                (div (@ (class "columns"))
                     (div (@ (class "column"))
                          ,(ascii-name-form ascii)
                          ))
                (div (@ (class "columns"))
                     (div (@ (class "column")) "")
                     (div (@ (class "column is-6"))
                          (img (@ (src ,(location-image-url await data-id
                                                            loc)))))
                     (div (@ (class "column")) ""))
                (div (@ (class "columns"))
                     (div (@ (class "column"))
                          (p (@ (class "buttons"))
                             ,(trigger-list data-id loc))))
                (table (@ (class "table"))
                       (tr (th "Trigger") (th "Label") (th "Flags"))
                       ,(map
                         (^[conv]
                           (show-record conv))
                         content)))))))

(define cdn-domain
  (let ((var (get-environment-variable "CDN_HOST")))
    (if var
        #"//~var"
        "")))

(define (location-image-url await data-id loc)
  (let ((ascii-name (get-ascii-name await data-id loc)))
    #"~|cdn-domain|/static/gameassets/~|data-id|/images/locations/~|ascii-name|.jpg"))

(define (render-location-graph await data-id)
  (define (render-links-page links)
    `(,(scenario-page-header await data-id)
      (pre (@ (id "dot")
              (class "is-hidden"))
           "digraph {"
           ,(map
             (^[row]
               (let ((from  (vector-ref row 0))
                     (to    (vector-ref row 1))
                     (label (vector-ref row 2)))
                 #"~from [shape=\"box\"]\n~from -> ~to [label=~|label| fontsize=9];\n"))
             links)
           "}")
      ,(container/
        `(div (@ (id "graph")) ""))))

  (let ((links (query*
                await
                '(SELECT DISTINCT "d1" |.| "location" |,| "d2" |.| "location"
                         |,| "d1" |.| "trigger"
                         FROM "dialogs" "d1"
                         |,| "dialogs" "d2"
                         |,| "portals" "p"
                         WHERE "p" |.| "dialog_id" = "d1" |.| "dialog_id"
                         AND "p" |.| "destination" = "d2" |.| "trigger"
                         AND "d1" |.| "type" = "portal"
                         AND "d2" |.| "type" = "portal"
                         AND "d1" |.| "scenario_id" = ?
                         AND "d2" |.| "scenario_id" = ?
                         ORDER BY "d1" |.| "location" |,| "d2" |.| "location"
                         )
                data-id data-id)))

    (list #"Location Graph #~data-id"
          (render-links-page links)
          `((script (@ (src "/static/viz.js")) "")
            (script (@ (src "/static/full.render.js")) "")
            (script "  var viz = new Viz();\n"
                    "  var graph = document.querySelector('#dot').innerText;\n"
                    "  viz.renderSVGElement(graph)\n"
                    "  .then(function(element) {\n"
                    "    document.querySelector('#graph').appendChild(element);\n"
                    "  })\n"
                    "  .catch(function(error) {\n"
                    "    viz = new Viz();\n"
                    "    // Possibly display the error\n"
                    "    console.error(error);\n"
                    "  });\n"
                    )))))

;; <script src="viz.js"></script>
;; <script src="full.render.js"></script>
;; <script>
;;   var viz = new Viz();

;;   viz.renderSVGElement('digraph { a -> b }')
;;   .then(function(element) {
;;     document.body.appendChild(element);
;;   })
;;   .catch(error => {
;;     // Create a new Viz instance (@see Caveats page for more info)
;;     viz = new Viz();

;;     // Possibly display the error
;;     console.error(error);
;;   });
;; </script>


(define (render-location-list await data-id)
  (define (show-location loc&ascii)
    (let ((loc (car loc&ascii))
          (ascii (or (cadr loc&ascii) '(span (@ (class "tag is-danger"))
                                             "ASCII NOT SPECIFIED"))))
      `(li (a (@ (href ,#"/scenarios/~|data-id|/locations/~loc"))
              ,(fas-icon/ "map-marker-alt") " " ,loc
              " (" ,ascii ")"))))

  (let* ((results
          (query* await
                  `(SELECT DISTINCT "d" |.| "location" |,| "a" |.| "ascii"
                           FROM "dialogs" "d"
                           LEFT OUTER JOIN "ascii_names" "a"
                           ON "d" |.| "location" = "a" |.| "original"
                           WHERE "d" |.| "scenario_id" = ,data-id
                           ORDER BY "d" |.| "location"))))
    (container/
     `(div (@ (class "columns"))
           (div (@ (class "column"))
                (h2 (@ (class "title"))
                    ,(fas-icon/ "map-marked-alt") " Locations")
                (p (a (@ (href ,#"/scenarios/~|data-id|/location-graph"))
                      "Location Graph"))
                (div (@ (class "menu"))
                     (ul (@ (class "menu-list"))
                         ,@(map
                            show-location
                            (map vector->list results)))))))))

(define (convert-dialog-to-relations await id dialog dialog-order)
  (define (add-line line dialog-id line-order)
    (let ((character (cdr (assoc "character" line)))
          (text (cdr (assoc "text" line)))
          (options (let1 opt (assoc "options" line)
                     (if opt (cdr opt) ()))))
      (let ((rset
             (query* await
                     '(INSERT INTO "lines"
                              ("dialog_id" |,| "ord"
                               |,| "character" |,| "text")
                              VALUES (? |,| ? |,| ? |,| ?)
                              |;| SELECT last_insert_rowid())
                     dialog-id line-order character text)))
        (let ((line-id (vector-ref (car rset) 0))
              (option-order 0))
          (add-line-details line-id option-order line options))
        (+ line-order 1024)
        )))

  (define (add-line-details line-id option-order line options)
    (for-each
     (^[option]
       (let ((rset
              (query* await
                      '(INSERT INTO "options" ("line_id" |,| "ord" |,| "text")
                               VALUES (? |,| ? |,| ?)
                               |;| SELECT last_insert_rowid())
                      line-id option-order (cdr (assoc "text" option)))))
         (let ((option-id (vector-ref (car rset) 0)))
           (add-option-details option-id option)))
       (set! option-order (+ option-order 1024)))
     options))

  (define (add-option-details option-id option)
    (for-each
     (^[flag]
       (query* await
               '(INSERT INTO "option_flags_required" ("option_id" |,| "flag")
                        VALUES (? |,| ?))
               option-id flag))
     (cdr (or (assoc "flags-required" option) (cons 'x #()))))
    (for-each
     (^[dest]
       (query* await
               '(INSERT INTO "option_jumps" ("option_id" |,| "destination")
                        VALUES (? |,| ?))
               option-id dest))
     (cdr (or (assoc "jump-to" option) (cons 'x #())))))

  (define (add-dialog-flags dialog-id flags-req flags-exc flags-set)
    (for-each
     (^[flag]
       (query* await
               '(INSERT INTO "flags_required" ("dialog_id" |,| "flag")
                        VALUES (? |,| ?))
               dialog-id flag))
     flags-req)
    (for-each
     (^[flag]
       (query* await
               '(INSERT INTO "flags_exclusive" ("dialog_id" |,| "flag")
                        VALUES (? |,| ?))
               dialog-id flag))
     flags-exc)
    (for-each
     (^[flag]
       (query* await
               '(INSERT INTO "flags_set" ("dialog_id" |,| "flag")
                        VALUES (? |,| ?))
               dialog-id flag))
     flags-set))

  (define (add-portal dialog-id destination)
    (query* await
            '(INSERT OR REPLACE INTO "portals" ("dialog_id" |,| "destination")
                     VALUES (? |,| ?))
            dialog-id destination))

  (let ((label (cdr (assoc "label" dialog)))
        (location (cdr (assoc "location" dialog)))
        (trigger (cdr (assoc "trigger" dialog)))
        (type (cdr (assoc "type" dialog)))
        (lines (cdr (assoc "lines" dialog)))
        (flags-req (cdr (or (assoc "flags-required" dialog) (cons 'x #()))))
        (flags-exc (cdr (or (assoc "flags-exclusive" dialog) (cons 'x #()))))
        (flags-set (cdr (or (assoc "flags-set" dialog) (cons 'x #())))))
    (let ((rset
           (query* await
                   '(INSERT INTO "dialogs"
                            ("scenario_id" |,| "ord" |,| "label"
                             |,| "location" |,| "type" |,| "trigger")
                            VALUES (? |,| ? |,| ? |,| ? |,| ? |,| ?)
                            |;| SELECT last_insert_rowid())
                   id dialog-order label location type trigger)))
      (let ((dialog-id (vector-ref (car rset) 0))
            (line-order 0))
        (add-dialog-flags dialog-id flags-req flags-exc flags-set)
        (when (string=? type "portal")
          (let ((portal-destination (cdr (assoc "portal-destination" dialog))))
            (add-portal dialog-id portal-destination)))
        (for-each (^[line]
                    (set! line-order (add-line line dialog-id line-order)))
                  lines)))))

(define (convert-scenario-file-to-relations await id)
  (define (put-dialogs json)
    (let ((dialog-order 0))
      (for-each
       (^[dialog]
         (convert-dialog-to-relations await id dialog dialog-order)
         (set! dialog-order (+ dialog-order 1024)))
       (json-query json '("dialogs")))))

  (define (put-title json)
    (query* await
            '(INSERT OR REPLACE INTO "scenarios" ("scenario_id" |,| "title")
                     VALUES (? |,| ?))
            id (json-query json '("title"))))

  (define (put-ascii-names json)
    (when (> (vector-length (json-query json '("locations"))) 0)
      (query* await
              `(DELETE FROM "ascii_names" WHERE "scenario_id" = ,(x->number id)
                       |;|
                       INSERT INTO "ascii_names"
                       ("original" |,| "scenario_id" |,| "ascii")
                       VALUES
                       ,@(match (json-query json '("locations"))
                                (#((("label" . label) ("ascii" . ascii)) ...)
                                 (intersperse
                                  '|,| (map (^[l a]
                                              `(,l |,| ,(x->number id) |,| ,a))
                                            label ascii))))))))

  (query* await '(BEGIN TRANSACTION))
  (guard (e [else (report-error e)
                  (query* await '("ROLLBACK"))
                  (print #"Transaction failed!")
                  "FAIL!"])
         (let ((rset
                (query* await
                        '(SELECT "dialog_id" FROM "dialogs"
                                 WHERE "scenario_id" = ?)
                        id)))
           (for-each
            (^[row]
              (await
               (^[]
                 (let ((dialog-id (vector-ref row 0)))
                   (print #"Found dialog ~|dialog-id| to delete.")
                   (delete-dialog await dialog-id)
                   ))))
            rset))

         (let ((json (read-scenario-file await id)))
           (put-title json)
           (put-dialogs json)
           (put-ascii-names json))
         (query* await '(COMMIT))
         (print #"Transaction done!")
         "DONE AND DONE!"))
