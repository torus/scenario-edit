(use gauche.threads)
(use gauche.collection)

(use file.util)
(use rfc.http)
(use rfc.json)
(use sxml.tools)
(use text.csv)

(add-load-path "./gauche-rheingau/lib/")
(use rheingau)
(rheingau-use makiki)

(add-load-path "./lib/")
(use violet)
(use json-match)

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

(define (render-line char text)
  `(div (@ (class "columns"))
        (div (@ (class "column is-one-fifth has-text-right"))
             ,char)
        (div (@ (class "column"))
             ,text)))

(define (render-lines lines)
  (reverse
   (fold (^[line rest]
           (let ((char (cdr (assoc "character" line)))
                 (text (cdr (assoc "text" line))))
             (cons (render-line char text) rest)))
         () lines)))


(define (render-conversation/edit-button conv data-id)
  (define (get name)
    (cdr (assoc name conv)))

  (let ((label (get "label"))
        (lines (get "lines")))
    `(section (@ (class "section") (id ,#"label-~label"))
              (div (@ (class "container"))
                   (div (@ (class "columns is-vcentered"))
                        (div (@ (class "column is-1"))
                             (form (@ (method "post")
                                      (action ,#"/scenarios/~|data-id|/delete"))
                                   (input (@ (type "hidden")
                                             (name "label")
                                             (value ,label)))
                                   (button (@ (class "button is-danger"))
                                           (span (@ (class "icon"))
                                                 (i (@ (Class "fas fa-trash-alt")) "")))))
                        (div (@ (class "column is-1"))
                             (a (@ (class "button")
                                   (href ,#"/scenarios/~|data-id|/edit/~|label|#form"))
                                (span (@ (class "icon"))
                                      (i (@ (Class "fas fa-edit")) ""))))
                        (div (@ (class "column is-2"))
                             (span (@ (class "tag is-info"))
                                   ,(get "type")))
                        (div (@ (class "column is-one-third"))
                             (h4 (@ (class "title is-4")) ,label))
                        (div (@ (class "column"))
                             (p ,(get "section"))))
                   ,@(render-lines lines)))))

(define (render-conversation conv data-id)
  (define (get name)
    (cdr (assoc name conv)))

  (let ((label (get "label"))
        (lines (get "lines")))
    `(section (@ (class "section"))
              (div (@ (class "container"))
                   (div (@ (class "columns is-vcentered"))
                        (div (@ (class "column is-one-third"))
                             (h4 (@ (class "title is-4")) ,label))
                        (div (@ (class "column"))
                             (p ,(get "section"))))
				   ,@(render-lines lines)))))

(define (not-empty? str) (and str (not (zero? (string-length str)))))

(define (form-field label help input)
  `(div (@ (class "field"))
        ,(if (not-empty? label) `(label (@ (class "label")) ,label) ())
        (div (@ (class "control")) ,input)
        ,(if (not-empty? help) `(p (@ (class "help")) ,help) ())))


(define (render-line-form char text)
  `(div (@ (class "columns line-form"))
        (div (@ (class "column is-one-fifth has-text-right"))
             (input (@ (class "input character-input")
                       (type "text")
                       (placeholder "キャラクター")
                       (value ,char))))
        (div (@ (class "column"))
             (input (@ (class "input line-input") (type "text") (placeholder "セリフ")
                       (value ,text))))))

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
                                       (value ,(get "section" ""))))))))

  `(section (@ (class "section")
               (id "form"))
            (div (@ (class "container"))
                 (form (@ (id "edit-form")
                          (action ,#"/scenarios/~|data-id|/submit/~label")
                          (method "post"))
                       ,hidden-inputs
                       ,(conv-form)
                       (div (@ (id "line-fields"))
                            ,(reverse
                              (fold (^[line rest]
                                      (let ((char (cdr (assoc "character" line)))
                                            (text (cdr (assoc "text" line))))
                                        (cons (render-line-form char text) rest)))
                                    () lines)))
                       (div (@ (class "field has-text-centered"))
                            (a (@ (class "button")
                                  (id "add-line-button"))
                               (span (@ (class "icon")) (i (@ (Class "fas fa-plus")) ""))
                               ))
                       (div (@ (class "field is-grouped is-grouped-right"))
                            (p (@ (class "control"))
                               (button (@ (class "button is-primary")) "更新"))
                            (p (@ (class "control"))
                               (a (@ (class "button is-light")
                                     (href ,#"/scenarios/~data-id"))
                                  "キャンセル")))))
            (div (@ (style "display: none")
                    (id "hidden-field"))
                 ,(render-line-form "" ""))))

(define (add-conversation-button data-id prev-label)
  `(div (@ (class "columns"))
        (div (@ (class "column has-text-centered"))
             (a (@ (class "button")
                   (href ,#"/scenarios/~|data-id|/insert/~|prev-label|#form"))
                (span (@ (class "icon")) (i (@ (Class "fas fa-plus")) ""))
                     ))))

(define (read-and-render-scenario-file id)
  (let ((filename (json-file-path id)))
    (with-input-from-file filename
      (^()
        (let ((content (parse-json))
              (prev-label ""))
          (append (reverse
                   (fold (^[conv rest]
                           (let ((elem
                                  (cons (render-conversation/edit-button conv id)
                                        (cons (add-conversation-button id prev-label) rest))))
                             (set! prev-label (cdr (assoc "label" conv)))
                             elem))
                         ()
                         content))
                  (list (add-conversation-button id prev-label))))))))

(define (json-file-path data-id)
  #"json/~|data-id|.json")

(define (read-and-render-scenario-file/insert id prev-label)
  (let ((filename (json-file-path id)))
    (with-input-from-file filename
      (^()
        (let ((content (parse-json)))
          (reverse
           (fold (^[conv rest]
                   (let ((label (cdr (assoc "label" conv))))
                     (if (string=? label prev-label)
                         (cons (render-conversation-form #f id
                                                         `(input (@ (id "prev-label-input")
                                                                    (type "hidden")
                                                                    (value ,label))))
                               (cons (render-conversation conv id) rest))
                         (cons (render-conversation conv id) rest))))
                 (if (string=? prev-label "")
                     (list
                      (render-conversation-form #f id
                                                `(input (@ (id "prev-label-input")
                                                           (type "hidden")
                                                           (value "")))))
                     ())
                 content)))))))

(define (read-and-render-scenario-file/edit id label-to-edit)
  (let ((filename (json-file-path id)))
    (with-input-from-file filename
      (^()
        (let ((content (parse-json)))
          (reverse
           (fold (^[conv rest]
                   (let ((label (cdr (assoc "label" conv))))
                     (cons (if (string=? label label-to-edit)
                               (render-conversation-form conv id
                                                         `(input (@ (id "original-label-input")
                                                                    (type "hidden")
                                                                    (value ,label))))
                               (render-conversation conv id))
                           rest)))
                 ()
                 content)))))))

(define-http-handler #/^\/scenarios\/(\d+)$/
  (^[req app]
    (violet-async
     (^[await]
       (let-params req ([id "p:1"])
                   (let ((rendered (read-and-render-scenario-file id)))
                     (respond/ok req (cons "<!DOCTYPE html>"
                                           (sxml:sxml->html
                                            (create-page
                                             rendered
                                             )))))
                   
                   )


       ))
    ))

(define-http-handler #/^\/scenarios\/(\d+)\/insert\/(.*)/
  (^[req app]
    (violet-async
     (^[await]
       (let-params req ([id "p:1"]
                        [label "p:2"])
                   (let ((rendered (read-and-render-scenario-file/insert id label)))
                     (respond/ok req (cons "<!DOCTYPE html>"
                                           (sxml:sxml->html
                                            (create-page
                                             rendered
                                             )))))
                   
                   )


       ))
    ))

(define-http-handler #/^\/scenarios\/(\d+)\/edit\/(.*)/
  (^[req app]
    (violet-async
     (^[await]
       (let-params req ([id "p:1"]
                        [label "p:2"])
                   (let ((rendered (read-and-render-scenario-file/edit id label)))
                     (respond/ok req (cons "<!DOCTYPE html>"
                                           (sxml:sxml->html
                                            (create-page
                                             rendered
                                             )))))
                   
                   )


       ))
    ))

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
        (section (get "section"))
        (lines (get "lines")))
    (let* ((filename (json-file-path data-id))
           (modified (await
                      (^[]
                        (with-input-from-file filename
                          (^()
                            (let ((content (parse-json)))
                              (reverse
                               (fold (^[conv rest]
                                       (cons
                                        (if (string=? orig-label (cdr (assoc "label" conv)))
                                            `((label . ,label)
                                              (section . ,section)
                                              (type . ,type)
                                              (lines . ,lines))
                                            conv)
                                        rest))
                                     ()
                                     content)))))))))
      (overwrite-json-file await modified filename))))

(define (delete-conversation await data-id label)
(let* ((filename (json-file-path data-id))
           (modified (await
                      (^[]
                        (with-input-from-file filename
                          (^()
                            (let ((content (parse-json)))
                              (reverse
                               (fold (^[conv rest]
                                        (if (string=? label (cdr (assoc "label" conv)))
                                            rest
                                            (cons conv rest)))
                                     ()
                                     content)))))))))
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
        (section (get "section"))
        (lines (get "lines")))
    (let* ((filename (json-file-path data-id))
           (modified (await
                      (^[]
                        (with-input-from-file filename
                          (^()
                            (let ((content (parse-json)))
                              (reverse
                               (fold (^[conv rest]
                                        (if (string=? prev-label (cdr (assoc "label" conv)))
                                            (cons `((label . ,label)
                                                    (section . ,section)
                                                    (type . ,type)
                                                    (lines . ,lines))
                                                  (cons conv rest))
                                            (cons conv rest)))
                                     (if (string=? prev-label "")
                                         `(((label . ,label)
                                            (section . ,section)
                                            (type . ,type)
                                            (lines . ,lines)))
                                         ())
                                     content)))))))))
      (overwrite-json-file await modified filename))))

(define (update-with-json await data-id json)
  (define form-data (parse-json-string json))
  (cond ((assoc "original-label" form-data)
         (update-existing-conversation await data-id form-data))
        ((assoc "previous-label" form-data)
         (insert-conversation await data-id form-data))))

(define (convert-json-to-csv await data-id)
  (let ((json-file (json-file-path data-id))
        (dialog-writer (make-csv-writer ","))
        (meta-writer (make-csv-writer ",")))
    (call-with-output-file "Dialog_meta.csv"
      (^[meta-port]
        (call-with-output-file "Dialog.csv"
          (^[dialog-port]
            (dialog-writer dialog-port '("" "character" "text"))
            (meta-writer meta-port '("" "type" "location" "count"))
            (json-match
             (await (^[] (with-input-from-file json-file parse-json)))
             (^[% @]
               (@ (^d
                   (let ((label #f) (section #f) (type #f) (linecount 0))
                     ((% "label" (cut set! label <>)) d)
                     ((% "section" (cut set! section <>)) d)
                     ((% "type" (cut set! type <>)) d)
                     ((% "lines"
                         (^d
                          (set! linecount (vector-length d))
                          (let ((char #f) (text #f) (num 0))
                            ((@ (^j
                                 ((% "character" (cut set! char <>)) j)
                                 ((% "text" (cut set! text <>)) j)
                                 (dialog-writer dialog-port
                                                `(,#"~|label|_~num" ,char ,text))
                                 (inc! num)
                                 j))
                             d))))
                      d)
                     (meta-writer meta-port
                                  `(,label ,type ,section
                                           ,(x->string linecount))))))))))))))

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

(define-http-handler "/login"
  (^[req app]
	(respond/ok req (cons "<!DOCTYPE html>"
                               (sxml:sxml->html
                                (create-page
                                 '(div (@ (class "fb-login-button")
                                          (onlogin "onlogin")
                                          (data-width "")
                                          (data-size "large")
                                          (data-button-type "continue_with")
                                          (data-auto-logout-link "false")
                                          (data-use-continue-as "false")))

                                 ))))))

(define-http-handler #/^\/static\// (file-handler))
