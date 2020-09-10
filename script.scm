(use gauche.threads)
(use gauche.collection)
(use rfc.http)
(use rfc.json)

(use sxml.tools)

(add-load-path "./gauche-rheingau/lib/")
(use rheingau)
(rheingau-use makiki)

(add-load-path "./lib/")
(use violet)

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
     ))
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

(define (render-conversation/edit-button conv data-id)
  (define (get name)
    (cdr (assoc name conv)))

  (let ((label (get "label"))
        (lines (get "lines")))
    `(section (@ (class "section"))
              (div (@ (class "container"))
                   (div (@ (class "columns is-vcentered"))
                        (div (@ (class "column is-1"))
                             (a (@ (class "button")
                                   (href ,#"/scenarios/~|data-id|/edit/~|label|#form"))
                                (span (@ (class "icon"))
                                      (i (@ (Class "fas fa-edit")) ""))))
                        (div (@ (class "column is-one-third"))
                             (h4 (@ (class "title is-4")) ,label))
                        (div (@ (class "column"))
                             (p ,(get "section"))))
                   ,(reverse
                     (fold (^[line rest]
                             (let ((char (cdr (assoc "character" line)))
                                   (text (cdr (assoc "text" line))))
                               (cons (render-line char text) rest)))
                           () lines))))))

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
                   ,(reverse
                     (fold (^[line rest]
                             (let ((char (cdr (assoc "character" line)))
                                   (text (cdr (assoc "text" line))))
                               (cons (render-line char text) rest)))
                           () lines))))))

(define (not-empty? str) (and str (not (zero? (string-length str)))))

(define (form-field label help input)
  `(div (@ (class "field"))
        ,(if (not-empty? label) `(label (@ (class "label")) ,label) ())
        (div (@ (class "control")) ,input)
        ,(if (not-empty? help) `(p (@ (class "help")) ,help) ())))


(define (render-line-form char text)
  `(div (@ (class "columns"))
        (div (@ (class "column is-one-fifth has-text-right"))
             (input (@ (class "input") (type "text") (placeholder "キャラクター")
                       (value ,char))))
        (div (@ (class "column"))
             (input (@ (class "input") (type "text") (placeholder "セリフ")
                       (value ,text))))))

(define (render-conversation-form conv data-id)
  (define (get name)
    (cdr (assoc name conv)))

  (let ((label (get "label"))
        (lines (get "lines")))
    `(section (@ (class "section")
                 (id "form"))
              (div (@ (class "container"))
                   (form (@ (method "post")
                            (action ,#"/scenarios/~|data-id|/submit"))
                         (div (@ (class "columns"))
                              (div (@ (class "column is-one-third"))
                                   ,(form-field "会話 ID" "他の会話と ID が重複しないようにしてください。"
                                                `(input (@ (class "input")
                                                           (type "text")
                                                           (placeholder "会話 ID")
                                                           (value ,label)))))
                              (div (@ (class "column"))
                                   ,(form-field "場所" #f
                                                `(input (@ (class "input") (type "text")
                                                           (placeholder "場所")
                                                           (value ,(get "section")))))))
                         ,(reverse
                           (fold (^[line rest]
                                   (let ((char (cdr (assoc "character" line)))
                                         (text (cdr (assoc "text" line))))
                                     (cons (render-line-form char text) rest)))
                                 () lines))
                         (div (@ (class "field is-grouped is-grouped-right"))
                              (p (@ (class "control"))
                                 (button (@ (class "button is-primary")) "更新"))
                              (p (@ (class "control"))
                                 (a (@ (class "button is-light")
                                       (href ,#"/scenarios/~data-id"))
                                    "キャンセル"))))))))

(define (add-conversation-button)
  `(div (@ (class "columns"))
        (div (@ (class "column has-text-centered"))
             (button (@ (class "button"))
                     (span (@ (class "icon")) (i (@ (Class "fas fa-plus")) ""))
                     ))))

(define (read-and-render-scenario-file id)
  (let ((filename #"data/~|id|.json"))
    (with-input-from-file filename
      (^()
        (let ((content (parse-json)))
          (reverse
           (fold (^[conv rest]
                   (cons (add-conversation-button)
                         (cons (render-conversation/edit-button conv id) rest)))
                 (list (add-conversation-button))
                 content)))))))

(define (read-and-render-scenario-file/edit id label-to-edit)
  #?=label-to-edit
  (let ((filename #"data/~|id|.json"))
    (with-input-from-file filename
      (^()
        (let ((content (parse-json)))
          (reverse
           (fold (^[conv rest]
                   (let ((label (cdr (assoc "label" conv))))
                     (cons (if (string=? #?=label #?=label-to-edit)
                               (render-conversation-form conv id)
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

(define-http-handler #/^\/scenarios\/(\d+)\/submit/
  (with-post-parameters
   (^[req app]
     (violet-async
      (^[await]
        (let-params req ([id "p:1"])
                    (let ()
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
