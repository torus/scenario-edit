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
              (href "https://cdn.jsdelivr.net/npm/bulma@0.9.0/css/bulma.min.css"))))
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

(define (read-scenario-file id)
  (let ((filename #"data/~|id|.json"))
    (with-input-from-file filename
      (^()
        (let ((content (parse-json)))
          (reverse
           (fold (^[conv rest]
                   (define (get name)
                     (cdr (assoc name conv)))
                   (cons (let ((label (get "label"))
                               (lines (get "lines")))
                           `(section (@ (class "section"))
                                     (div (@ (class "columns"))
                                          ((div (@ (class "column is-half"))
                                                (h4 (@ (class "title is-4")) ,label))
                                           (div (@ (class "column is-half"))
                                                (p ,(get "section")))))
                                     (ul
                                      ,(reverse
                                        (fold (^[line rest]
                                                (let ((char (cdr (assoc "character" line)))
                                                      (text (cdr (assoc "text" line))))
                                                  (cons `(div (@ (class "columns"))
                                                              (div (@ (class "column is-one-fifth has-text-right"))
                                                                   ,char)
                                                              (div (@ (class "column"))
                                                                   ,text))
                                                        rest)))
                                              () lines)))
                                     ))
                         rest)
                      )
                 () content))
          ))
      )
    ))

(define-http-handler #/^\/scenarios\/(\d+)/
  (^[req app]
    (violet-async
     (^[await]
       (let-params req ([id "p:1"])
                   (let ((rendered (read-scenario-file id)))
                     (write rendered)
                     (respond/ok req (cons "<!DOCTYPE html>"
                                           (sxml:sxml->html
                                            (create-page
                                             rendered
                                             )))))
                   
                   )


       ))
    ))


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
