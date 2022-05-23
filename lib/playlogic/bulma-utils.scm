(define-module bulma-utils
  (export navbar/
          container/
          fas-icon/
 ))


(select-module bulma-utils)

(define (navbar/ await data-id title . content)
  `(navbar (@ (class "navbar is-fixed-top is-light")
              (role "navigation")
              (aria-label "main navigation"))
           (div (@ (class "navbar-brand"))
                (div (@ (class "navbar-item"))
                     (h1 (@ (class "title is-4")) ,#"~|title|"))
                (a (@ (role "button") (class "navbar-burger")
                      (aria-label "menu") (aria-expanded "false")
                      (data-target "playlogic-navbar"))
                   (span (@ (aria-hidden "true")) "")
                   (span (@ (aria-hidden "true")) "")
                   (span (@ (aria-hidden "true")) "")))

           ,content))

(define (container/ . children)
  `(div (@ (class "container"))
        ,@children))

(define (fas-icon/ name)
  `(span (@ (class "icon"))
         (i (@ (class ,#"fas fa-~name")) "")))
