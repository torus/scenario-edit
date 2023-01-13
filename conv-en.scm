(use gauche.collection)
(use gauche.parseopt)
(use gauche.process)
(use gauche.generator)

(use file.util)
(use rfc.json)
(use text.csv)

(add-load-path "./lib" :relative)
(use json-match)

(define (main args)
  (let-args (cdr args)
      ((lang "l|language=s" "en"))
    (let ((lines (call-with-input-file #"csv/Dialogs_lines_~|lang|.csv"
                   (^p (csv-rows->tuples
                        (generator->list (cute (make-csv-reader #\,) p))
                        '("" "text")))))
          (options (call-with-input-file #"csv/Dialogs_options_~|lang|.csv"
                     (^p (csv-rows->tuples
                          (generator->list (cute (make-csv-reader #\,) p))
                          '("" "option"))))))
      (with-input-from-file "json/2-dialogs.json"
        (^[]
          (let ((json (parse-json)))
            (json-match
             json
             (^[% @]
               (@
                (^[dialog]
                  (define label (assoc-ref dialog "label"))
                  (define index 0)
                  ((% "lines"
                      (@ (^[line]
                           (define opt-index 0)
                           ((% "text"
                               (^e (let ((text (get-line lines label index)))
                                     (when text (assoc-set! line "text" text)))
                                   #t))
                            line)
                           ((% "options"
                               (@ (^[opt]
                                    ((% "text"
                                        (^e
                                         (let ((text (get-option options label
                                                                 index opt-index)))
                                           (when text (assoc-set! opt "text" text)))
                                         #t)) opt)
                                    (inc! opt-index)))) line)
                           (inc! index))))
                   dialog)))))

            (with-output-to-process #"jq . > json/2-dialogs_~|lang|.json"
              (^[]
                (construct-json json))))))))

  0)

(define (get-line lines label index)
  (let ((found (assoc #"~|label|_~index" lines)))
    (and found (cadr found))))

(define (get-option options label line-index option-index)
  (let ((found (assoc #"~|label|_~|line-index|_~option-index" options)))
    (and found (cadr found))))
