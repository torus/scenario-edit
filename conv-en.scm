(use gauche.collection)
(use gauche.process)
(use gauche.generator)

(use file.util)
(use rfc.json)
(use text.csv)

(add-load-path "./lib" :relative)
(use json-match)

(define (main . args)
  (let ((lines (call-with-input-file "csv/Dialogs_lines_en.csv"
                 (^p (csv-rows->tuples
                      (generator->list (cute (make-csv-reader #\,) p))
                      '("" "text")))))
        (options (call-with-input-file "csv/Dialogs_options_en.csv"
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
                             (^e (assoc-set! line "text"
                                             (get-line lines label index))
                                 #t))
                          line)
                         ((% "options"
                             (@ (^[opt]
                                  ((% "text"
                                      (^e
                                       (assoc-set! opt "text"
                                                   (get-option options label
                                                               index opt-index))
                                       #t)) opt)
                                  (inc! opt-index)))) line)
                         (inc! index))))
                 dialog)))))

          (with-output-to-process "jq . > json/2-dialogs_en.json"
            (^[]
              (construct-json json)))))))

  0)

(define (get-line lines label index)
  (cadr (assoc #"~|label|_~index" lines)))

(define (get-option options label line-index option-index)
  (cadr (assoc #"~|label|_~|line-index|_~option-index" options)))
