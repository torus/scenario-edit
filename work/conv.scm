(use gauche.generator)
(use gauche.process)

(use rfc.json)
(use text.csv)
(use util.match)

(define (main args)
  (let* ((dialogs (make-dialogs (cadr args)))
         (scenario (make-scenario dialogs)))
    (with-output-to-process "jq . > ../json/3.json"
                            (^[]
                              (construct-json scenario))))
  0)

(define (make-scenario dialogs)
  `(("version" . "1.0")
    ("title" . "Ukiyo Rebooted")
    ("locations" . #())
    ("dialogs" . ,dialogs)))

(define (make-dialogs path)
  (call-with-input-file path
    (^[port]

      (let ((reader (cute (make-csv-reader #\tab) port)))
        (reader)                        ; skip the header
        (let ((dialogs
               (let loop ((result ()))
                 (let ((row (reader)))
                   (if (eof-object? row)
                       (list->vector (reverse result))
                       (loop (cons (read-row row) result))))
                 )))
          dialogs)))))

(define (read-row row)
  (match row ((id file-no rank category title summary location characters notes)
              `(("label" . ,#"~|id|~|title|")
                ("type" . "area")
                ("location" . ,location)
                ("trigger" . "?")
                ("lines" . #((("character" . "")
                              ("text" . ,#"~|id|/~|title|/~|rank|/~|category|/~|summary|/~|location|/~|characters|/~notes")
                              ("options" . #()))))
                )


))
)
