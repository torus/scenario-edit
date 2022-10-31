(use gauche.generator)
(use gauche.process)

(use rfc.json)
(use text.csv)
(use util.match)

(define (main args)
  (let* ((dialogs (make-dialogs (cadr args)))
         (scenario (make-scenario dialogs)))
    (with-output-to-process "jq . > ../json/3_x.json"
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
               (let loop ((result ())
                          (prev-label ()))
                 (let ((row (reader)))
                   (if (eof-object? row)
                       (list->vector (reverse result))
                       (let ((dialog (read-row row prev-label)))
                         (loop (append dialog result)
                               (if (null? dialog)
                                   prev-label
                                   (list (cdr (assoc "label" (car dialog))))))
                         ))))))
          dialogs)))))

(define (read-row row prev-label)
  (match row ((id rank category title summary location characters notes loc2 trigger)
              (if (zero? (string-length loc2))
                  ()
                  (let ((trig (if (zero? (string-length trigger))
                                  loc2
                                  trigger)))
                    (list`(("label" . ,#"~|id|_~|title|")
                           ("type" . "area")
                           ("location" . ,loc2)
                           ("trigger" . ,trig)
                           ("flags-required" . ,(list->vector prev-label))
                           ("lines" . #((("character" . "")
                                         ("text" . ,#"~|id|/~|title|/~|rank|/~|category|/~|summary|/~|location|/~|characters|/~notes")
                                         ("options" . #()))))
                           ))
                    ))
              ))
  )
