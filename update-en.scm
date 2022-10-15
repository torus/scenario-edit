(use gauche.collection)
(use gauche.process)
(use gauche.generator)

(use file.util)
(use rfc.json)
(use text.csv)

(add-load-path "./lib" :relative)
(use json-match)

(define (main . args)
  (let ((orig-tbl (read-orig-en))
        (ja-json (read-ja)))

    #?=orig-tbl
    #?=ja-json

    (vector-for-each
     (^[dialog]
       (let ((en-dialog
              (hash-table-get orig-tbl (assoc-ref dialog "label") #f)))
         (when en-dialog
           (replace-dialog! dialog en-dialog))
         ))
     ja-json)

    (with-output-to-process "jq . > json/2-dialogs_en_2.json"
                            (^[]
                              (construct-json ja-json)))
    )
  0)

(define (replace-dialog! dialog en-dialog)

  (let ((lines (assoc-ref dialog "lines"))
        (en-lines (assoc-ref en-dialog "lines")))

    (vector-for-each-with-index
     (^[index line]
       (when (< index (vector-length en-lines))
         (let ((en-line (vector-ref en-lines index)))
           (assoc-set! line "text" (assoc-ref en-line "text"))
         ))
       )
     lines)


    )

  )

(define (read-orig-en)
  (with-input-from-file "json/2-dialogs_en.json"
    (^[]
      (let ((json (parse-json))
            (tbl (make-hash-table 'string=?)))

        (vector-for-each
         (^[dialog]
           (let ((label (assoc-ref dialog "label")))
             (hash-table-put! tbl label dialog)))
         json)
        tbl))))

(define (read-ja)
  (with-input-from-file "json/2-dialogs.json"
    (^[]
      (parse-json))))
