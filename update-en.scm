(use gauche.collection)
(use gauche.process)
(use gauche.generator)

(use file.util)
(use rfc.json)
(use text.csv)

(add-load-path "./lib" :relative)
(use json-match)

(define (main . args)
  (let ((table (make-translation-table))
        (dialogs (read-json "json/2-dialogs.json")))
    (vector-for-each
     (^[dialog]
       (let ((lines (assoc-ref dialog "lines")))
         (vector-for-each
          (^[line]
            (let ((text (assoc-ref line "text")))
              (when (hash-table-contains? table text)
                (assoc-set! line "text" (hash-table-get table text)))))
          lines)))
     dialogs)

    (with-output-to-process "jq . > json/2-dialogs_en_new.json"
                            (^[]
                              (construct-json dialogs))))
  0)

(define (make-translation-table)
  (let ((orig-ja (read-json "json/2-dialogs_orig.json"))
        (orig-en (read-json "json/2-dialogs_en.json"))
        (table (make-hash-table 'string=?)))

    (unless (= (vector-length orig-ja) (vector-length orig-en))
      (error "length unmatched"))

    (vector-for-each
     (^[ja-dialog en-dialog]
       (let ((ja-lines (assoc-ref ja-dialog "lines"))
             (en-lines (assoc-ref en-dialog "lines")))
         (vector-for-each
          (^[ja-line en-line]
            (let ((ja (assoc-ref ja-line "text"))
                  (en (assoc-ref en-line "text")))
              (when (hash-table-contains? table "")
                (print #"duplicated text: ~ja"))
              (hash-table-put! table ja en)))
          ja-lines en-lines)))
     orig-ja orig-en)

    table))

(define (read-json file)
  (with-input-from-file file
    (^[]
      (parse-json))))
