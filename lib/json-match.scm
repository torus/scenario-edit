(define-module json-match
  (export json-match-object
          json-match-array
          json-match
          json-query))

(select-module json-match)


(define (json-match-object json key resolve reject)
  (if (pair? json)
      (let ((val (assoc key json)))
        (if val
            (resolve (cdr val))
            (reject #"key not found: ~key")))
      (reject "not an object")))

(define (json-match-array json resolve reject)
  (vector-for-each (^v (resolve v)) json)
  #t)

(define (json-match json proc)
  (define (reject err)
    #f)

  (define (o key proc)
    (^j
     (define (resolve result)
       (proc result)
       #t)
     (json-match-object j key resolve reject)))

  (define (a . procs)
    (^j
     (define (resolve result)
       (let loop ((procs procs))
         (if (null? procs)
             #t
             (and
              ((car procs) result)
              (loop (cdr procs))))))

     (json-match-array j resolve reject)))

  ((proc o a) json)
  )

;; Utilities

(define (json-query json path)
  (let loop ((path path)
             (json json))
    (if (null? path)
        json
        (let ((p (car path)))
          (if (number? p)
              (loop (cdr path) (vector-ref json p))
              (loop (cdr path) (cdr (assoc p json))))))))
