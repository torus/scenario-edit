(define-module json-match
  (export json-match-object
          json-match-array
          json-match))

(select-module json-match)


(define (json-match-object json key resolve reject)
  (if (pair? json)
      (let ((val (assoc key json)))
        (if val
            (resolve (cdr val))
            (reject "key not found")))
      (reject "not an object")))

(define (json-match-array json resolve reject)
  (for-each (^v (resolve v)) json)
  #t)

(define (json-match json proc)
  (define (reject err)
    #?=err
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
