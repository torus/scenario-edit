(define-module query
  (use dbi)
  (add-load-path "../gosh-modules/dbd-sqlite" :relative)
  (use dbd.sqlite)

  (export build-query
   ))

(select-module query)

(define (build-query conn lis)
  (let loop ((lis lis) (tree ()))
    (if (null? lis)
        (intersperse " " (reverse tree))
        (let ((e (car lis)))
          (cond ((symbol? e)
                 (loop (cdr lis) (cons (symbol->string e) tree)))
                ((string? e)
                 (loop (cdr lis) (cons #"\"~(dbi-escape-sql conn e)\"" tree)))
                ((pair? e)
                 (loop (cdr lis) (cons (list "(" (build-query conn e) ")") tree)))
                ((number? e)
                 (loop (cdr lis) (cons (number->string e) tree)))
                ((null? e)
                 (loop (cdr lis) (cons (list "(" ")") tree)))
                ))))

)
