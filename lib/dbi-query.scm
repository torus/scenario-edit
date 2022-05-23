(define-module dbi-query
  (use text.tree)
  (use dbi)

  (export build-query
          do-query
   ))

(select-module dbi-query)

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
                )))))

(define (do-query await conn query . params)
  (with-query-result/tree
   await conn (build-query conn query) params
   (^[rset]
     (if (is-a? rset <relation>)
         (relation-rows rset)
         rset))))

(define (with-query-result await conn str args proc)
  (let ((rset (await (^[] (apply dbi-do conn str '() args)))))
    (let ((result (proc rset)))
      (dbi-close rset)
      result)))

(define (with-query-result/tree await conn tree args proc)
  (with-query-result await conn (tree->string tree) args proc))
