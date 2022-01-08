(use gauche.test)
(test-start "JSON Matcher")
(load "./lib/json-match")
(import json-match)

(test-module 'json-match)

(use rfc.json)

(load "./testlib/test-utils")

(test-section "json-match-object")

(test-behavior "access object elements"
               (^[]
                 (let ((json (parse-json-string "{\"a\":123}")))
                   (assert-equal 123 (json-match-object json "a" id #f)))))

(test-behavior "fail if not an object"
               (^[]
                 (let ((json (parse-json-string "123")))
                   (assert-equal 'fail (json-match-object json "a" id fail)))))

(test-behavior "access elements in nested object"
               (^[]
                 (let ((json (parse-json-string "{\"a\":{\"b\":234}}")))
                   (assert-equal
                    234
                    (json-match-object json "a" (^v (json-match-object v "b" id fail)) fail)))))

(test-section "json-match-array")

(test-behavior "access each element in an array"
               (^[]
                 (let ((json (parse-json-string "[1,2,3]")))
                   (assert-equal
                    123
                    (let ((total 0))
                      (json-match-array json (^v (set! total (+ (* 10 total) v))) fail)
                      total)))))

(test-section "json-match")

(define *json*
  (string-append
   "[{\"a\" :  1, \"b\" :  2, \"c\" : [{\"d\" :  3, \"e\" :  4}, {\"d\" :  5, \"e\" :  6}]},"
   " {\"a\" : 11, \"b\" : 12, \"c\" : [{\"d\" : 13, \"e\" : 14}, {\"d\" : 15, \"e\" : 16}]}]"))

(test-behavior "matches an complicated JSON"
               (^[]
                 (let ((json (parse-json-string *json*)))
                   (assert-equal
                    #t
                    (json-match
                     json
                     (^[o a]
                       (a (o "a" id)
                          (o "b" id)
                          (o "c"
                             (a (o "d" id)
                                (o "e" id))))))))))

(test-section "json-query")

(test-behavior "returns the element"
               (^[]
                 (let ((json (parse-json-string *json*)))
                   (assert-equal
                    13
                    (json-query json '(1 "c" 0 "d"))))))

(test-end :exit-on-failure #t)
