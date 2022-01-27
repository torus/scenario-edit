(use gauche.test)
(use gauche.generator)

(use data.random)
(use rfc.sha)
(use util.digest)

(test-start "Random number generator and digest framework")

(set! (random-data-seed) (sys-time))
(define algo (make <sha1>))
(define random-bytes (generator->bytevector uint8s 16))

(digest-update! algo random-bytes)
(test-log (digest-hexify (digest-final! algo)))
