(use gauche.test)
(test-start "Scenario")
(load "./lib/playlogic")
(import playlogic)

(test-module 'playlogic)

(import playlogic.editor)
(test-module 'playlogic.editor)

(import playlogic.play)
(test-module 'playlogic.play)

(import web-session)
(test-module 'web-session)

(import playlogic.datastore)
(test-module 'playlogic.datastore)

(test-end :exit-on-failure #t)
