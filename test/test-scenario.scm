(use gauche.test)
(test-start "Scenario")
(load "./lib/playlogic")
(import playlogic)

(test-module 'playlogic)

(import playlogic.editor)
(test-module 'playlogic.editor)

(import playlogic.play)
(test-module 'playlogic.play)

(test-end)
