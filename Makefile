SCRIPT=main.scm

BUILT:
	rh1 install > $@ || (rm -f $@ ; exit 1)

run: BUILT
	nodemon -e scm --ignore gosh-modules/ --exec violet $(SCRIPT)

clean:
	rm -rf *~ *.o gosh-modules

.PHONY: check run clean build

check:
	gosh test/test-random.scm
	gosh test/test-sqlite.scm
	gosh test/test-json-match.scm
	gosh -I /usr/local/lib/violet test/test-scenario.scm

en:
	gosh conv-en.scm

zh:
	gosh conv-en.scm --language zh-Hant

update-en:
	gosh update-en.scm
