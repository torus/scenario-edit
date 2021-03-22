SCRIPT=scenario.scm

build:
	rh1 install

BUILT: build
	touch $@

run: BUILT
	nodemon -e scm --ignore gosh-modules/ --exec violet $(SCRIPT)

clean:
	rm -rf *~ *.o gosh-modules

.PHONY: check run clean build

check:
	gosh test/test-json-match.scm
