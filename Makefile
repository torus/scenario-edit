SCRIPT=main.scm

BUILT:
	rh1 install | tee $@

run: BUILT
	nodemon -e scm --ignore gosh-modules/ --exec violet $(SCRIPT)

clean:
	rm -rf *~ *.o gosh-modules

.PHONY: check run clean build

check:
	gosh test/test-json-match.scm
	gosh -I /usr/local/lib/violet test/test-scenario.scm
