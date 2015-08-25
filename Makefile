PSC=./node_modules/.bin/psc
PULP=./node_modules/.bin/pulp

default: run

version:
	$(PSC) --version
	$(PULP) --version

run:
	export PATH=$(PATH):./node_modules/.bin && pulp run
