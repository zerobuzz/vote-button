PSC=./node_modules/.bin/psc
PULP=./node_modules/.bin/pulp

default: run-browser

version:
	$(PSC) --version
	$(PULP) --version

run:
	export PATH=$(PATH):./node_modules/.bin && pulp run

run-browser:
	export PATH=$(PATH):./node_modules/.bin && pulp build -O --to output.js
	lighttpd -D -f lighttpd.conf
