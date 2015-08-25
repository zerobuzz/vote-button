PSC=./node_modules/.bin/psc
PULP=./node_modules/.bin/pulp
TARGET=./server/public_html

default: build-browser

version:
	$(PSC) --version
	$(PULP) --version

run:
	export PATH=$(PATH):./node_modules/.bin && pulp run

build-browser:
	export PATH=$(PATH):./node_modules/.bin && pulp build -O --to $(TARGET)/output.js

run-browser:
	cd server && ./.cabal-sandbox/bin/vote-button
