PSC=./node_modules/.bin/psc
PULP=./node_modules/.bin/pulp
TARGET=./server/public_html

default: build-quick

version:
	$(PSC) --version
	$(PULP) --version

build-quick:
	export PATH=$(PATH):./node_modules/.bin && pulp build -O --to $(TARGET)/output.js

build-deps:
	export PATH=$(PATH):./node_modules/.bin && pulp dep install

build-all: clean build-deps build-quick

run:
	cd server && ./.cabal-sandbox/bin/vote-button

clean:
	test \! -e $(TARGET)/output
	rm -rf $(TARGET)/output.js ./output
	cd ./bower_components && export PATH=$(PATH):../node_modules/.bin && ( pulp dep uninstall * || true )


# install new dependencies:
# $ pulp dep install purescript-math --save
