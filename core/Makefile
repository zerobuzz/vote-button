PSC=./node_modules/.bin/psc
PULP=./node_modules/.bin/pulp
TARGET=./backend/public_html
CABAL_SANDBOX=../../.cabal-sandbox

default: build-quick

init:
	cd backend && cabal sandbox --sandbox=$(CABAL_SANDBOX) init
	make build-backend
	cd frontend && npm install pulp purescript
	mkdir -p ./frontend/bower_components
	make build-all

version:
	$(PSC) --version
	$(PULP) --version

build-quick:
	cd frontend && export PATH=$(PATH):./node_modules/.bin && pulp build -O --to ../$(TARGET)/output.js

build-deps:
	cd frontend && export PATH=$(PATH):./node_modules/.bin && pulp dep install

build-all: clean build-deps build-quick

build-backend:
	cd backend && cabal install

run-backend:
	cd backend && $(CABAL_SANDBOX)/bin/vote-button

clean:
	rm -rf $(TARGET)/output.js ./output/
	cd ./frontend/bower_components && export PATH=$(PATH):../node_modules/.bin && ( pulp dep uninstall * || true )


# install new dependencies:
# $ pulp dep install purescript-math --save
