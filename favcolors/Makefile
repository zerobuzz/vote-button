PSC=psc
PULP=pulp
TARGET=./backend/public_html
CABAL_SANDBOX=../.cabal-sandbox

default: build-quick

init:
	cd backend && cabal sandbox --sandbox=../$(CABAL_SANDBOX) init
	make build-backend
	mkdir -p ./frontend/bower_components
	make build-all

version:
	$(PSC) --version
	$(PULP) --version

build-quick:
	cd frontend && pulp browserify -O --to ../$(TARGET)/output.js

build-deps:
	cd frontend && pulp dep install
	npm install virtual-dom#2.0.0

build-all: clean build-deps build-quick

build-backend:
	cd backend && cabal install

run-backend:
	cd backend && ../$(CABAL_SANDBOX)/bin/favcolors

clean:
	rm -rf $(TARGET)/output.js ./frontend/output/
	rm -rf ./frontend/node_modules/
	cd ./frontend/bower_components && ( pulp dep uninstall * || true )
