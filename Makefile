LDFLAGS=-ldflags ""

GOSRC = $(shell find ./extension -type f -name '*.go' -not -path "./vendor/*")
ARMASRC = $(shell find ./addons -type f -name '*')

.PHONY: build clean

all: a3web.pbo extension/liba3web.so server-go/server static/index.js

a3web.pbo: $(ARMASRC)
	armake2 pack -v addons a3web.pbo

extension/liba3web.so: $(GOSRC)
	cd extension && GOARCH=386 CGO_ENABLED=1 go build $(LDFLAGS) -o liba3web.so -buildmode=c-shared .

server-go/server:
	cd server-go && go build

static/index.js:
	cd ui && npm install
	cd ui && npx webpack

clean:
	-rm a3web.pbo
	-rm ./extension/liba3web.so
	-rm ./server-go/server
	-rm ./static/index.js
