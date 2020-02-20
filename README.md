# A3Web

Arma 3 Realtime Map

## To build extension

```
GOARCH=386 CGO_ENABLED=1 go build -o liba3web.so -buildmode=c-shared .
```
