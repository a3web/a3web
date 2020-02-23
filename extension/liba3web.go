package main

/*
  #include <stdlib.h>
  #include <stdio.h>
  #include <string.h>

  typedef int(*callbackProc)(char const *name, char const *function, char const *data);

  static inline int bridge_cb(callbackProc cb, char const *name, char const *function, char const *data) {
	return cb(name, function, data);
  }

  static inline uint min(uint a, uint b) { return a < b ? a : b; }
*/
import "C"

import (
	"os"
	"fmt"
	"log"
	"strings"
	"unsafe"
	"net/http"
	"encoding/json"
	"io/ioutil"
)

var cb C.callbackProc
var name = C.CString("a3web")
var serverURL = "https://localhost"

// RVExtensionRegisterCallback on extension load
//export RVExtensionRegisterCallback
func RVExtensionRegisterCallback(cbptr unsafe.Pointer) {
	cb = C.callbackProc(cbptr)

	log.Println("Calling callback function ……")
	function := C.CString("registered")
	defer C.free(unsafe.Pointer(function))
	C.bridge_cb(cb, name, function, function)
}

// RVExtensionVersion on extension load
//export RVExtensionVersion
func RVExtensionVersion(output *C.char, outputsize C.size_t) {
	version := C.CString("Version 0.1")
	defer C.free(unsafe.Pointer(version))
	var size = C.min(C.strlen(version)+1, outputsize-1)
	C.strncpy(output, version, size)

	// Load config
	const configPath string = "~/.a3web.conf"
	type Config struct {
		serverURL string `json:"serverURL"`
	}
	var config Config

	if _, err := os.Stat(configPath); err == nil {
		configFile, _ := ioutil.ReadFile(configPath)
		json.Unmarshal(configFile, &config)
		serverURL = config.serverURL
	}
}

// RVExtensionArgs STRING callExtension ARRAY
//export RVExtensionArgs
func RVExtensionArgs(output *C.char, outputsize C.size_t, function *C.char, argv **C.char, argc C.int) {
	var offset = unsafe.Sizeof(uintptr(0))
	var out []string
	for index := C.int(0); index < argc; index++ {
		out = append(out, C.GoString(*argv))
		argv = (**C.char)(unsafe.Pointer(uintptr(unsafe.Pointer(argv)) + offset))
	}

	go handleArgs(C.GoString(function), out)
}

// RVExtension STRING callExtension STRING
//export RVExtension
func RVExtension(output *C.char, outputsize C.size_t, function *C.char) {
	result := C.CString(fmt.Sprintf("Hello, %s!", C.GoString(function)))
	defer C.free(unsafe.Pointer(result))
	var size = C.min(C.strlen(result)+1, outputsize-1)
	C.strncpy(output, result, size)
}

func main() {}

func handleArgs(function string, argv []string) {
	switch dispatcher := "function"
	dispatcher {
	case "upload_pos":
		uploadUnitsInfo(argv[0])
	}
}

func uploadUnitsInfo(body string) {
	var endpoint string = "/units-info"
	resp, err := http.Post(serverURL + endpoint, "", strings.NewReader(body))
	if err != nil {
		log.Printf("Error Sending HTTP Request: %s", err)
		return
	}
	resp.Body.Close()
}
