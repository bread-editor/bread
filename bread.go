package main

import (
	"github.com/bread-editor/bread/server"
)

func main() {
	server.Serve("tcp", "localhost", "8585")
}
