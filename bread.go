package main

import (
	"flag"
	"fmt"
	"github.com/bread-editor/bread/core"
	"github.com/bread-editor/bread/server"
)

func main() {
	f := flag.String("f", "", "File to read")
	flag.Parse()

	buf := core.ReadFile(*f)
	fmt.Println("Buffer path is: ", buf.FilePath)
	fmt.Println("Buffer contents are: ", buf.Contents.ToString())
	server.Serve("tcp", "localhost", "8585")
}
