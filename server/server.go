package server

import (
	"bytes"
	"fmt"
	"github.com/bread-editor/bread/version"
	"github.com/ugorji/go/codec"
	"net"
	"os"
)

type Server struct {
	Clients []Client
}

func Serve(conn_type string, conn_host string, conn_port string) {
	l, err := net.Listen(conn_type, conn_host+":"+conn_port)
	if err != nil {
		fmt.Println("Error listening", err.Error())
		l.Close()
		os.Exit(1)
	}
	fmt.Println("Listening on " + conn_host + ":" + conn_port)

	for {
		conn, err := l.Accept()
		if err != nil {
			fmt.Println("Client tried to connect, but failed: ",
				err.Error())
		}
		go handleRequest(conn)
	}
}

func handleRequest(conn net.Conn) {
	buf := make([]byte, 1024)
	_, err := conn.Read(buf)
	if err != nil {
		fmt.Println("Error reading", err.Error())
		return
	}

	if 0 == bytes.Compare(buf[0:3], []byte("GET")) {
		fmt.Println("Got HTTP request")
		conn.Write(defaultHTTPResponse)
		conn.Close()
		return
	}

	var mp codec.MsgpackHandle
	var b []byte
	handle := &mp

	enc := codec.NewEncoderBytes(&b, handle)
	err = enc.Encode(buf)

	conn.Write(b)
	go handleRequest(conn)
	conn.Close()
}

var defaultHTTPResponse []byte = []byte("HTTP/1.1 200 OK\r\nServer: " +
	version.BreadVersion + "\r\n\n" + "<h1>Hello from Bread!</h1>" +
	"<p>This server is running " + version.BreadVersion + "." +
	" However, it is not running an HTTP handler. Try" +
	" connecting with a Bread client!</p>")
