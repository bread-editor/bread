package tcp_server

import (
	"fmt"
	"github.com/bread-editor/bread/api"
	"github.com/bread-editor/bread/core"
	"github.com/bread-editor/bread/version"
	"net"
)

type Server struct {
	Clients []Client
}

const (
	CONN_HOST = "localhost"
	CONN_PORT = "5309"
	CONN_TYPE = "tcp"
)

// The TCP Server listens for client connections, parses the msgpack'd data,
// and sends the request to the API server
func StartTCPServer(ch chan api.Request) {
	server, err := net.Listen(CONN_TYPE, CONN_HOST+":"+CONN_PORT)

	if server == nil {
		panic("Couldn't start the Bread server: " + err.Error())
	}
	fmt.Printf("Bread server is running on %s", CONN_HOST+":"+CONN_PORT)

	defer server.Close()

	for {
		conn, err := server.Accept()
		if err != nil {
			core.Log(core.ERROR, "Connection failed: %s", err.Error())
		}

		go HandleRequest(conn, ch)
	}
}

// When we get a request, parse the msgpack'd data and send it to the API server
func HandleRequest(conn net.Conn, ch chan api.Request) {

}

var defaultHTTPResponse []byte = []byte("HTTP/1.1 200 OK\r\nServer: " +
	version.BreadVersion + "\r\n\n" + "<h1>Hello from Bread!</h1>" +
	"<p>This server is running " + version.BreadVersion + "." +
	" However, it is not running an HTTP handler. Try" +
	" connecting with a Bread client!</p>")
