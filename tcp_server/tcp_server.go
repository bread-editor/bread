package tcp_server

import (
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
func StartTCPServer(reqch chan api.Request, resch chan api.Response) {
	// TODO: Make this configurable
	// Start server on localhost:5309
	server, err := net.Listen(CONN_TYPE, CONN_HOST+":"+CONN_PORT)

	if server == nil {
		panic("Couldn't start the Bread server: " + err.Error())
	}
	core.Log(core.LOG, "Bread server is running on %s", CONN_HOST+":"+CONN_PORT)

	defer server.Close()

	// Listen for TCP connections, and then handle the requests
	for {
		conn, err := server.Accept()
		if err != nil {
			core.Log(core.ERROR, "Connection failed: %s", err.Error())
		}

		go HandleRequest(conn, reqch, resch)
	}
}

// When we get a request, parse the msgpack'd data and send it to the API server
func HandleRequest(conn net.Conn, reqch chan api.Request, resch chan api.Response) {
	data := ReadFull(conn)
	reqch <- ParseRequest(data)

	// Listen for responses from the API server,
	go func() {
		for {
			res := <-resch
			DispatchResults(conn, res)
		}
	}()
}

// Dispatch results to the proper client
func DispatchResults(conn net.Conn, res api.Response) {
	conn.Close()
}

var defaultHTTPResponse []byte = []byte("HTTP/1.1 200 OK\r\nServer: " +
	version.BreadVersion + "\r\n\n" + "<h1>Hello from Bread!</h1>" +
	"<p>This server is running " + version.BreadVersion + "." +
	" However, it is not running an HTTP handler. Try" +
	" connecting with a Bread client!</p>")
