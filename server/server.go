package server

import (
	"fmt"
	"github.com/ugorji/go/codec"
	"net"
	"os"
)

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
	}
	var mp codec.MsgpackHandle
	var b []byte
	handle := &mp

	enc := codec.NewEncoderBytes(&b, handle)
	err = enc.Encode(buf)

	conn.Write(b)
	conn.Close()
}
