package server_test

import (
	"github.com/bread-editor/bread/server"
	"net"
	"reflect"
	"testing"
)

func TestRequests(t *testing.T) {
	test := []byte{0x80, 0x30, 0x40, 0x50, 0x41, 0xDE, 0x84, 0xDE, 0xAD,
		0xDE, 0xBB, 0xEE}

	go func() {
		conn, _ := net.Dial("tcp", ":8488")
		defer conn.Close()

		conn.Write(test)
	}()

	l, _ := net.Listen("tcp", ":8488")
	defer l.Close()
	conn, _ := l.Accept()

	res := server.ReadFull(conn)

	if reflect.DeepEqual(res, test) {
		t.Fail()
	}
}
