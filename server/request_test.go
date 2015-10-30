package server_test

import (
	"github.com/bread-editor/bread/server"
	"net"
	"reflect"
	"testing"
)

func TestRequests(t *testing.T) {
	test := make([]byte, 4096)
	for i := 0; i < 4088; i++ {
		test[i] = 8
	}

	go func() {
		conn, _ := net.Dial("tcp", ":8488")
		defer conn.Close()

		conn.Write(test)
	}()

	l, _ := net.Listen("tcp", ":8488")
	defer l.Close()
	conn, _ := l.Accept()

	res := server.ReadFull(conn)

	if !reflect.DeepEqual(res, test) {
		t.Fail()
	}
}
