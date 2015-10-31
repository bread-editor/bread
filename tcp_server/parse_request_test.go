package tcp_server_test

import (
	"github.com/bread-editor/bread/tcp_server"
	"net"
	"reflect"
	"testing"
)

func TestReadFull(t *testing.T) {
	test1 := make([]byte, 893)
	for i := 0; i < 885; i++ {
		test1[i] = byte(i)
	}

	go func() {
		conn, err := net.Dial("tcp", "127.0.0.1:8488")
		if err != nil {
			t.Fatal(err)
		}
		defer conn.Close()
		conn.Write(test1)
	}()

	l, err := net.Listen("tcp", "127.0.0.1:8488")

	if err != nil {
		t.Fatal(err)
	}
	defer l.Close()

	for {
		conn, err := l.Accept()
		if err != nil {
			return
		}
		defer conn.Close()

		res := tcp_server.ReadFull(conn)
		if !reflect.DeepEqual(res, test1) {
			t.Logf("What we wanted: \n%v\n\nWhat we got: \n%v\n", test1, res)
			t.Logf("Length expected: %d\n\nLength got: %d\n", len(test1), len(res))
			t.Fail()
		}
		return
	}
}
