package server

import (
	"bytes"
	"net"
)

type Request []byte

func ReadFull(conn net.Conn) []byte {
	buf := make([]byte, 512)
	res := make([]byte, 0)

	n, _ := conn.Read(buf)

	// Requests in Bread are terminated by 8 null bytes
	for 0 != bytes.Compare(buf[n-8:n], []byte{0, 0, 0, 0, 0, 0, 0, 0}) {
		res = append(res, buf[len(buf)-n:len(buf)]...)
		n, _ = conn.Read(buf)
	}
	res = append(res, buf...)

	return res
}
