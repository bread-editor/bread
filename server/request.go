package server

import (
	"bytes"
	"net"
)

type Request []byte

func ReadFull(conn net.Conn) []byte {
	tmp := make([]byte, 512)
	res := make([]byte, 512)

	n, _ := conn.Read(tmp)
	// Requests in Bread are terminated by 0xDEADDEBBEE
	for 0 != bytes.Compare(tmp[n-5:n], []byte{0xDE, 0xAD, 0xDE, 0xBB, 0xEE}) {
		copy(res, tmp)
		n, _ = conn.Read(tmp)
	}
	copy(res, tmp)

	return res
}
