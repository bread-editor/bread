package tcp_server

import (
	"bytes"
	"fmt"
	"github.com/bread-editor/bread/api"
	"github.com/bread-editor/bread/core"
	"github.com/ugorji/go/codec"
	"net"
)

// Read a full TCP request, up until the terminating seqeunce
func ReadFull(conn net.Conn) []byte {
	buf := make([]byte, 512)
	res := make([]byte, 0)

	n, _ := conn.Read(buf)

	// Requests in Bread are terminated by 8 null bytes
	for 0 != bytes.Compare(buf[n-8:n], []byte{0, 0, 0, 0, 0, 0, 0, 0}) {
		res = append(res, buf[len(buf)-n:len(buf)]...)
		n, _ = conn.Read(buf)
	}
	res = append(res, buf[len(buf)-n:len(buf)]...)

	return res
}

// Decode raw bytes into an API Request
func ParseRequest(data []byte) api.Request {
	var (
		mh codec.MsgpackHandle
		b  []byte
		h  = &mh
	)

	dec := codec.NewDecoderBytes(b, h)

	err := dec.Decode(data[:len(data)-8])
	if err != nil {
		core.Log(core.ERROR, "Error decoding msgpack request: %s", err.Error())
	}

	fmt.Printf("%v", b)

	return api.Request{}
}
