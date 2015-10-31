package api

// The TCP server parses raw msgpack data to this format, and passes it to the
// API server
type Request struct {
	Name string        // The name of the call being requested
	Args []interface{} // The arguments of the call
}
