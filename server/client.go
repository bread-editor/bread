package server

// Used to represent the two types of clients that exist in Bread.
//
// PASSIVE clients WILL NOT change the state of the editor themselves. They
// simply monitor the editor state with various hooks. They can, however, run
// any other programs outside of themselves, including other Bread clients. Each
// call that a PASSIVE client makes will be executed asynchronously by default.
//
// ACTIVE clients WILL change the state of the editor themselves. Each call that
// an ACTIVE client makes will be executed synchronously by default.
type ClientType int

const (
	ACTIVE ClientType = iota
	PASSIVE
)

type Client struct {
	// Did the Client send the proper initialization request?
	Initialized bool

	// Client type
	Type ClientType

	// Is the client connected?
	Connected bool
}
