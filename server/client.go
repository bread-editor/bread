package server

type Client struct {
	// Did the Client send the proper initialization request?
	Initialized bool

	// Is the client connected?
	Connected bool

	// Hooks the client is listening for
	Hooks string
}
