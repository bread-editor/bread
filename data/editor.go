package data

// The structure for holding the editor state
type Editor struct {
	Buffers []Buffer
	// Placeholder for holding connection information
	clients []byte
	options OptionMap
}
