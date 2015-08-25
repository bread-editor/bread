package editor

import (
	"github.com/bread-editor/bread/buffer"
)

// The structure for holding the editor state
type Editor struct {
	// List of Buffers
	Buffers []buffer.Buffer

	// Placeholder for holding connection information
	clients []byte

	// Map holding all options for the Editor
	options OptionMap

	// Server
}
