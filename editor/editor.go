package editor

import (
	"github.com/bread-editor/bread/buffer"
	"github.com/bread-editor/bread/core"
	"github.com/bread-editor/bread/ui"
	"github.com/bread-editor/bread/version"
)

// The structure for holding the editor state
type Editor struct {
	// List of Buffers
	Buffers []buffer.Buffer

	// Placeholder for holding connection information
	clients []byte

	// Map holding all options for the Editor
	options VarMap

	// Server

	// UI
	Interface ui.UI
}

// Sets some basic options
func InitOptions() VarMap {
	var vars VarMap = make(VarMap)
	vars["bread-version-number"] = version.VersionNumber
	vars["bread-version-name"] = version.ReleaseName
	vars["bread-version"] = version.BreadVersion

	return vars
}

// InitEditor starts the server, then runs the configuration file
func InitEditor() Editor {
	core.Log(core.LOG, "Starting Bread...")

	// Start server
	//go server.Serve("tcp", "localhost", "8585")

	// TODO: Load configuration
	// Create Editor

	return Editor{}
}
