package api

import (
	"github.com/bread-editor/bread/core"
)

type Call struct {
	// Call name
	Name string

	// Associated internal function
	Do func([]interface{})
}

var CallMap map[string]Call

// Define the API
func DefineApi() {
	// File-Related API Definitions

	// openFile : [Path String]
	CallMap["openFile"] = Call{
		Name: "openFile",
		Do: func(args []interface{}) {
			switch a := args[0].(type) {
			case string:
				core.ReadFile(a)

			default:
				core.Log(core.ERROR, "Wrong type of argument sent to 'openFile'")
			}
		},
	}
}
