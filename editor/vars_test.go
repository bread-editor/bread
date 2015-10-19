package editor_test

import (
	"github.com/bread-editor/bread/editor"
	"testing"
)

// Testing the options structures
func TestOptions(t *testing.T) {
	var testOpts editor.VarMap = make(editor.VarMap)
	testOpts["bread-name"] = "bread"
	testOpts["bread-ver"] = 1
	testOpts["bread-avail"] = true

	if testOpts["bread-name"] != "bread" {
		t.Fail()
	}
	if testOpts["bread-ver"] != 1 {
		t.Fail()
	}
	if testOpts["bread-avail"] != true {
		t.Fail()
	}
}
