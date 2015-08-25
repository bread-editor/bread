package editor_test

import (
	"github.com/bread-editor/bread/editor"
	"testing"
)

// Testing the options structures
func TestOptions(t *testing.T) {
	var testOpts editor.OptionMap = make(editor.OptionMap)
	testOpts["testOptions"] = make(editor.OptionGroup)
	testOpts["testOptions"]["isTrue"] = true
	testOpts["testOptions"]["isFalse"] = false
	testOpts["testOptions"]["string"] = "hello!"
	testOpts["testOptions"]["int"] = 8

	// Really really useful checking of things
	if testOpts["testOptions"]["isTrue"] != true {
		t.Fail()
	}
	if testOpts["testOptions"]["isFalse"] != false {
		t.Fail()
	}
	if testOpts["testOptions"]["string"] != "hello!" {
		t.Fail()
	}
	if testOpts["testOptions"]["int"] != 8 {
		t.Fail()
	}
}
