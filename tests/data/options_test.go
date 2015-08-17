package data_test

import (
	"github.com/bread-editor/bread/data"
	"testing"
)

// Testing the options structures
func TestOptions(t *testing.T) {
	var testOpts data.OptionMap = make(data.OptionMap)
	testOpts["testOptions"] = make(data.OptionGroup)
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
