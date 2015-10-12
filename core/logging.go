package core

import (
	"fmt"
	"strings"
)

type LogLevel int

const (
	LOG = iota
	WARNING
	ERROR
)

func Log(l LogLevel, s string, args ...interface{}) {
	switch l {
	case LOG:
		str := strings.Join([]string{"Bread LOG: ", s}, "")
		fmt.Printf(str, args)

	case WARNING:
		str := strings.Join([]string{"Bread WARNING: ", s}, "")
		fmt.Printf(str, args)

	case ERROR:
		str := strings.Join([]string{"Bread ERROR: ", s}, "")
		fmt.Printf(str, args)
	}
}
