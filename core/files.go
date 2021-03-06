package core

import (
	"fmt"
	"github.com/bread-editor/bread/buffer"
	"io/ioutil"
)

// Take a string and split it into a Lines linked list
func splitLines(contents []byte) *buffer.Line {
	l := new(buffer.Line)
	var rest []byte

	for i, b := range contents {
		if b == 0x0a {
			rest = contents[i+1:]
			l.Contents = string(contents[:i])
			break
		}
	}

	if len(rest) != 0 {
		l.Next = splitLines(rest)
		l.Next.Prev = l
	}

	return l
}

// Reads a file and returns a buffer
func ReadFile(path string) buffer.Buffer {
	d, err := ioutil.ReadFile(path)
	if err != nil {
		fmt.Printf("Error reading file '%s', error was: %s",
			path, err.Error())
	}

	line := splitLines(d)
	buf := buffer.Buffer{FilePath: path, Contents: line, CurrentLine: line}
	return buf
}
