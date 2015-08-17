package data

import (
	"strings"
)

// This is the basic type of structure for holding contents in Bread. It is
// a doubly-linked list
type Line struct {
	Contents string
	Next     *Line
	Prev     *Line
}

// Create a new line and update the linked list if given other lines
func NewLine(contents string, next *Line, prev *Line) *Line {
	new_l := &Line{Contents: contents}

	if prev != nil {
		prev.Next = new_l
		new_l.Prev = prev
	}

	if next != nil {
		next.Prev = new_l
		new_l.Next = next
	}

	return new_l
}

// Create an empty line
func EmptyLine(next *Line, prev *Line) *Line {
	return NewLine("", next, prev)
}

// Find line which is `i` number of lines forward, stopping when it hits the
// bottommost line
func (l *Line) Forward(i int) *Line {
	if l.Next != nil && i != 0 {
		return l.Next.Forward(i - 1)
	}
	return l
}

// Find line which is `i` number of lines backwards, stopping when it hits the
// uppermost line
func (l *Line) Backward(i int) *Line {
	if l.Prev != nil && i != 0 {
		return l.Prev.Backward(i - 1)
	}
	return l
}

// Remove a line from the linked list
func (l *Line) Remove() (*Line, *Line) {
	if l.Next != nil && l.Prev != nil {
		l.Next.Prev = l.Prev
		l.Prev.Next = l.Next
	} else if l.Prev == nil && l.Next != nil {
		l.Next.Prev = nil
	} else if l.Prev != nil && l.Next == nil {
		l.Prev.Next = nil
	}
	return l.Prev, l.Next
}

// Insert a line before the current line
func (l *Line) InsertBefore(contents string) *Line {
	return NewLine(contents, l, l.Prev)
}

// Insert a line after the current line
func (l *Line) InsertAfter(contents string) *Line {
	return NewLine(contents, l.Next, l)
}

// Get the distance of a line from the start of the linked list
func (l *Line) DistFromStart() int {
	if l.Prev != nil {
		return l.Prev.DistFromStart() + 1
	}
	return 0
}

// Get distance of a line from the end of the linked list
func (l *Line) DistFromEnd() int {
	if l.Next != nil {
		return l.Next.DistFromEnd() + 1
	}
	return 0
}

// Get number of lines in linked list
func (l *Line) ListLength() int {
	// 1 is added to account for self
	return l.DistFromStart() + l.DistFromEnd() + 1
}

// Find start of linked list
func (l *Line) FindStart() *Line {
	if l.Prev != nil {
		return l.Prev.FindStart()
	}
	return l
}

// Find end of linked list
func (l *Line) FindEnd() *Line {
	if l.Next != nil {
		return l.Next.FindEnd()
	}
	return l
}

// Flatten linked list to just a list
func (l *Line) Flatten() []string {
	list := []string{}
	cl := l.FindStart()

	for cl.Next != nil {
		list = append(list, cl.Contents)
		cl = cl.Next
	}

	// If last line isn't an empty newline, append newline
	if cl.Contents == "" {
		return append(list, cl.Contents)
	} else {
		list = append(list, cl.Contents)
		return append(list, "")
	}
}

// Flatten linked list to string
func (l *Line) ToString() string {
	return strings.Join(l.Flatten(), "\n")
}

// Copy linked list to new linked list. Note that this does NOT copy the line to
// the clipboard, but rather copies the structure itself to a new one. The
// pointers in either list won't interface with one another
func (l *Line) Copy() *Line {
	cl := l.FindStart()
	new := NewLine(cl.Contents, nil, nil)

	cl = cl.Next
	for cl != nil {
		new = new.InsertAfter(cl.Contents)
		cl = cl.Next
	}

	return new
}

// Clear linked list of lines, leaving only an empty head. Note that this just
// the line it's called on to be an empty line. The garbage collector should
// clean up the orphaned pointers, I think
func (l *Line) Clear() {
	l.Next = nil
	l.Prev = nil
	l.Contents = ""
}
