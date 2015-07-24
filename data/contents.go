package data

import (
	"strings"
)

// This is the basic type of structure for holding contents in Bread. It is
// similar to a linked list, except that it also tracks the previous line as
// well. We'll improperly refer to it as a "linked list" in the rest of the
// documentation when referencing what the type of data structure is.
type Line struct {
	Contents string
	Next     *Line
	Prev     *Line
}

// Courtesy name to help thinking about the Line struct conceptually
type LinkedLines Line

// Create a new line and update the linked list
func NewLine(contents string, next *Line, prev *Line) *Line {
	new_l := &Line{Contents: contents, Next: next, Prev: prev}

	if prev != nil {
		prev.Next = new_l
	}

	if next != nil {
		next.Prev = new_l
	}

	return new_l
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
func (l *Line) Remove() {
	if l.Next != nil && l.Prev != nil {
		l.Next.Prev = l.Prev
		l.Prev.Next = l.Next
	} else if l.Prev == nil && l.Next != nil {
		l.Next.Prev = nil
	} else if l.Prev != nil && l.Next == nil {
		l.Prev.Next = nil
	}
}

// Insert a line before the current line
func (l *Line) InsertBefore(contents string) {
	NewLine(contents, l, l.Prev)
}

// Insert a line after the current line
func (l *Line) InsertAfter(contents string) {
	NewLine(contents, l.Next, l)
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
func (l *Line) Length() int {
	return l.DistFromStart() + l.DistFromEnd()
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

// Combine linked list to just a list
func (l *Line) Flatten() []string {
	list := []string{}
	cl := l.FindStart()

	for cl.Next != nil {
		list = append(list, cl.Contents)
		cl = cl.Next
	}

	return append(list, cl.Contents)
}

// Flatten linked list to string
func (l *Line) ToString() string {
	return strings.Join(l.Flatten(), "\n")
}
