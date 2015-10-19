package ui

import (
	"github.com/bread-editor/bread/buffer"
)

// In Bread, there is 1 Modeline that all windows share
type Modeline struct {
	Content  string
	Editable bool
}

// Split types describe how the window is split
const (
	Vertical = iota
	Horizontal
)

// A tree-like structure representing Windows which can be split horizontally
// and vertically. When a window isn't split, it points to a buffer. However,
// once split, it no longer points to a buffer. But its children do
type Window struct {
	Buffer    *buffer.Buffer
	SplitType int
	Children  [2]*Window
}

// Example window layouts

// --------------------------
// |                        |
// |                        |
// |                        |
// --------------------------

// --------------------------
// |            |           |
// |            |           |
// |            |           |
// --------------------------

// --------------------------
// |            |           |
// |            |-----------|
// |            |           |
// --------------------------

// --------------------------
// |                        |
// |------------------------|
// |            |           |
// --------------------------

// Splits a window and combines it with the current one
func (w *Window) Split(nw *Window, split int) {
	w = &Window{
		Buffer:    nil,
		SplitType: split,
		Children:  [2]*Window{w, nw},
	}
}

type ModeLine struct {
	Contents string
}

type MessageLine struct {
	Contents string
}

type CommandBuffer struct {
	Visible  bool
	Contents string
	Prompt   string
}

type UI struct {
	Windows Window
	ML      ModeLine
	MB      CommandBuffer
}
