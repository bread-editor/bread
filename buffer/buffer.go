package buffer

// The data structure that holds buffer information.
type Buffer struct {
	Name        string
	Writable    bool
	Contents    *Line
	CurrentLine *Line
	FileName    string
	FilePath    string
}

// Make a scratch buffer
func ScratchBuffer() Buffer {
	emptyLine := NewLine("", nil, nil)

	return Buffer{
		Name:        "*scratch*",
		Writable:    true,
		Contents:    emptyLine,
		CurrentLine: emptyLine,
		FileName:    "",
		FilePath:    "",
	}
}
