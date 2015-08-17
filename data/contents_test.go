package data_test

import (
	"github.com/bread-editor/bread/data"
	"testing"
)

// Testing the contents structures
func TestLines(t *testing.T) {
	// Make EmptyLine returns the same as NewLine called with an empty
	// contents argument
	if *data.EmptyLine(nil, nil) != *data.NewLine("", nil, nil) {
		t.Fatalf("Lines created by \"EmptyLine\" and \"NewLine\" are not the same")
	}

	// Test adding lines
	currentLine := data.EmptyLine(nil, nil)
	firstLine := currentLine
	for i := 0; i < 998; i++ {
		currentLine = data.EmptyLine(nil, currentLine)
	}

	currentLine.InsertBefore("Hello World")

	if currentLine.DistFromStart() != 999 {
		t.Fatalf("Line distance from start is not 999")
	}

	if currentLine.DistFromEnd() != 0 {
		t.Fatalf("Line distance from end is not 0")
	}

	lastLine := currentLine
	currentLine = currentLine.FindStart()

	if currentLine.DistFromEnd() != 999 {
		t.Fatalf("Line distance from end is not 999")
	}

	if currentLine.ListLength() != 1000 {
		t.Fatalf("Supposed to get length of 1000, only got %d", currentLine.ListLength())
	}

	// Test Forward and Backward
	if firstLine.Forward(100) != lastLine.Backward(lastLine.ListLength()-100-1) {
		t.Log(firstLine.Forward(100).DistFromStart())
		t.Log(lastLine.Backward(lastLine.ListLength() - 100).DistFromStart())
		t.Fatalf("Forward and backward not working properly")
	}

	// Test finding the first and last lines
	if currentLine.FindStart() != firstLine {
		t.Fatalf("First line does not match the return value of FindStart")
	}

	if currentLine.FindEnd() != lastLine {
		t.Fatalf("Last line does not match the return value of FindEnd")
	}

	currentLine = lastLine
	// Test removing lines
	for i := 0; i < 999; i++ {
		prevLine := currentLine.Prev
		currentLine.Remove()
		currentLine = prevLine
	}

	if currentLine.ListLength() != 1 {
		t.Fatalf("Lines not removed properly. Expected length of 1")
	}
	if currentLine.DistFromStart() != 0 {
		t.Fatalf("DistFromStart not returning 0 on a list of length 1")
	}
	if currentLine.DistFromEnd() != 0 {
		t.Fatalf("DistFromEnd not returning 0 on a list of length 1")
	}

	// Test flattening lines
	currentLine.Contents = "Hello"
	for i := 0; i < 5; i++ {
		currentLine = currentLine.InsertAfter("World")
	}

	testStrings := currentLine.Flatten()

	for i, s := range []string{"Hello", "World", "World", "World", "World", "World"} {
		if s != testStrings[i] {
			t.Fatalf("Flatten broke at index %d on %s", i, s)
		}
	}

	if currentLine.ToString() != "Hello\nWorld\nWorld\nWorld\nWorld\nWorld\n" {
		t.Log(currentLine.ToString())
		t.Fatalf("ToString() not returning proper string")
	}

	// Test copying line structures
	newCurrentLine := currentLine.Copy()
	if newCurrentLine.ToString() != currentLine.ToString() {
		t.Fatalf("Copied structs contents do not match")
	}

	// Test clearing a line
	currentLine.Clear()
	if currentLine.Contents != "" {
		t.Log(currentLine.Contents)
		t.Fatalf("Line not cleared")
	}
}

// Benchmark adding lines
func benchmarkAddLines(num int, str string, b *testing.B) {
	for n := 0; n < b.N; n++ {
		testLine := data.NewLine(str, nil, nil)
		for i := 0; i < (num - 1); i++ {
			testLine = testLine.InsertAfter(str)
		}
	}
}

func BenchmarkAddLines0(b *testing.B) {
	benchmarkAddLines(1, "hello world", b)
}

func BenchmarkAddLines1(b *testing.B) {
	benchmarkAddLines(10, "hello world", b)
}

func BenchmarkAddLines2(b *testing.B) {
	benchmarkAddLines(100, "hello world", b)
}

func BenchmarkAddLines3(b *testing.B) {
	benchmarkAddLines(1000, "hello world", b)
}

func BenchmarkAddLines4(b *testing.B) {
	benchmarkAddLines(1000000, "hello world", b)
}

// Benchmark copying line structures
func benchmarkCopyLines(numLines int, contained string, b *testing.B) {
	testLine := data.NewLine(contained, nil, nil)
	for i := 0; i < (numLines - 1); i++ {
		testLine = testLine.InsertAfter(contained)
	}

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		testLine = testLine.Copy()
	}
}

func BenchmarkCopyLines0(b *testing.B) {
	benchmarkCopyLines(1, "hello world", b)
}

func BenchmarkCopyLines1(b *testing.B) {
	benchmarkCopyLines(10, "hello world", b)
}

func BenchmarkCopyLines2(b *testing.B) {
	benchmarkCopyLines(100, "hello world", b)
}

func BenchmarkCopyLines3(b *testing.B) {
	benchmarkCopyLines(1000, "hello world", b)
}

func BenchmarkCopyLines4(b *testing.B) {
	benchmarkCopyLines(10000, "hello world", b)
}

func BenchmarkCopyLines5(b *testing.B) {
	benchmarkCopyLines(100000, "hello world", b)
}

func BenchmarkCopyLines6(b *testing.B) {
	benchmarkCopyLines(1000000, "hello world", b)
}

// Benchmark removing lines
func benchmarkRemoveLines(numLines int, contained string, b *testing.B) {
	testLine := data.NewLine(contained, nil, nil)
	for i := 0; i < (numLines - 1); i++ {
		testLine = testLine.InsertAfter(contained)
	}

	tests := b.N
	var testLines []*data.Line
	for i := 0; i < tests; i++ {
		testLines = append(testLines, testLine.Copy())
	}

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		length := testLines[i].ListLength()

		for j := 0; j < length; j++ {
			testLines[i], _ = testLines[i].Remove()
		}
	}
}

func BenchmarkRemoveLines0(b *testing.B) {
	benchmarkRemoveLines(1, "hello world", b)
}

func BenchmarkRemoveLines1(b *testing.B) {
	benchmarkRemoveLines(10, "hello world", b)
}

func BenchmarkRemoveLines2(b *testing.B) {
	benchmarkRemoveLines(100, "hello world", b)
}

func BenchmarkRemoveLines3(b *testing.B) {
	benchmarkRemoveLines(1000, "hello world", b)
}

func BenchmarkRemoveLines4(b *testing.B) {
	benchmarkRemoveLines(10000, "hello world", b)
}

func BenchmarkRemoveLines5(b *testing.B) {
	benchmarkRemoveLines(100000, "hello world", b)
}

// NOTE: Up until this level, Remove exhibits a constant growth rate
func BenchmarkRemoveLines6(b *testing.B) {
	benchmarkRemoveLines(1000000, "hello world", b)
}

// Test clearing lines
func benchmarkClearLines(numLines int, contained string, b *testing.B) {
	testLine := data.NewLine(contained, nil, nil)
	for i := 0; i < (numLines - 1); i++ {
		testLine = testLine.InsertAfter(contained)
	}

	tests := b.N
	var testLines []*data.Line
	for i := 0; i < tests; i++ {
		testLines = append(testLines, testLine.Copy())
	}

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		length := testLines[i].ListLength()

		for j := 0; j < length; j++ {
			testLines[i].Clear()
		}
	}
}

func BenchmarkClearLines1(b *testing.B) {
	benchmarkClearLines(10, "hello world", b)
}

func BenchmarkClearLines2(b *testing.B) {
	benchmarkClearLines(100, "hello world", b)
}

func BenchmarkClearLines3(b *testing.B) {
	benchmarkClearLines(1000, "hello world", b)
}

func BenchmarkClearLines4(b *testing.B) {
	benchmarkClearLines(10000, "hello world", b)
}

func BenchmarkClearLines5(b *testing.B) {
	benchmarkClearLines(100000, "hello world", b)
}

func BenchmarkClearLines6(b *testing.B) {
	benchmarkClearLines(1000000, "hello world", b)
}

// Test flattening
func benchmarkFlattenLines(numLines int, contained string, b *testing.B) {
	testLine := data.NewLine(contained, nil, nil)
	for i := 0; i < (numLines - 1); i++ {
		testLine = testLine.InsertAfter(contained)
	}

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		testLine.Flatten()
	}
}

func BenchmarkFlattenLines1(b *testing.B) {
	benchmarkFlattenLines(10, "hello world", b)
}

func BenchmarkFlattenLines2(b *testing.B) {
	benchmarkFlattenLines(100, "hello world", b)
}

func BenchmarkFlattenLines3(b *testing.B) {
	benchmarkFlattenLines(1000, "hello world", b)
}

func BenchmarkFlattenLines4(b *testing.B) {
	benchmarkFlattenLines(10000, "hello world", b)
}

func BenchmarkFlattenLines5(b *testing.B) {
	benchmarkFlattenLines(100000, "hello world", b)
}

func BenchmarkFlattenLines6(b *testing.B) {
	benchmarkFlattenLines(1000000, "hello world", b)
}

// Test converting to String
func benchmarkToStringLines(numLines int, contained string, b *testing.B) {
	testLine := data.NewLine(contained, nil, nil)
	for i := 0; i < (numLines - 1); i++ {
		testLine = testLine.InsertAfter(contained)
	}

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		testLine.ToString()
	}
}

func BenchmarkToStringLines1(b *testing.B) {
	benchmarkToStringLines(10, "hello world", b)
}

func BenchmarkToStringLines2(b *testing.B) {
	benchmarkToStringLines(100, "hello world", b)
}

func BenchmarkToStringLines3(b *testing.B) {
	benchmarkToStringLines(1000, "hello world", b)
}

func BenchmarkToStringLines4(b *testing.B) {
	benchmarkToStringLines(10000, "hello world", b)
}

func BenchmarkToStringLines5(b *testing.B) {
	benchmarkToStringLines(100000, "hello world", b)
}

func BenchmarkToStringLines6(b *testing.B) {
	benchmarkToStringLines(1000000, "hello world", b)
}
