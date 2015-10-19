[![Circle CI](https://circleci.com/gh/bread-editor/bread.svg?style=svg)](https://circleci.com/gh/bread-editor/bread) [![Build Status](https://travis-ci.org/bread-editor/bread.svg?branch=master)](https://travis-ci.org/bread-editor/bread)
[![Coverage Status](https://coveralls.io/repos/bread-editor/bread/badge.svg?branch=master&service=github)](https://coveralls.io/github/bread-editor/bread?branch=master)

# Bread
Bread is an extensible text-editor written in Go. It takes inspiration from those that came before it, namely (Neo)Vim and Emacs.

## What is Bread?
Bread is a text-editor being written in Go. Essentially, Bread acts as a "server", handling requests over TCP from plugins and UIs. You can find out more about Bread [on the github page](https://bread-editor.github.io).

## Baking Bread
```
go get github.com/bread-editor/bread
```

## Bread Goals
- [ ] Editor
  - [ ] Functionality
	- [ ] Loading Files
	  - [x] Parse file to linked-list structure
	- [ ] Syntax Highlighting
	- [ ] Asynchronous calls
	- [ ] Keymaps
	- [ ] Colorschemes
  - [ ] IDE Features
	- [ ] Standardized autocompletion
	- [ ] Pop-ups
	- [ ] Linting
	- [ ] REPLs
- [ ] GUI
  - [ ] Terminal GUI
  - [ ] QT GUI
  - [ ] GTK GUI??
- [ ] API
  - [ ] Decide on API Structure
  - [ ] Parse msgpack'd calls

### Release Goals
#### Yeast
- [ ] Basic Editing Functionality
- [ ] Tentative API definition
- [ ] Basic GUI
- [ ] Init file

#### Batter
- [ ] Finalized API definition
- [ ] Syntax Highlighting
- [ ] Custom Keymaps
- [ ] Custom Colorschemes

#### Dough
- [ ] Fully exposed API
- [ ] QT GUI and Terminal UI finished
- [ ] Mostly ready for release

#### Bread
- [ ] Bug fixes
