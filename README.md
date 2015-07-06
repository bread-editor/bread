[![Circle CI](https://circleci.com/gh/bread-editor/bread.svg?style=svg)](https://circleci.com/gh/bread-editor/bread) [![Build Status](https://travis-ci.org/bread-editor/bread.svg?branch=master)](https://travis-ci.org/bread-editor/bread)

Bread
======
Bread is an extensible text-editor written in Haskell. It takes inspiration from those that came before it, namely (Neo)Vim and Emacs.

What is Bread?
--------------
Bread is a text-editor being written in Haskell. Essentially, Bread acts as a "server", handling requests over TCP from plugins and UIs. You can find out more about Bread [on the github page](https://bread-editor.github.io).

Baking Bread
------------
```
cabal configure --enable-coverage --enable-benchmarks --enable-tests -v2
cabal sandbox init
cabal install --dependencies-only
cabal build
```
