# Chapter 4

This project contains an A.I. for the ladder game.

The three ladder subdirectories contain different versions of this project.

* ladder-slow
  * Implements the graph and permuation map with the `AssocMap` type.
* ladder-fast
  * Implements the graph and permuation map with the `HashMap` type.
* ladder-optimized
  * Implements the graph and permuation map with the `HashMap` type, replaces occurrences of `String` with `ByteString` and uses a bidirectional breadth-first search implementation.
  
The large dictionary has been taken from http://www.mieliestronk.com/corncob_lowercase.txt. The small dictionary is a selection of 200 english words.

When using the large dictionary, only the optimized version of this project has a bareable run-time. See the chapter in the book for more details.

## Exercises

The exercises in the chapter can be found in the ladder-slow project. The implementation of the bidirectional breadth-first search implementation from the last exercise can be found in the ladder-optimized project.
