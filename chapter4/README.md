# Chapter 4

This project contains an A.I. for the ladder game.

The three ladder subdirectories contain different versions of this project.

* ladder-slow
  * Implements the graph and permuation map with the `AssocMap` type.
* ladder-fast
  * Implements the graph and permuation map with the `HashMap` type.
* ladder-optimized
  * Implements the graph and permuation map with the `HashMap` type, replaces occurrences of `String` with `ByteString` and uses a bidirectional breadth-first search implementation.

## Exercises

The exercises in the chapter can be found in the ladder-slow project. The implementation of the bidirectional breadth-first search implementation from the last exercise can be found in the ladder-optimized project.
