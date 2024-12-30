# About
This project is made to implement a simple solver that, given a formula $f$, finds all combinations of propositional variables that set $f$ to true.

[![asciicast](https://asciinema.org/a/hTHDs2pJwd5LnlAoPejW3qPnI.svg)](https://asciinema.org/a/hTHDs2pJwd5LnlAoPejW3qPnI)

# Inner workings
A TL;DR for the logic is that, we enumarete all possible valuations of a formula and evaluates the given formula for each valuation. We then filter out those that evaluates to false.  
This is an exponential operation, more sophisticated heuristics are planned to be implemented (I'm yet to learn them).

# As a web server
Run
```sh
docker build . -t "prop-solveur"
docker run -p 8080:8080 "prop-solveur"
```

# Execution
1. Install `stack` with your favorite package manager.
2. Run `stack run` to execute the program.

## Read from file
Use `stack run -- -f path/to/file` to run with file as input.

## Read from stdin
Use `stack run -- --stdin` to read from stdin

## Repl mode
Use `stack run -- -r` to solve interactively.

# Installation
If you want to use this tool anywhere, simply do `stack install .`.

Have fun :)
