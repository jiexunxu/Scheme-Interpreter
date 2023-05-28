# Scheme-Interpreter
A simple interpreter, written in scheme, a functional programming language. The interpreter can interpret itself.

# Basic Setup
To run this project, open the terminal and follow these steps:
1) Unzip both files to a directory, and 'cd' to that directory.
2) Open a Scheme interpreter (such as MzScheme) under that directory.
3) Type (load 'mini_interpreter.scm')
4) Type (repl)
Now you're running a copy of the mini_interpreter. Before you play with it, you should first do step 5.
5) Type (load 'library.scm')
After step 5, the required libraries are loaded. At this point, you can type prompts such as (+ 1 2), (define x 3) to play with the interpreter. Simply type (exit) to exit the mini-interpreter.

# Self-interpreting
After basic setup above, follow these steps to run a copy of the mini-interpreter on top of itself, that is, let the mini-interpreter interpret itself.
1) Type (load 'mini_interpreter.scm')
2) Type (repl)
3) Type (load 'library.scm')
Now you're running two copies of the mini-interpreter, one on top of the other. You will need two (exit) to return to exit both copies of the mini-interpreter.
