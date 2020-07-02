# crossword

A ClojureScript demo presenting a custom React-like solution for creating a crossword.

## Development

To get started, install JVM, Clojure, Leiningen and Calva (VSCode plugin).

Open the project in VSCode and run 

    Calva: Start a Project REPL and Connect (aka Jack-In)
	Leiningen + Figwheel Main
	fig
	dev
	(wait for Clojure REPL to start)
	dev
	(wait for ClojureScript REPL to start)

This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

You can evaluate pieces of code either in the REPL or in the editor.
Type some code in the editor (for example the alert example), move the cursor
inside or at end of the block and execute Evaluate Top Level Form or some other
interesting evaluation option.

Paredit is integrated in Calva. Some useful commands: Expand Selection, Slurp, Barf, Kill Sexp.

The comment section at the end of core.cljs provides some things to evaluate.

## License

WTFPL