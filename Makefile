
all:
	buildapp --output game-frame --eval '(load "load.lisp")' --entry server::main
