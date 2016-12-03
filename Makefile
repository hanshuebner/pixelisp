
all:
	buildapp --output game-frame --eval '(load "load.lisp")' --entry server::main
	sudo /sbin/setcap 'cap_net_bind_service=+ep' game-frame
