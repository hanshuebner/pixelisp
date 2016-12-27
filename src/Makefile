
all:
	buildapp --output game-frame --eval '(load "load.lisp")' --entry server::main
	mv game-frame ..
	sudo /sbin/setcap 'cap_net_bind_service=+ep' ../game-frame
