
all:
	buildapp --output pixelisp --eval '(load "load.lisp")' --entry server::main
	mv pixelisp ..
	sudo /sbin/setcap 'cap_net_bind_service=+ep' ../pixelisp
