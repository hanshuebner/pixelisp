#!/bin/sh

while true
do
	mv -f log/pixelisp.log log/pixelisp.log.old
	./pixelisp
	sleep 1
done
