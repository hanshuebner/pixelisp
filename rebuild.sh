#!/bin/sh

git pull
killall run.sh
killall pixelisp

set -ex
cd src; make
sudo reboot
