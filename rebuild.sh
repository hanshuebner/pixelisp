#!/bin/sh

git pull
killall run.sh
killall game-frame

set -ex
make
sudo reboot
