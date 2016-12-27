#!/bin/sh

git pull
killall run.sh
killall game-frame

set -ex
cd src; make
sudo reboot
