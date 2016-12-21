#!/bin/sh

set -ex

git pull
killall game-frame
make
sudo reboot
