#!/bin/sh

set -e

for dir in "$@"
do
    if [ -f $dir/config.ini ]
    then
        delay=$(perl -ne 'if (/^hold *= *(\d+)/) { print $1 / 10, "\n"; exit }' < $dir/config.ini)
        convert -crop 16x16 +repage $dir/0.bmp $dir/frame_%02d.bmp
        convert -delay $delay -loop 0 $dir/frame_*.bmp $dir.gif
        rm $dir/frame_*.bmp
    else
        echo 1>&2 skipped $dir
    fi
done
                
