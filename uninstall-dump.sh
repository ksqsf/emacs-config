#!/bin/sh

DIR=/Applications/Emacs.app/Contents/MacOS

if [ -f $DIR/.Dumped ]; then
    mv $DIR/Emacs.pdmp.bak $DIR/Emacs.pdmp
    rm $DIR/.Dumped
fi
