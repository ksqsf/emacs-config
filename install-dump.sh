#!/bin/sh

DIR=/Applications/Emacs.app/Contents/MacOS

if [ -f $DIR/.Dumped ]; then
    rm $DIR/Emacs.pdmp
    cp ~/.emacs.d/emacs.pdmp $DIR/Emacs.pdmp
else
    mv $DIR/Emacs.pdmp $DIR/Emacs.pdmp.bak
    cp ~/.emacs.d/emacs.pdmp $DIR/Emacs.pdmp
    touch $DIR/.Dumped
fi
