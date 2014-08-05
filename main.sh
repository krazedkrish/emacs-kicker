#!/bin/bash

if [ -e ~/.emacs ]; then
    name=emacs$(date +%s).bak  
    mv ~/.emacs ~/$name
    echo "Old config has been renamed as $name"
fi

if [ -d ~/.emacs.d ]; then
    name=emacs.d$(date +%s).bak  
    mv ~/.emacs.d ~/$name
    echo "Old config has been renamed as $name"
fi


mkdir -p ~/.emacs.d
REPO=$PWD
cp $REPO/init.el ~/.emacs.d/
echo "Can start emacs now" 
