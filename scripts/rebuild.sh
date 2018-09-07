#!/bin/bash
set -x

cd ~/.xmonad

if [ ! -f xmobar ]; then
    stack --local-bin-path=`pwd` install xmobar
fi

stack --local-bin-path=`pwd` install
