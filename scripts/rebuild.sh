#!/bin/bash
set -x

cd ~/.xmonad

if [ ! -f xmobar ]; then
    stack --nix --local-bin-path=`pwd` install xmobar
fi

stack --nix --local-bin-path=`pwd` install
