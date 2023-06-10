#!/bin/bash
set -x

cd ~/.xmonad

if [ ! -f xmobar ]; then
    stack --nix --local-bin-path=`pwd` install xmobar-0.44.2
fi

stack --nix --local-bin-path=`pwd` install
