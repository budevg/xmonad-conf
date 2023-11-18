#!/bin/bash
set -x

cd ~/.xmonad

stack --nix --local-bin-path=`pwd` install
