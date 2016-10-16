#!/bin/bash
set -x
cd ~/.xmonad && stack --local-bin-path=`pwd` install
