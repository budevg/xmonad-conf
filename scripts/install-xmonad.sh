#!/bin/bash
set -x
echo "Creating xmonad xsession configuration..."
sudo mv /usr/share/xsessions/xmonad.desktop /usr/share/xsessions/xmonad.desktop.original
sudo cp ~/.xmonad/xsession/xmonad.desktop /usr/share/xsessions
sudo chmod a+r /usr/share/xsessions/xmonad.desktop
