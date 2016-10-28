#!/bin/sh

xsetroot -cursor_name left_ptr

hash VBoxClient-all 2>/dev/null && { VBoxClient-all & }

exec xmonad
