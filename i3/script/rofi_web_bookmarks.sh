#!/bin/bash

BROWSER=google-chrome

surfraw -browser=$BROWSER "$(cat ~/.config/surfraw/bookmarks | sed '/^$/d' | sed '/^#/d' | sed '/^\//d' | sort -n | rofi -lines 9 -font "Iceberg 13" -dmenu -mesg "Add new bookmarks at ~/.config/surfraw/bookmarks" -i -p "rofi-surfraw-bookmarks: ")"
