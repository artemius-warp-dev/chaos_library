#!/bin/bash

link=$(xclip -o)
youtube-dl -x --audio-format mp3 --output '~/Music/youtube/%(title)s.%(ext)s' --no-playlist "$link"
