#!/bin/sh

## echo sox "$1" "${1%.*}.mp3"
sox "$1" "${1%.*}.mp3"
# ffmpeg -y -t 1 -i "$1" "${1%.*}.mp3"
