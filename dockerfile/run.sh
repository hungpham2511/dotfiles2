#!/bin/bash

docker run -e DISPLAY=unix$DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix -it --rm \
    --privileged --device /dev/dri/ --ipc host --net host  \
    dotfile-test \
    /bin/bash
