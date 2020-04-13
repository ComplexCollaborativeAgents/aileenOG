#!/bin/bash
export AUDIODEV=null
export DEBIAN_FRONTEND=noninteractive
export DISPLAY=:99
export LIBGL_ALWAYS_SOFTWARE=true
xvfb-run --server-num=99 --server-args="-screen 0 1024x768x16" webots --mode=fast --stdout --stderr --batch --no-sandbox world/data/aileen_world.wbt

