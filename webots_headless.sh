#!/bin/bash
export AUDIODEV=null
export DEBIAN_FRONTEND=noninteractive
export DISPLAY=:99
export LIBGL_ALWAYS_SOFTWARE=true
webots --stdout --stderr --batch --no-sandbox
