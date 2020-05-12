#!/bin/bash -e
#Kill process on dubs
ssh dubs << 'EOF'
kill $(ps -u aileen | grep mlisp | awk '{print $1}')
EOF
