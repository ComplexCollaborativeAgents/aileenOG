pkill lisp
# Currently there is one argument for the port number
# and one optional argument for kbdir (in analogystack/planb/kbs).
cd "${0%/*}"
# Process port number.
if [ "$1" == "" ]; then
  echo "Please specify a port number!"
  exit 1
fi
echo "concept learner port number = $1"
# Clean up from last run.
rm concept.log
# Process kbdir.
kbdir=$2
rm -rf analogystack/planb/kbs/nextkb/*
if [ "$kbdir" == "" ]; then
  kbdir="analogystack/planb/kbs/nextkb.zip"
  unzip analogystack/planb/kbs/nextkb.zip -d analogystack/planb/kbs/nextkb/
else
  cp -r $kbdir/* analogystack/planb/kbs/nextkb/
fi
echo "concept learner initialized from $kbdir"
# Run Lisp.
screen /usr/local/acl10.1.64/mlisp8 -q -L server.lsp -e "(progn (aileen::start-server :port $1 :kbdir \"nextkb\") (do ()(nil nil)(sleep 10)))" > concept.log
