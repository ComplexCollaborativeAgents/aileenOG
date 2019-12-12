# Currently there is one argument for the port number
# and one optional argument for kbdir (in analogystack/planb/kbs).
cd "${0%/*}"
# Process port number.
if [ "$1" == "" ]; then
  echo "Please specify a port number!"
  exit 1
fi
echo "concept learner port number = $1"
# Process kbdir.
kbdir=$2
if [ "$kbdir" == "" ]; then
  set kbdir="nextkb"
  echo "Initializing default kb."
  rm -rf analogystack/planb/kbs/nextkb/*
  unzip analogystack/planb/kbs/nextkb.zip -d analogystack/planb/kbs/nextkb/
fi
echo "concept learner kbdir = $2 (in analogystack/planb/kbs)"
# Run Lisp.
kill `pidof mlisp8`
rm concept.log
/usr/local/acl10.1.64/mlisp8 -q -L server.lsp -e "(progn (aileen::start-server :port $1 :kbdir \"$kbdir\") (do ()(nil nil)(sleep 10)))" > concept.log
