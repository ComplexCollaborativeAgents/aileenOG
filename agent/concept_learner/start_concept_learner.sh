# Currently just one argument for the port number
# There should be some kind of conditional on nuking the KB
cd "${0%/*}"
rm concept.log
echo "hello"
echo $1
#rm -rf analogystack/planb/kbs/nextkb/*
#unzip analogystack/planb/kbs/nextkb.zip -d analogystack/planb/kbs/nextkb/
kill `pidof mlisp8`
/usr/local/acl10.1.64/mlisp8 -q -L server.lsp -e "(progn (aileen::start-server :port $1) (do ()(nil nil)(sleep 10)))" > concept.log
