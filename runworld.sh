export WEBOTS_HOME="/usr/local/webots"
export PYTHONPATH="$WEBOTS_HOME/lib/controller/python27:$PYTHONPATH"
export LD_LIBRARY_PATH="$WEBOTS_HOME/lib/controller"
export SWT_GTK3=0
python world
