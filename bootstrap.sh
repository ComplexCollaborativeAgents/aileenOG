#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

if [ -v GITLAB_CI ]; then
  SUDO=""
else
SUDO="sudo "
fi

$SUDO apt-get update --fix-missing
$SUDO apt-get install -y qhull-bin python-setuptools build-essential libgl1-mesa-glx unzip

(cd agent/vision; GPU=0 OPENCV=0 REBUILD=1 python2 setup.py build_ext)

AILEEN_ENV="$(conda config --show envs_dirs | grep -o "/.*" | head -1)/aileen"
# echo "$AILEEN_ENV"

(
  cd /tmp;
  wget https://github.com/cyberbotics/webots/releases/download/R2021a/webots_2021a_amd64.deb
  $SUDO apt install -y /tmp/webots_2021a_amd64.deb
)

(
  cd /tmp;
  wget http://soar.eecs.umich.edu/downloads/SoarSuite/SoarSuite_9.6.0-Multiplatform_64bit.zip --no-check-certificate;
  $SUDO unzip -d /usr/local /tmp/SoarSuite_9.6.0-Multiplatform_64bit.zip
  $SUDO /usr/local/SoarSuite_9.6.0-Multiplatform_64bit/SoarJavaDebugger.sh
  pkill java
)


(
  cd /tmp;
  wget http://www.openfst.org/twiki/pub/FST/FstDownload/openfst-1.6.8.tar.gz;
  tar xf openfst-1.6.8.tar.gz;
  cd openfst-1.6.8;
  ./configure --enable-grm --prefix="$AILEEN_ENV";
  make -j4 install
)

(
  cd /tmp;
  # git clone https://github.com/google/re2;
  wget https://github.com/google/re2/archive/refs/tags/2018-04-01.tar.gz;
  tar xf 2018-04-01.tar.gz;
  cd re2-2018-04-01;
  # git checkout 2018-04-01;
  make -j4 install prefix="" DESTDIR="$AILEEN_ENV"
)

(
  cd /tmp;
  wget http://www.openfst.org/twiki/pub/GRM/PyniniDownload/pynini-1.9.3.tar.gz;
  tar xf pynini-1.9.3.tar.gz;
  cd pynini-1.9.3;
  CPATH="$AILEEN_ENV/include" python setup.py install
)
