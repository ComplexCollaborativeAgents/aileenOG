#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

if ! command -v sudo; then
  apt-get update --fix-missing
  apt-get install -y sudo
else
  sudo apt-get update --fix-missing
fi

sudo apt-get install -y python-setuptools build-essential libgl1-mesa-glx

(cd agent/vision; GPU=0 OPENCV=0 REBUILD=1 python2 setup.py build_ext)
conda env create --force --file environment.yml

AILEEN_ENV="$(conda config --show envs_dirs | grep -o "/.*" | head -1)/aileen"

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
  git clone https://github.com/google/re2;
  cd re2;
  git checkout 2018-04-01;
  make -j4 install prefix="" DESTDIR="$AILEEN_ENV"
)

(
  cd /tmp;
  wget http://www.openfst.org/twiki/pub/GRM/PyniniDownload/pynini-1.9.3.tar.gz;
  tar xf pynini-1.9.3.tar.gz;
  cd pynini-1.9.3;
  source activate aileen;
  CPATH="$AILEEN_ENV/include" python setup.py install
)
