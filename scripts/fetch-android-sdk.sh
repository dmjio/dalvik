#!/usr/bin/env bash

set -e

if [ ! -d "$HOME/android" ]
then

  pushd $HOME

  mkdir -p android

  pushd android

  wget http://dl.google.com/android/android-sdk_r24.2-linux.tgz
  tar -xvf android-sdk_r24.2-linux.tgz
  cd android-sdk-linux/tools
  echo y | ./android update sdk --no-ui -a -t build-tools-23.0.1

  popd
  popd

fi
