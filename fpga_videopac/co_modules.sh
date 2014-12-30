#!/bin/bash

repo=${REPO:="-d /home/arnim/prg/vhdl/cvs"}
echo "Using CVS repository '$repo'"
co=${CO:=co}
echo "Using CVS command '$co'"

pushd sw
  rev=$REV; rev=${rev:=HEAD}
  cvs $repo $co -r $rev -d hex2rom core/hex2rom
popd

pushd src
  rev=$REV; rev=${rev:=rel_1_0}
  cvs $repo $co -r $rev -d t48 core/t48

  pushd board
    rev=$REV; rev=${rev:=HEAD}
    cvs $repo $co -r $rev -d gamepads core/gamepads

    pushd misc
      rev=$REV; rev=${rev:=HEAD}
      cvs $repo $co -r $rev -d ac97_ctrl core/ac97_ctrl

      rev=$REV; rev=${rev:=HEAD}
      cvs $repo $co -r $rev -d ps2 core/ps2
    popd
  popd
popd
