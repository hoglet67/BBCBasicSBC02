#!/bin/bash

mkdir -p build

# Lookup the last commit ID
GITVERSION="$(git rev-parse --short HEAD)"

# Check if any uncommitted changes in tracked files
if [ -n "$(git status --untracked-files=no --porcelain)" ]; then
  GITVERSION="${GITVERSION}?"
fi

echo -e "version:\n    ${GITVERSION}"

cd src
beebasm -i test.asm -v  -o ../build/test |& tee ../build/test.lst
beebasm -S GITVERSION=$GITVERSION -i combined.asm -v  -o ../build/SBC02 |& tee ../build/SBC02.lst
cd ..
