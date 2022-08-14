#!/bin/bash

mkdir -p build

# Lookup the last commit ID
GITVERSION="$(git rev-parse --short HEAD)"

# Check if any uncommitted changes in tracked files
if [ -n "$(git status --untracked-files=no --porcelain)" ]; then
  GITVERSION="${VERSION}?"
fi

echo -e "version:\n    ${VERSION}"

beebasm -S GITVERSION=$GITVERSION -i src/SBC_MOS.asm -v  -o build/SBC_MOS |& tee build/SBC_MOS.lst
