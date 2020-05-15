#!/usr/bin/env bash
find  | grep .hs |grep src| entr cabal build
