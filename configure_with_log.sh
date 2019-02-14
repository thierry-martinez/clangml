#!/usr/bin/env bash
PREFIX="$1"
./configure --prefix="$PREFIX" || ( cat config.log && exit 1 )
