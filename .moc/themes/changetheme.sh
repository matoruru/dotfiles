#!/bin/bash

if [[ -f mytheme ]]; then
   rm mytheme
fi
cp $1 mytheme
