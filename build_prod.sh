#! /usr/bin/env bash

nix-build --arg production true
# Ignore time stamps since we will copy from the nix store
rsync --archive --verbose --recursive --delete --ignore-times --chmod=u=rwX result/ docs/
touch docs/.nojekyll

