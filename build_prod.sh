#! /usr/bin/env bash

nix-build --arg production true
rsync --archive --verbose --recursive --delete --chmod=u=rwX result/ docs/
touch docs/.nojekyll

