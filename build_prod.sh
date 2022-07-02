#! /usr/bin/env bash

nix-build --arg production true
rsync --archive --quiet --delete --chmod=u=rwX result/ docs/
touch docs/.nojekyll

