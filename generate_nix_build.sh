#! /usr/bin/env bash

elm2nix convert > elm-srcs.nix
elm2nix snapshot > registry.dat

