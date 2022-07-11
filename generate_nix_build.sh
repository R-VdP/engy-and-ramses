#! /usr/bin/env nix-shell
#! nix-shell -p elm2nix -i bash

repo_dir="$(dirname "$(realpath "${0}")")"

cd "${repo_dir}"

mkdir --parents "${repo_dir}/nix/"

elm2nix convert > "${repo_dir}/nix/elm-srcs.nix"
elm2nix snapshot
mv ./registry.dat "${repo_dir}/nix/registry.dat"

