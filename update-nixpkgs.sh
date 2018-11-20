#!/bin/bash

# Update the pinned Nixpkgs version to the latest commit in the specified
# channel. Uses the Github API to resolve the channel ref to a commit hash.

channel='nixos-18.09'
commit_hash=$(curl --silent "https://api.github.com/repos/NixOS/nixpkgs-channels/git/refs/heads/$channel" | jq .object.sha --raw-output)
echo "Prefetching ${channel} ${commit_hash} ..."
archive_hash=$(nix-prefetch-url --unpack "https://github.com/NixOS/nixpkgs/archive/${commit_hash}.tar.gz")

cat << EOF > nixpkgs-pinned.nix
fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${commit_hash}.tar.gz";
  sha256 = "${archive_hash}";
}
EOF

git add nixpkgs-pinned.nix
git commit -m "Upgrade to latest commit in $channel channel"
