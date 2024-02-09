#!/usr/bin/env bash

if [ "$(uname)" == "Darwin" ]; then
  # use with --silent --show-error in CI
  curl https://github.com/facebook/dotslash/releases/download/v0.2.0/dotslash-macos.v0.2.0.tar.gz --output "dotslash-macos.v0.2.0.tar.gz"  --location --fail --retry 5
  tar xvf dotslash-macos.v0.2.0.tar.gz
  chmod +x ./dotslash
  if [ -e "$HOME/.local/bin/dotslash" ]; then
      echo "Found an existing $HOME/.local/bin/dotslash. Please remove it and run this command again."
  else
    cp ./dotslash ~/.local/bin/
  fi
fi
