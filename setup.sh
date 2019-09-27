#!/bin/bash

echo "Looking for package manager..."
if [[ ! -z $(which yum 2>&-) ]]; then
  echo "Found yum."
  UPDATE="yum update"
  INSTALL="yum install"
elif [[ ! -z $(which apt-get 2>&-) ]]; then
  echo "Found apt-get."
  UPDATE="apt-get update -qq"
  INSTALL="apt-get install"
elif [[ ! -z $(which pacman 2>&-) ]]; then
  echo "Found pacman."
  UPDATE="pacman -Sy"
  INSTALL="pacman -S"
elif [[ ! -z $(which brew 2>&-) ]]; then
  echo "Found brew."
  UPDATE="brew update"
  INSTALL="brew install"
else
   echo "Error: no common package manager found, please add your own to $0!"
   exit 1;
fi

echo "1. Installing system requirements"
sudo $UPDATE
sudo $INSTALL git m4 make curl
read -p "Step 1 done. Press enter to continue."

echo "2. Installing opam: confirm prompts with y to adjust your shell config!"
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
read -p "Step 2 done. Press enter to continue."

echo "3. Installing opam packages"
opam update
opam install utop merlin ocp-indent
read -p "Step 3 done. Press enter to continue."

echo "4. Check"
opam --version
ocaml --version
echo 'print_endline "hello";;' | ocaml
