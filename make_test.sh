#!/usr/bin/env bash

[ "z$1" = z ] && echo 'Please give me the path to the neruda gem to test' \
  && exit 1

export PATH="$PATH:$HOME/.rvm/bin"
source "$HOME/.rvm/scripts/rvm"

neruda_gem=$(realpath $1)

rubyver=$(cat .ruby-version)

cd
rm -rf nerudatest
mkdir nerudatest
cd nerudatest
rvm use $rubyver
rvm --force gemset delete nerudatest
rvm gemset create nerudatest
rvm gemset use nerudatest
gem install $neruda_gem
echo "$rubyver@nerudatest" > .ruby-version
pwd
