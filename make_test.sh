#!/usr/bin/env bash

export PATH="$PATH:$HOME/.rvm/bin"
source "$HOME/.rvm/scripts/rvm"

neruda_gem=$(realpath $1)

cd
rm -rf nerudatest
mkdir nerudatest
cd nerudatest
rvm use ruby-2.4.1
rvm --force gemset delete nerudatest
rvm gemset create nerudatest
rvm gemset use nerudatest
gem install $neruda_gem
echo 'ruby-2.4.1@nerudatest' > .ruby-version
pwd
