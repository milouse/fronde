#!/usr/bin/env sh

pandoc -t markdown_github -o README.md README.org
GEMFILE=$(gem build neruda.gemspec | sed -n 's/.*File: \(.*\)$/\1/p')
echo "Successfully generated $GEMFILE"
[ "z$1" = ztest ] && ./make_test.sh "$GEMFILE"
