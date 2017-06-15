#!/usr/bin/env sh

pandoc -t markdown_github -o README.md README.org
gem build neruda.gemspec
