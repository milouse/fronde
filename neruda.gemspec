# coding: utf-8
# frozen_string_literal: true

require './lib/neruda/version'

Gem::Specification.new do |s|
  s.name        = 'neruda'
  s.version     = Neruda::VERSION
  s.date        = '2017-09-27'
  s.summary     = 'A simplistic way to create an org-mode static website.'
  s.description = <<~DESC
    An opinionated org-mode static website generator.
    Write your org files, we take care of the rest.
  DESC
  s.author      = 'Étienne Deparis'
  s.email       = 'etienne@depar.is'
  s.files       = ['lib/neruda/config.rb',
                   'lib/neruda/index.rb',
                   'lib/neruda/org_file.rb',
                   'lib/neruda/version.rb',
                   # Rake tasks
                   'lib/tasks/org.rake',
                   'lib/tasks/site.rake',
                   'lib/tasks/sync.rake',
                   # Translations
                   'locales/en.yml',
                   'locales/fr.yml',
                   'docs/config.yml.example',
                   'LICENSE']
  s.executables = ['pablo']
  s.homepage    = 'https://fossil.deparis.io/neruda/'
  s.license     = 'WTFPL'

  s.extra_rdoc_files = ['LICENSE']

  s.required_ruby_version = '>= 2.4'
  s.add_dependency 'nokogiri', '~> 1.10'
  s.add_dependency 'r18n-core', '~> 3.2'
  s.add_dependency 'rake', '~> 12.3'
end
