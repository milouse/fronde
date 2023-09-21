# frozen_string_literal: true

require './lib/fronde/version'

Gem::Specification.new do |s|
  s.name        = 'fronde'
  s.version     = Fronde::VERSION
  s.summary     = 'An opinionated static website generator for Org'
  s.description = <<~DESC
    Fronde helps you to convert Org mode files into websites, giving you
    full control over the publication process.
  DESC
  s.authors     = ['Étienne Deparis']
  s.email       = 'etienne@depar.is'
  s.metadata    = {
    'rubygems_mfa_required' => 'true',
    'source_code_uri' => 'https://git.umaneti.net/fronde',
    'homepage_uri' => 'https://etienne.depar.is/fronde/',
    'funding_uri' => 'https://liberapay.com/milouse'
  }
  s.files       = ['lib/fronde/cli/commands.rb',
                   'lib/fronde/cli/data/gitignore',
                   'lib/fronde/cli/data/Rakefile',
                   'lib/fronde/cli/helpers.rb',
                   'lib/fronde/cli.rb',
                   'lib/fronde/config/data/org-config.el',
                   'lib/fronde/config/data/ox-fronde.el',
                   'lib/fronde/config/helpers.rb',
                   'lib/fronde/config/lisp.rb',
                   'lib/fronde/config.rb',
                   'lib/fronde/emacs.rb',
                   'lib/fronde/index/atom_generator.rb',
                   'lib/fronde/index/data/template.xml',
                   'lib/fronde/index/org_generator.rb',
                   'lib/fronde/index.rb',
                   'lib/fronde/org_file/extracter.rb',
                   'lib/fronde/org_file/htmlizer.rb',
                   'lib/fronde/org_file.rb',
                   'lib/fronde/preview.rb',
                   'lib/fronde/source.rb',
                   'lib/fronde/templater.rb',
                   'lib/fronde/utils.rb',
                   'lib/fronde/version.rb',
                   # Monkey patches / extensions
                   'lib/ext/r18n.rb',
                   # Rake tasks
                   'lib/tasks/org.rake',
                   'lib/tasks/site.rake',
                   'lib/tasks/sync.rake',
                   'lib/tasks/tags.rake',
                   # Translations
                   'locales/en.yml',
                   'locales/fr.yml',
                   # Others
                   'LICENSE']
  s.executables = ['fronde']
  s.homepage    = 'https://git.umaneti.net/fronde/about/'
  s.license     = 'WTFPL'

  s.required_ruby_version = '>= 2.7'
  s.add_runtime_dependency 'liquid', '~> 5.4'
  s.add_runtime_dependency 'nokogiri', '~> 1.15'
  s.add_runtime_dependency 'r18n-core', '~> 5.0'
  s.add_runtime_dependency 'rainbow', '~> 3.1'
  s.add_runtime_dependency 'rake', '~> 13.0'
  s.add_runtime_dependency 'webrick', '~> 1.8'
end
