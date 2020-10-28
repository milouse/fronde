# frozen_string_literal: true

require './lib/fronde/version'

Gem::Specification.new do |s|
  s.name        = 'fronde'
  s.version     = Fronde::VERSION
  s.summary     = 'A simplistic way to create an Org static website.'
  s.description = <<~DESC
    An opinionated Org static website generator.
    Write your org-mode files, we take care of the rest.
  DESC
  s.authors     = ['Étienne Deparis']
  s.email       = 'etienne@depar.is'
  s.files       = ['lib/fronde/cli.rb',
                   'lib/fronde/cli/commands.rb',
                   'lib/fronde/config.rb',
                   'lib/fronde/config/lisp_config.rb',
                   'lib/fronde/config/org-config.el',
                   'lib/fronde/config/ox-fronde.el',
                   'lib/fronde/emacs.rb',
                   'lib/fronde/index.rb',
                   'lib/fronde/index/atom_generator.rb',
                   'lib/fronde/index/org_generator.rb',
                   'lib/fronde/org_file.rb',
                   'lib/fronde/org_file/class_methods.rb',
                   'lib/fronde/org_file/extracter.rb',
                   'lib/fronde/org_file/htmlizer.rb',
                   'lib/fronde/templater.rb',
                   'lib/fronde/utils.rb',
                   'lib/fronde/preview.rb',
                   'lib/fronde/version.rb',
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

  s.required_ruby_version = '>= 2.6'
  s.add_runtime_dependency 'nokogiri', '~> 1.10'
  s.add_runtime_dependency 'r18n-core', '~> 4.0'
  s.add_runtime_dependency 'rainbow', '~> 3.0'
  s.add_runtime_dependency 'rake', '~> 13.0'

  s.add_development_dependency 'byebug', '~> 11.1'
  s.add_development_dependency 'org-ruby', '~> 0.9'
  s.add_development_dependency 'pry', '~> 0.13'
  s.add_development_dependency 'pry-doc', '~> 1.1'
  s.add_development_dependency 'rspec', '~> 3.9'
  s.add_development_dependency 'rubocop', '~> 0.93'
  s.add_development_dependency 'rubocop-performance', '~> 1.8'
  s.add_development_dependency 'rubocop-rspec', '~> 1.43'
  s.add_development_dependency 'simplecov', '~> 0.19'
  s.add_development_dependency 'yard', '~> 0.9'
end
