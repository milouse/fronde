# frozen_string_literal: true

require './lib/neruda/version'

Gem::Specification.new do |s|
  s.name        = 'neruda'
  s.version     = Neruda::VERSION
  s.summary     = 'A simplistic way to create an Org static website.'
  s.description = <<~DESC
    An opinionated Org static website generator.
    Write your org-mode files, we take care of the rest.
  DESC
  s.authors     = ['Étienne Deparis']
  s.email       = 'etienne@depar.is'
  s.files       = ['lib/neruda/config.rb',
                   'lib/neruda/config/lisp_config.rb',
                   'lib/neruda/config/org-config.el',
                   'lib/neruda/config/ox-neruda.el',
                   'lib/neruda/emacs.rb',
                   'lib/neruda/index.rb',
                   'lib/neruda/index/atom_generator.rb',
                   'lib/neruda/index/org_generator.rb',
                   'lib/neruda/org_file.rb',
                   'lib/neruda/org_file/class_methods.rb',
                   'lib/neruda/org_file/extracter.rb',
                   'lib/neruda/org_file/htmlizer.rb',
                   'lib/neruda/templater.rb',
                   'lib/neruda/utils.rb',
                   'lib/neruda/preview.rb',
                   'lib/neruda/version.rb',
                   # Rake tasks
                   'lib/tasks/org.rake',
                   'lib/tasks/site.rake',
                   'lib/tasks/sync.rake',
                   'lib/tasks/tags.rake',
                   # Translations
                   'locales/en.yml',
                   'locales/fr.yml',
                   # Default theme
                   'themes/default/css/htmlize.css',
                   'themes/default/css/style.css',
                   'themes/default/img/bottom.png',
                   'themes/default/img/tic.png',
                   'themes/default/img/top.png',
                   # Others
                   'LICENSE']
  s.executables = ['pablo']
  s.homepage    = 'https://git.umaneti.net/neruda/about/'
  s.license     = 'WTFPL'

  s.rdoc_options << '-m' << 'org'
  s.extra_rdoc_files = ['LICENSE']

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
  s.add_development_dependency 'rubocop', '~> 0.90'
  s.add_development_dependency 'simplecov', '~> 0.19'
  s.add_development_dependency 'yard', '~> 0.9'
end
