# coding: utf-8
# frozen_string_literal: true

Gem::Specification.new do |s|
  s.name        = 'neruda'
  s.version     = '0.0.1'
  s.date        = '2017-06-12'
  s.summary     = 'A simplistic way to publish book online.'
  s.description = 'Write your org files, we take care of the rest.'
  s.authors     = ['Étienne Deparis']
  s.email       = 'etienne@depar.is'
  s.files       = ['lib/neruda.rb', 'lib/neruda/chapter.rb',
                   'lib/neruda/url.rb', 'lib/tasks/chapters.rake',
                   'lib/tasks/sinatra.rake', 'lib/assets/chapter.slim',
                   'lib/assets/layout.slim', 'lib/assets/index.slim',
                   'lib/assets/style.css', 'docs/Capfile.example',
                   'docs/Rakefile.example', 'docs/config.yml.example',
                   'README.org', 'TODO.org', 'LICENSE']
  s.executables = ['pablo']
  s.homepage    = 'https://git.deparis.io/neruda/'
  s.license     = 'WTFPL'

  s.add_runtime_dependency('org-ruby', '~> 0.9')
  s.add_runtime_dependency('slim', '~> 3.0')
  s.add_runtime_dependency('thin', '~> 1.7')
  s.add_runtime_dependency('sinatra', '~> 2.0')
end
