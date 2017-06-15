# coding: utf-8
# frozen_string_literal: true

Gem::Specification.new do |s|
  s.name        = 'neruda'
  s.version     = '0.0.2'
  s.date        = '2017-06-14'
  s.summary     = 'A simplistic way to publish a book online.'
  s.description = <<~EOF
    A simplistic way to publish a book online.
    Write your org files, we take care of the rest.
  EOF
  s.author      = 'Étienne Deparis'
  s.email       = 'etienne@depar.is'
  s.files       = ['lib/neruda.rb',
                   'lib/neruda/chapter.rb',
                   'lib/neruda/url.rb',
                   # Rake tasks
                   'lib/tasks/chapters.rake',
                   'lib/tasks/sinatra.rake',
                   'lib/tasks/capistrano/chapters.rake',
                   'lib/tasks/capistrano/sinatra.rake',
                   # Various template examples
                   'lib/assets/chapter.slim',
                   'lib/assets/layout.slim',
                   'lib/assets/index.slim',
                   'lib/assets/style.css',
                   # Bootstrap config files
                   'docs/Rakefile.example',
                   'docs/config.yml.example',
                   'README.md',
                   'LICENSE']
  s.executables = ['pablo']
  s.homepage    = 'https://git.deparis.io/neruda/about/'
  s.license     = 'WTFPL'

  s.extra_rdoc_files = ['README.md', 'LICENSE']

  s.required_ruby_version = '>= 2.4'

  s.add_runtime_dependency('rainbow', '~> 2.2')
  s.add_runtime_dependency('org-ruby', '~> 0.9')
  s.add_runtime_dependency('slim', '~> 3.0')
  s.add_runtime_dependency('thin', '~> 1.7')
  s.add_runtime_dependency('sinatra', '~> 2.0')
end
