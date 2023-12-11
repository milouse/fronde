# frozen_string_literal: true

require './lib/fronde/version'

Gem::Specification.new do |spec|
  spec.name        = 'fronde'
  spec.version     = Fronde::VERSION
  spec.summary     = 'An opinionated static website generator for Org'
  spec.description = <<~DESC
    Fronde helps you to convert Org mode files into websites, giving you
    full control over the publication process.
  DESC
  spec.post_install_message = <<~POSTINST
    Start your first fronde project with:

        fronde new myproject

    Update your existing projects with:

        fronde update
        fronde build -f

  POSTINST
  spec.authors     = ['Étienne Deparis']
  spec.email       = 'etienne@depar.is'
  spec.metadata    = {
    'rubygems_mfa_required' => 'true',
    'source_code_uri' => 'https://git.umaneti.net/fronde',
    'homepage_uri' => 'https://etienne.depar.is/fronde/',
    'funding_uri' => 'https://liberapay.com/milouse'
  }
  spec.files = [
    # Monkey patches / extensions
    'lib/ext/nil_time.rb',
    'lib/ext/r18n.rb',
    'lib/ext/time.rb',
    # Core files
    'lib/fronde/cli/commands.rb',
    'lib/fronde/cli/data/config.yml',
    'lib/fronde/cli/data/gitignore',
    'lib/fronde/cli/data/Rakefile',
    'lib/fronde/cli/data/zsh_completion',
    'lib/fronde/cli/helpers.rb',
    'lib/fronde/cli/opt_parse.rb',
    'lib/fronde/cli.rb',
    'lib/fronde/cli/throbber.rb',
    'lib/fronde/config/data/org-config.el',
    'lib/fronde/config/data/ox-fronde.el',
    'lib/fronde/config/helpers.rb',
    'lib/fronde/config/lisp.rb',
    'lib/fronde/config.rb',
    'lib/fronde/emacs.rb',
    'lib/fronde/index/atom_generator.rb',
    'lib/fronde/index/data/all_tags.org',
    'lib/fronde/index/data/template.org',
    'lib/fronde/index/data/template.xml',
    'lib/fronde/index/org_generator.rb',
    'lib/fronde/index.rb',
    'lib/fronde/org/file_extracter.rb',
    'lib/fronde/org/file.rb',
    'lib/fronde/org.rb',
    'lib/fronde/preview.rb',
    'lib/fronde/slug.rb',
    'lib/fronde/source/gemini.rb',
    'lib/fronde/source/html.rb',
    'lib/fronde/source.rb',
    'lib/fronde/templater.rb',
    'lib/fronde/version.rb',
    # Rake tasks
    'lib/tasks/cli.rake',
    'lib/tasks/org.rake',
    'lib/tasks/site.rake',
    'lib/tasks/sync.rake',
    'lib/tasks/tags.rake',
    # Translations
    'locales/en.yml',
    'locales/fr.yml',
    # Themes
    'lib/fronde/config/data/themes/umaneti/css/htmlize.css',
    'lib/fronde/config/data/themes/umaneti/css/style.css',
    'lib/fronde/config/data/themes/umaneti/img/bottom.png',
    'lib/fronde/config/data/themes/umaneti/img/content.png',
    'lib/fronde/config/data/themes/umaneti/img/tic.png',
    'lib/fronde/config/data/themes/umaneti/img/top.png',
    # Others
    'LICENSE'
  ]
  spec.executables = ['fronde']
  spec.homepage    = 'https://git.umaneti.net/fronde/about/'
  spec.license     = 'WTFPL'

  spec.required_ruby_version = '>= 2.7'
  spec.add_runtime_dependency 'base64', '~> 0.2'
  spec.add_runtime_dependency 'bigdecimal', '~> 3.1'
  spec.add_runtime_dependency 'liquid', '~> 5.5'
  spec.add_runtime_dependency 'nokogiri', '~> 1.16'
  spec.add_runtime_dependency 'r18n-core', '~> 5.0'
  spec.add_runtime_dependency 'rainbow', '~> 3.1'
  spec.add_runtime_dependency 'rake', '~> 13.2'
  spec.add_runtime_dependency 'webrick', '~> 1.8'

  spec.requirements << 'emacs'
end
