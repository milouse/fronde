# frozen_string_literal: true

# Neruda::Config is required by Neruda::Utils
require 'neruda/utils'
require 'neruda/org_config_el'

namespace :org do
  desc 'Download last version of org-mode'
  task :download do
    verbose = Rake::FileUtilsExt.verbose_flag
    org_version = Neruda::Config.org_last_version
    next if Neruda::Config.org_last_version.nil?
    next if Dir.exist?("org-#{org_version}/lisp")
    tarball = "org-#{org_version}.tar.gz"
    curl = ['curl', '--progress-bar', '-O',
            "https://orgmode.org/#{tarball}"]
    make = ['make', '-C', "org-#{org_version}"]
    unless verbose
      curl[1] = '-s'
      make << '-s'
      make << 'EMACSQ="emacs -Q --eval \'(setq inhibit-message t)\'"'
    end
    build = Thread.new do
      sh curl.join(' ')
      sh "tar xzf #{tarball}"
      File.unlink tarball
      sh((make + ['compile']).join(' '))
      sh((make + ['autoloads']).join(' '))
    end
    if verbose
      build.join
      warn "org-mode #{org_version} has been locally installed"
    else
      Neruda::Utils.throbber(build, 'Installing org mode:')
    end
  end

  file 'org-config.el' do
    next if Neruda::Config.org_last_version.nil?
    IO.write('org-config.el', Neruda::OrgConfigEl.org_config)
  end

  desc 'Install org'
  task install: ['org:download', 'org-config.el'] do
    mkdir_p "#{Neruda::Config.settings['public_folder']}/assets"
    mkdir_p 'src'
  end
end
