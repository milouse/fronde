# frozen_string_literal: true

# Neruda::Config is required by Neruda::Utils
require 'neruda/utils'

namespace :org do
  desc 'Download last version of org-mode'
  task :download do
    verbose = Rake::FileUtilsExt.verbose_flag
    next if Neruda::Config.org_last_version.nil?
    org_version = "org-#{Neruda::Config.org_last_version}"
    tarball = "#{org_version}.tar.gz"
    next if File.exist?(tarball)
    curl = ['curl', '--progress-bar', '-O',
            "https://orgmode.org/#{tarball}"]
    curl[1] = '-s' unless verbose
    download = Thread.new do
      sh curl.join(' ')
    end
    if verbose
      download.join
      warn "#{org_version} has been downloaded"
    else
      Neruda::Utils.throbber(download, 'Downloading org mode:')
    end
  end

  desc 'Compile org-mode'
  task compile: ['org:download'] do
    verbose = Rake::FileUtilsExt.verbose_flag
    org_version = "org-#{Neruda::Config.org_last_version}"
    next if Dir.exist?("#{org_version}/lisp")
    tarball = "#{org_version}.tar.gz"
    next unless File.exist?(tarball)
    make = ['make', '-C', org_version]
    unless verbose
      make << '-s'
      make << 'EMACSQ="emacs -Q --eval \'(setq inhibit-message t)\'"'
    end
    build = Thread.new do
      sh "tar xzf #{tarball}"
      File.unlink tarball
      sh((make + ['compile']).join(' '))
      sh((make + ['autoloads']).join(' '))
      Dir.glob('org-[0-9.]*').each do |ov|
        next if ov == org_version
        rm_r ov
      end
    end
    if verbose
      build.join
      warn "#{org_version} has been locally installed"
    else
      Neruda::Utils.throbber(build, 'Installing org mode:')
    end
  end

  file 'htmlize.el' do
    verbose = Rake::FileUtilsExt.verbose_flag
    curl = ['curl', '--progress-bar', '-O',
            'https://raw.githubusercontent.com/hniksic/emacs-htmlize/master/htmlize.el']
    curl[1] = '-s' unless verbose
    build = Thread.new { sh curl.join(' ') }
    if verbose
      build.join
      warn 'htmlize.el has been locally installed'
    else
      Neruda::Utils.throbber(build, 'Installing htmlize.el:')
    end
  end

  file 'org-config.el' => 'htmlize.el' do
    next if Neruda::Config.org_last_version.nil?
    Neruda::Config.write_org_lisp_config
  end

  file '.dir-locals.el' do
    Neruda::Config.write_dir_locals
  end

  desc 'Install org'
  task install: ['org:compile', 'org-config.el', '.dir-locals.el'] do
    mkdir_p "#{Neruda::Config.settings['public_folder']}/assets"
    mkdir_p 'src'
  end
end
