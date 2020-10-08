# frozen_string_literal: true

require 'open-uri'

# Neruda::Config is required by Neruda::Utils
require 'neruda/utils'

namespace :org do
  desc 'Download last version of org-mode'
  task :download do
    verbose = Rake::FileUtilsExt.verbose_flag
    download = Thread.new do
      Thread.current[:org_version] = Neruda::Config.org_last_version
      Neruda::Utils.download_org
    end
    if verbose
      download.join
      warn "org-#{download[:org_version]} has been downloaded"
    else
      Neruda::Utils.throbber(download, 'Downloading org mode:')
    end
  end

  desc 'Compile org-mode'
  task compile: ['org:download'] do
    verbose = Rake::FileUtilsExt.verbose_flag
    org_version = "org-#{Neruda::Config.org_last_version}"
    next if Dir.exist?("#{org_version}/lisp")
    make = ['make', '-C', org_version]
    unless verbose
      make << '-s'
      make << 'EMACSQ="emacs -Q --eval \'(setq inhibit-message t)\'"'
    end
    build = Thread.new do
      tarball = "tmp/#{org_version}.tar.gz"
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
    build = Thread.new do
      htmlize = URI(
        'https://raw.githubusercontent.com/hniksic/emacs-htmlize/master/htmlize.el'
      ).open.read
      IO.write 'htmlize.el', htmlize
    end
    if verbose
      build.join
      warn 'htmlize.el has been locally installed'
    else
      Neruda::Utils.throbber(build, 'Installing htmlize.el:')
    end
  end

  file 'org-config.el' => 'htmlize.el' do
    Neruda::Config.write_org_lisp_config
  end

  file '.dir-locals.el' do
    Neruda::Config.write_dir_locals
  end

  desc 'Install org'
  task install: ['org:compile', 'org-config.el', '.dir-locals.el'] do
    mkdir_p "#{Neruda::Config.settings['public_folder']}/assets"
    Neruda::Config.sources.each do |s|
      mkdir_p s['path'] unless Dir.exist? s['path']
    end
  end
end
