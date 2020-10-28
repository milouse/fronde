# frozen_string_literal: true

require 'open-uri'

# Fronde::Config is required by Fronde::Utils
require 'fronde/utils'

require 'rake/clean'

CLOBBER.push(
  'tmp/org.tar.gz', 'tmp/__last_org_version__',
  'org-config.el', '.dir-locals.el', 'htmlize.el'
)

namespace :org do
  desc 'Download last version of Org'
  file 'tmp/org.tar.gz' do
    verbose = Rake::FileUtilsExt.verbose_flag
    download = Thread.new do
      Thread.current[:org_version] = Fronde::Config.org_last_version
      Fronde::Utils.download_org
    end
    if verbose
      download.join
      warn "Org version #{download[:org_version]} has been downloaded"
    else
      Fronde::Utils.throbber(download, 'Downloading Org:')
    end
  end

  desc 'Compile Org'
  task compile: 'tmp/org.tar.gz' do |task|
    verbose = Rake::FileUtilsExt.verbose_flag
    org_version = "org-#{Fronde::Config.org_last_version}"
    next if Dir.exist?("#{org_version}/lisp")
    make = ['make', '-C', org_version]
    unless verbose
      make << '-s'
      make << 'EMACSQ="emacs -Q --eval \'(setq inhibit-message t)\'"'
    end
    build = Thread.new do
      sh "tar xzf #{task.prerequisites[0]}"
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
      Fronde::Utils.throbber(build, 'Installing Org:')
    end
  end

  file 'htmlize.el' do
    htmlize = URI(
      'https://raw.githubusercontent.com/hniksic/emacs-htmlize/master/htmlize.el'
    ).open.read
    IO.write 'htmlize.el', htmlize
  end

  file 'org-config.el' => 'htmlize.el' do
    Fronde::Config.write_org_lisp_config
  end

  file '.dir-locals.el' do
    Fronde::Config.write_dir_locals
  end

  desc 'Install Org'
  multitask install: ['org:compile', 'org-config.el', '.dir-locals.el'] do
    mkdir_p "#{Fronde::Config.settings['public_folder']}/assets"
    Fronde::Config.sources.each do |s|
      mkdir_p s['path'] unless Dir.exist? s['path']
    end
  end

  # The following task only run the clobber task (not provided by us)
  # and the org:install one, which is already tested. Thus, we can
  # safely remove it from coverage.
  # :nocov:
  desc 'Upgrade Org'
  task :upgrade do
    Rake::Task['clobber'].execute
    Rake::Task['org:install'].invoke
  end
  # :nocov:
end
