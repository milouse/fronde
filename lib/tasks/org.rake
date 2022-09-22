# frozen_string_literal: true

require 'open-uri'

# Fronde::Config is required by Fronde::Utils
require 'fronde/utils'

require 'rake/clean'

CLOBBER.push(
  'var/tmp/org.tar.gz', 'var/tmp/last_org_version',
  'var/lib/org-config.el', '.dir-locals.el', 'lib/htmlize.el'
)

namespace :org do
  directory 'var/tmp'

  desc 'Download last version of Org'
  file 'var/tmp/org.tar.gz' => 'var/tmp' do
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
  task compile: 'var/tmp/org.tar.gz' do |task|
    org_version = Fronde::Config.org_last_version
    org_dir = "lib/org-#{org_version}"
    next if Dir.exist?("#{org_dir}/lisp")
    make = ['make', '-C', org_dir]
    unless verbose
      make << '-s'
      make << 'EMACSQ="emacs -Q --eval \'(setq inhibit-message t)\'"'
    end
    build = Thread.new do
      sh "tar -C lib -xzf #{task.prerequisites[0]}"
      mv "lib/org-mode-release_#{org_version}", org_dir
      sh((make + ['compile']).join(' '))
      sh((make + ['autoloads']).join(' '))
      Dir.glob('lib/org-[0-9.]*').each do |ov|
        next if ov == org_dir
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

  directory 'lib'

  file 'lib/htmlize.el' => 'lib' do
    htmlize = URI(
      'https://raw.githubusercontent.com/hniksic/emacs-htmlize/master/htmlize.el'
    ).open.read
    File.write 'lib/htmlize.el', htmlize
  end

  file 'lib/ox-gmi.el' => 'lib' do
    ox_gmi = URI(
      'https://git.umaneti.net/ox-gmi.el/plain/ox-gmi.el'
    ).open.read
    File.write 'lib/ox-gmi.el', ox_gmi
  end

  file 'var/lib/org-config.el' => ['lib/htmlize.el', 'lib/ox-gmi.el'] do
    Fronde::Config.write_org_lisp_config
  end

  file '.dir-locals.el' => 'var/lib/org-config.el' do
    Fronde::Config.write_dir_locals
  end

  desc 'Install Org'
  multitask install: ['org:compile', '.dir-locals.el'] do
    mkdir_p "#{Fronde::Config.get('public_folder')}/assets"
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
