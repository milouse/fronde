# frozen_string_literal: true

require 'open-uri'

require_relative '../fronde/config'
require_relative '../fronde/cli/helpers'
require_relative '../fronde/cli/throbber'

require 'rake/clean'

CLOBBER.push(
  'var/tmp/org.tar.gz', 'var/tmp/last_org_version',
  'var/lib/org-config.el', '.dir-locals.el', 'lib/htmlize.el'
)

def make_org_cmd(org_dir, target)
  make = ['make', '-C', org_dir, target]
  return make.join(' ') if verbose
  make.insert(3, '-s')
  make << 'EMACSQ="emacs -Q --eval \'(setq inhibit-message t)\'"'
  make.join(' ')
end

namespace :org do
  directory 'var/tmp'

  desc 'Download last version of Org'
  file 'var/tmp/org.tar.gz' => 'var/tmp' do
    download = Thread.new do
      Thread.current[:org_version] = Fronde::CONFIG.org_last_version
      Fronde::CLI::Helpers.download_org
    end
    if verbose
      download.join
      warn "Org version #{download[:org_version]} has been downloaded"
    else
      Fronde::CLI::Throbber.run(download, 'Downloading Org:')
    end
  end

  desc 'Compile Org'
  task compile: 'var/tmp/org.tar.gz' do |task|
    org_version = Fronde::CONFIG.org_last_version
    org_dir = "lib/org-#{org_version}"
    next if Dir.exist?("#{org_dir}/lisp")
    build = Thread.new do
      sh "tar -C lib -xzf #{task.prerequisites[0]}"
      mv "lib/org-mode-release_#{org_version}", org_dir
      # Fix a weird unknown package version
      File.write("#{org_dir}/mk/version.mk", "ORGVERSION ?= #{org_version}")
      sh make_org_cmd(org_dir, 'compile')
      sh make_org_cmd(org_dir, 'autoloads')
      Dir.glob('lib/org-[0-9.]*').each do |ov|
        next if ov == org_dir
        rm_r ov
      end
    end
    if verbose
      build.join
      warn "#{org_version} has been locally installed"
    else
      Fronde::CLI::Throbber.run(build, 'Installing Org:')
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
      'https://git.umaneti.net/ox-gmi/plain/ox-gmi.el'
    ).open.read
    File.write 'lib/ox-gmi.el', ox_gmi
  end

  file 'var/lib/org-config.el' => ['lib/htmlize.el', 'lib/ox-gmi.el'] do
    Fronde::CONFIG.write_org_lisp_config
  end

  file '.dir-locals.el' => 'var/lib/org-config.el' do
    Fronde::Config::Helpers.write_dir_locals
  end

  desc 'Install Org'
  multitask install: ['org:compile', '.dir-locals.el'] do
    mkdir_p "#{Fronde::CONFIG.get('html_public_folder')}/assets"
    Fronde::CONFIG.sources.each do |s|
      mkdir_p s['path']
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
