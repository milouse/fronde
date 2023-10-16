# frozen_string_literal: true

require 'open-uri'

require_relative '../fronde/config'
require_relative '../fronde/cli/throbber'

require 'rake/clean'

CLOBBER.push(
  'var/tmp/org.tar.gz', 'var/tmp/last_org_version',
  'var/lib/org-config.el', '.dir-locals.el', '.gitignore',
  'lib/htmlize.el'
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
      Thread.current[:org_version] = Fronde::Org.download
    end
    if verbose
      download.join
      warn R18n.t.fronde.tasks.org.downloaded(version: download[:org_version])
    else
      Fronde::CLI::Throbber.run(download, R18n.t.fronde.tasks.org.downloading)
    end
  end

  desc 'Compile Org'
  multitask compile: ['var/tmp/org.tar.gz', 'lib'] do |task|
    org_version = Fronde::Org.last_version
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
      warn R18n.t.fronde.tasks.org.installed(version: org_version)
    else
      Fronde::CLI::Throbber.run(build, R18n.t.fronde.tasks.org.installing)
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

  file '.gitignore' do
    upstream = File.expand_path(
      '../fronde/cli/data/gitignore', __dir__
    )
    cp upstream, '.gitignore'
  end

  desc 'Install Org'
  multitask install: ['org:compile', '.gitignore'] do
    # I need a fully installed org mode to correctly generate the lisp
    # config
    Rake::Task['.dir-locals.el'].invoke
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
    next if Fronde::Org.current_version == Fronde::Org.last_version(force: true)

    Rake::Task['clobber'].execute
    Rake::Task['org:install'].invoke
  end
  # :nocov:
end
