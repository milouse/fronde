# frozen_string_literal: true

require 'open-uri'

require_relative '../fronde/config'
require_relative '../fronde/cli/throbber'

require 'rake/clean'

CLOBBER.push(
  'var/lib/org-config.el', 'lib/htmlize.el'
)

HTMLIZE_TAG = 'release/1.58'
OX_GMI_TAG = 'v0.2'

namespace :org do
  directory 'var/tmp'

  desc 'Download last version of Org'
  file 'var/tmp/org.tar.gz' => 'var/tmp' do
    # Weird Rake issue, still executing the task even if the file exists
    next if File.exist? 'var/tmp/org.tar.gz'

    download = Thread.new do
      version = Fronde::Org.download
      puts I18n.t('fronde.tasks.org.downloaded', version:) if verbose
    end
    Fronde::CLI::Throbber.run(
      download, I18n.t('fronde.tasks.org.downloading'), verbose
    )
  rescue RuntimeError, Interrupt
    warn I18n.t('fronde.tasks.org.no_download') if verbose
  end

  desc 'Compile Org'
  multitask compile: ['var/tmp/org.tar.gz', 'lib'] do |task|
    # No need to force fetch last version as it is only interesting as
    # part of the upgrade task
    version = Fronde::Org.last_version

    org_dir = "lib/org-#{version}"
    next if Dir.exist?("#{org_dir}/lisp")

    build = Thread.new do
      Fronde::Org.compile(task.prerequisites[0], version, org_dir, verbose:)
      Dir.glob('lib/org-[0-9.]*').each { rm_r _1 unless _1 == org_dir }
      puts I18n.t('fronde.tasks.org.installed', version:) if verbose
    end
    Fronde::CLI::Throbber.run(
      build, I18n.t('fronde.tasks.org.installing'), verbose
    )
  rescue RuntimeError, Interrupt
    next
  end

  directory 'lib'

  file 'lib/htmlize.el' => 'lib' do
    htmlize = URI(
      "https://raw.githubusercontent.com/hniksic/emacs-htmlize/refs/tags/#{HTMLIZE_TAG}/htmlize.el"
    ).open.read
    File.write 'lib/htmlize.el', htmlize
  end

  file 'lib/ox-gmi.el' => 'lib' do
    ox_gmi = URI(
      "https://git.umaneti.net/ox-gmi/plain/ox-gmi.el?h=#{OX_GMI_TAG}"
    ).open.read
    File.write 'lib/ox-gmi.el', ox_gmi
  end

  file 'var/lib/org-config.el' => ['lib/htmlize.el', 'lib/ox-gmi.el'] do
    Fronde::CONFIG.write_org_lisp_config
  end

  file '.gitignore' do
    next if File.exist? '.gitignore'

    upstream = File.expand_path(
      '../fronde/cli/data/gitignore', __dir__
    )
    cp upstream, '.gitignore'
  end

  desc 'Install Org'
  multitask install: ['org:compile', '.gitignore'] do
    # lib/htmlize.el and lib/ox-gmi.el cannot be generated in parallel
    # of org:compilation, as it will leads to a weird SSL error. Thus
    # finishing file generation "manually" here.
    Rake::Task['var/lib/org-config.el'].invoke
    sources = Fronde::CONFIG.sources
    sources.each { mkdir_p _1['path'] }

    outputs = sources.map { _1['type'] }.uniq
    if outputs.include?('html')
      mkdir_p "#{Fronde::CONFIG.get('html_public_folder')}/assets"
    end
    if outputs.include?('gemini')
      mkdir_p Fronde::CONFIG.get('gemini_public_folder')
    end
  end

  desc 'Upgrade Org'
  task :upgrade do
    Rake::Task['clobber'].execute
    if File.exist? 'var/tmp/org.tar.gz'
      # Cleanup cached tarball only if a new version is available.
      # Also cached the new remote org version in the same time.
      org_version = Fronde::Org.current_version
      begin
        last_version = Fronde::Org.last_version(force: true)
      rescue RuntimeError
        last_version = org_version
      end
      File.unlink 'var/tmp/org.tar.gz' unless org_version == last_version
    end
    Rake::Task['org:install'].invoke
  end
end
