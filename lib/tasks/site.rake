# frozen_string_literal: true

require_relative '../fronde/emacs'
require_relative '../fronde/index'
require_relative '../fronde/templater'
require_relative '../fronde/cli/throbber'

namespace :site do
  desc 'Build all your projects'
  task :build, [:force?] => ['var/lib/org-config.el'] do |_, args|
    args.with_defaults(force?: false)

    FileUtils.rm_f 'var/tmp/keywords'
    build_index = Thread.new do
      all_index = Fronde::Index.all_blog_index
      offset = 0
      all_index.each do |index|
        index.write_all_org(verbose:)
        offset = File.write('var/tmp/keywords', index.emacs_keywords, offset)
      end
      Thread.current[:all_indexes] = all_index
    end
    if verbose
      build_index.join
    else
      Fronde::CLI::Throbber.run(
        build_index, I18n.t('fronde.tasks.site.generating_indexes')
      )
    end
    all_indexes = build_index[:all_indexes]

    build_html = Thread.new do
      rm_r 'var/tmp/timestamps', force: true if args[:force?]
      Fronde::Emacs.new(verbose:).publish
    end
    Fronde::CLI::Throbber.run(build_html, I18n.t('fronde.tasks.site.building'))

    if all_indexes.any?
      if verbose
        all_indexes.each(&:write_all_feeds)
      else
        publish_feed = Thread.new do
          all_indexes.each do |index|
            index.write_all_feeds(verbose: false)
          end
        end
        Fronde::CLI::Throbber.run(
          publish_feed, I18n.t('fronde.tasks.site.publishing_feeds')
        )
      end
    end

    next unless Fronde::CONFIG.sources.any? { |source| source.type == 'html' }

    customize_html = Thread.new do
      pubfolder = Fronde::CONFIG.get('html_public_folder')
      Dir.glob("#{pubfolder}/**/*.html").each do |f|
        Fronde::Templater.customize_output(f)
      end
    end
    Fronde::CLI::Throbber.run(
      customize_html, I18n.t('fronde.tasks.site.customizing')
    )
    # :nocov:
  rescue RuntimeError, Interrupt
    warn I18n.t('fronde.tasks.site.aborting')
    next
    # :nocov:
  end

  desc 'Cleanup orphaned published files'
  task :clean do
    pubfolder = Fronde::CONFIG.get('html_public_folder')
    Dir.glob("#{pubfolder}/**/*.html").each do |file_name|
      source = Fronde::Org::File.new(file_name)

      # Return if an org file has been found for this published file
      next unless source.file == file_name

      print I18n.t('fronde.tasks.site.remove_orphan_file')
      action = $stdin.gets.strip.downcase
      next unless action == 'y'

      rm file_name
    end
  end

  desc 'Start a test server'
  task :preview do
    require_relative '../fronde/preview'
    Fronde::Preview.start
  end
end
