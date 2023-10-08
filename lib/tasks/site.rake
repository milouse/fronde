# frozen_string_literal: true

require_relative '../fronde/emacs'
require_relative '../fronde/index'
require_relative '../fronde/templater'
require_relative '../fronde/cli/throbber'

namespace :site do
  desc 'Generates all index files'
  task :index, :build? do |_, args|
    index = Fronde::Index.new
    if verbose
      index.write_all
      next
    end
    build = Thread.new do
      index.write_all(verbose: false)
    end
    Fronde::CLI::Throbber.run(
      build, R18n.t.fronde.tasks.site.generating_indexes
    )
    next unless args[:build?]

    blog_homes = index.blog_homes
    next unless blog_homes.any?

    build_html = Thread.new do
      rm_r 'var/tmp/timestamps/tags.cache', force: true
      emacs = Fronde::Emacs.new(verbose: verbose)
      emacs.publish('tags')
      blog_homes.each_key do |home|
        emacs.publish_file home
      end
    end
    begin
      Fronde::CLI::Throbber.run(
        build_html, R18n.t.fronde.tasks.site.building_indexes
      )
    # :nocov:
    rescue RuntimeError
      warn R18n.t.fronde.tasks.site.aborting
      next
    end
    # :nocov:
  end

  desc 'Convert and customize all org files'
  task :build, [:force?] => ['var/lib/org-config.el'] do |_, args|
    args.with_defaults(:force? => false)
    build_html = Thread.new do
      rm_r 'var/tmp/timestamps', force: true if args[:force?]
      Fronde::Emacs.new(verbose: verbose).publish
    end
    begin
      Fronde::CLI::Throbber.run(build_html, R18n.t.fronde.tasks.site.building)
    # :nocov:
    rescue RuntimeError
      warn R18n.t.fronde.tasks.site.aborting
      next
    end
    # :nocov:
    Rake::Task['site:index'].invoke('build')
    next unless Fronde::CONFIG.sources.any? { |source| source.type == 'html' }

    customize_html = Thread.new do
      pubfolder = File.expand_path Fronde::CONFIG.get('html_public_folder')
      Dir["#{pubfolder}/**/*.html"].each do |f|
        Fronde::Templater.customize_output(f)
      end
    end
    Fronde::CLI::Throbber.run(
      customize_html, R18n.t.fronde.tasks.site.customizing
    )
  end

  desc 'Cleanup orphaned published files'
  task :clean do
    pubfolder = Fronde::CONFIG.get('html_public_folder')
    Dir["#{pubfolder}/**/*.html"].each do |file_name|
      source = Fronde::Org::File.new(file_name)

      # Return if an org file has been found for this published file
      next unless source.file == file_name

      print R18n.t.fronde.tasks.site.remove_orphan_file
      action = $stdin.gets.strip.downcase
      next unless action == 'y'

      rm file_name
    end
  end

  desc 'Start a test server'
  task :preview do
    require_relative '../fronde/preview'
    Fronde.start_preview
  end
end
