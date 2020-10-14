# frozen_string_literal: true

require 'neruda/emacs'
require 'neruda/index'
require 'neruda/utils'
require 'neruda/org_file'
require 'neruda/templater'

namespace :site do
  desc 'Generates all index files'
  task :index do
    index = Neruda::Index.new
    verbose = Rake::FileUtilsExt.verbose_flag
    if verbose
      index.write_all
      next
    end
    build = Thread.new do
      index.write_all(verbose: false)
    end
    Neruda::Utils.throbber(build, 'Generating indexes:')
    next if index.empty?
    Neruda::Config.write_org_lisp_config(with_tags: true)
  end

  desc 'Convert all org files'
  task :build, [:force?] => [:index] do |_, args|
    args.with_defaults(:force? => false)
    build_html = Thread.new do
      rm_r 'tmp/timestamps', force: true if args[:force?]
      Neruda::Emacs.new(verbose: Rake::FileUtilsExt.verbose_flag).publish
    end
    begin
      Neruda::Utils.throbber(build_html, 'Building:')
    # :nocov:
    rescue RuntimeError
      warn 'Aborting'
      next
    end
    # :nocov:
    customize_html = Thread.new do
      pubfolder = Neruda::Config.settings['public_folder']
      Dir["#{pubfolder}/**/*.html"].each do |f|
        Neruda::Templater.customize_output(f)
      end
    end
    Neruda::Utils.throbber(customize_html, 'Customizing:')
  end

  namespace :build do
    desc 'Convert one org file'
    task :one, :source do |_, args|
      if args[:source].nil?
        warn 'No source file given'
        next
      end
      verbose = Rake::FileUtilsExt.verbose_flag
      project = Neruda::OrgFile.project_for_source(args[:source])
      if project.nil?
        warn "No project found for #{args['source']}"
        next
      end
      build_html = Thread.new do
        o = Neruda::OrgFile.new(
          args[:source], project: project, verbose: verbose
        )
        Thread.current[:org_file] = o
        o.publish
      end
      begin
        Neruda::Utils.throbber(build_html, 'Building:')
      rescue RuntimeError
        warn 'Aborting'
        next
      end
      target = Neruda::OrgFile.target_for_source(args[:source], project)
      warn "Customizing file #{target}" if verbose
      Neruda::Templater.customize_output(target, build_html[:org_file])
    end
  end

  desc 'Start a test server'
  task :preview do
    require 'neruda/preview'
    Neruda.start_preview
  end
end
