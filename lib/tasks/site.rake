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

  desc 'Convert and customize all org files'
  task :build, [:force?] => ['org-config.el', :index] do |_, args|
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

  desc 'Start a test server'
  task :preview do
    require 'neruda/preview'
    Neruda.start_preview
  end
end
