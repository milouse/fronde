# frozen_string_literal: true

require 'fronde/emacs'
require 'fronde/index'
require 'fronde/utils'
require 'fronde/org_file'
require 'fronde/templater'

namespace :site do
  desc 'Generates all index files'
  task :index do
    index = Fronde::Index.new
    verbose = Rake::FileUtilsExt.verbose_flag
    if verbose
      index.write_all
      next
    end
    build = Thread.new do
      index.write_all(verbose: false)
    end
    Fronde::Utils.throbber(build, 'Generating indexes:')
    next if index.empty?
    Fronde::Config.write_org_lisp_config(with_tags: true)
  end

  desc 'Convert and customize all org files'
  task :build, [:force?] => ['var/lib/org-config.el', :index] do |_, args|
    args.with_defaults(:force? => false)
    build_html = Thread.new do
      rm_r 'var/tmp/timestamps', force: true if args[:force?]
      Fronde::Emacs.new(verbose: Rake::FileUtilsExt.verbose_flag).publish
    end
    begin
      Fronde::Utils.throbber(build_html, 'Building:')
    # :nocov:
    rescue RuntimeError
      warn 'Aborting'
      next
    end
    # :nocov:
    customize_html = Thread.new do
      pubfolder = Fronde::Config.get('public_folder')
      Dir["#{pubfolder}/**/*.html"].each do |f|
        Fronde::Templater.customize_output(f)
      end
    end
    Fronde::Utils.throbber(customize_html, 'Customizing:')
  end

  desc 'Start a test server'
  task :preview do
    require 'fronde/preview'
    Fronde.start_preview
  end
end
