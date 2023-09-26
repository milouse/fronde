# frozen_string_literal: true

require_relative '../fronde/emacs'
require_relative '../fronde/index'
require_relative '../fronde/org_file'
require_relative '../fronde/templater'
require_relative '../fronde/cli/throbber'

namespace :site do
  desc 'Generates all index files'
  task :index do
    index = Fronde::Index.new
    if verbose
      index.write_all
      next
    end
    build = Thread.new do
      index.write_all(verbose: false)
    end
    Fronde::CLI::Throbber.run(build, 'Generating indexes:')
  end

  desc 'Convert and customize all org files'
  task :build, [:force?] => ['var/lib/org-config.el', :index] do |_, args|
    args.with_defaults(:force? => false)
    build_html = Thread.new do
      rm_r 'var/tmp/timestamps', force: true if args[:force?]
      Fronde::Emacs.new(verbose: verbose).publish
    end
    begin
      Fronde::CLI::Throbber.run(build_html, 'Building:')
    # :nocov:
    rescue RuntimeError
      warn 'Aborting'
      next
    end
    # :nocov:
    next unless Fronde::CONFIG.sources.any? { |source| source.type == 'html' }

    customize_html = Thread.new do
      pubfolder = Fronde::CONFIG.get('html_public_folder')
      Dir["#{pubfolder}/**/*.html"].each do |f|
        Fronde::Templater.customize_output(f)
      end
    end
    Fronde::CLI::Throbber.run(customize_html, 'Customizing:')
  end

  desc 'Start a test server'
  task :preview do
    require_relative '../fronde/preview'
    Fronde.start_preview
  end
end
