# frozen_string_literal: true

require 'neruda/index'
require 'neruda/utils'
require 'neruda/org_file'

namespace :site do
  desc 'Generates all index files'
  task :index do
    blog_path = Neruda::Config.settings['blog_path']
    next unless Dir.exist?("src/#{blog_path}")
    index = Neruda::Index.new
    verbose = Rake::FileUtilsExt.verbose_flag
    if verbose
      index.write_all
      next
    end
    build = Thread.new do
      index.write_all(false)
    end
    Neruda::Utils.throbber(build, 'Generating indexes:')
  end

  desc 'Customize HTML output for a given file'
  task :customize_output, :target do |_, args|
    if args[:target].nil?
      warn 'No source file given'
      next
    end
    require 'neruda/templater'
    warn "Customizing file #{args[:target]}" if Rake::FileUtilsExt.verbose_flag
    Neruda::Templater.customize_output(args[:target])
  end

  desc 'Convert all org files'
  task build: :index do
    build = Thread.new do
      Neruda::OrgFile.new(nil, verbose: Rake::FileUtilsExt.verbose_flag).publish
    end
    Neruda::Utils.throbber(build, 'Publishing:')
  end

  namespace :build do
    desc 'Convert one org file'
    task :one, :target do |_, args|
      if args[:target].nil?
        warn 'No source file given'
        next
      end
      Neruda::OrgFile.new(args[:target],
                          verbose: Rake::FileUtilsExt.verbose_flag).publish
    end
  end

  # :nocov:
  desc 'Start a test server'
  task :preview do
    require 'neruda/preview'
    Neruda.start_preview
  end
  # :nocov:
end
