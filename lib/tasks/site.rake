# frozen_string_literal: true

require 'neruda/index'
require 'neruda/utils'
require 'neruda/org_file'
require 'neruda/templater'

namespace :site do
  desc 'Generates all index files'
  task :index do
    blog_path = Neruda::Config.settings['blog_path']
    next if blog_path.nil?
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

  desc 'Convert all org files'
  task build: :index do
    build_html = Thread.new do
      Neruda::OrgFile.new(nil, verbose: Rake::FileUtilsExt.verbose_flag).publish
    end
    Neruda::Utils.throbber(build_html, 'Publishing:')
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
      o = Neruda::OrgFile.new(args[:source], verbose: verbose)
      o.publish
      target = Neruda::OrgFile.target_for_source(args[:source])
      warn "Customizing file #{target}" if verbose
      Neruda::Templater.customize_output(target, o)
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

namespace :tags do
  desc 'List all tags by name'
  task :name do
    blog_path = Neruda::Config.settings['blog_path']
    next if blog_path.nil?
    next unless Dir.exist?("src/#{blog_path}")
    index = Neruda::Index.new
    puts index.sort_by(:name).join("\n")
  end

  desc 'List all tags by weight'
  task :weight do
    blog_path = Neruda::Config.settings['blog_path']
    next if blog_path.nil?
    next unless Dir.exist?("src/#{blog_path}")
    index = Neruda::Index.new
    puts index.sort_by(:weight).join("\n")
  end
end
