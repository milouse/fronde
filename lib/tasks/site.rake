# frozen_string_literal: true

require 'nokogiri'
require 'digest/md5'
require 'neruda/index'
require 'neruda/utils'
require 'neruda/org_file'

def apply_template(elem, position, content)
  elem.each do |e|
    if position == 'before'
      e.add_previous_sibling content
    elsif position == 'replace'
      e.replace content
    else
      e.add_next_sibling content
    end
  end
end

def template_in_file?(head, content)
  digest = Digest::MD5.hexdigest(content)
  check = " Neruda Template: #{digest} "
  head.children.to_a.filter(&:comment?).each do |c|
    return true if c.text == check
  end
  head.first.prepend_child("<!--#{check}-->\n")
  false
end

def open_dom(file_name)
  file = File.new file_name, 'r'
  dom = Nokogiri::HTML file
  file.close
  dom
end

def write_dom(file_name, dom)
  file = File.new file_name, 'w'
  dom.write_to file
  file.close
end

def check_path(file_name, pathes)
  pub_folder = Neruda::Config.settings['public_folder']
  if pathes.is_a?(Array)
    pathes.each do |tp|
      return true if File.fnmatch?("#{pub_folder}#{tp}",
                                   file_name, File::FNM_DOTMATCH)
    end
    return false
  end
  File.fnmatch?("#{pub_folder}#{pathes}",
                file_name, File::FNM_DOTMATCH)
end

def customize_output(org_file, file_name)
  templates = Neruda::Config.settings['templates']
  return if templates.nil? || templates.empty?
  dom = open_dom(file_name)
  templates.each do |t|
    next unless t.has_key?('selector') && t.has_key?('content')
    next if t.has_key?('path') && !check_path(file_name, t['path'])
    next if template_in_file?(dom.xpath('//head'), t['content'])
    apply_template(dom.css(t['selector']), t['type'] || 'after',
                   org_file.format(t['content']))
  end
  write_dom(file_name, dom)
end

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
    source = Neruda::OrgFile.source_for_target(args[:target])
    if Rake::FileUtilsExt.verbose_flag
      warn "Customizing file #{args[:target]} from #{source}"
    end
    customize_output(Neruda::OrgFile.new(source), args[:target])
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
