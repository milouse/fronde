# frozen_string_literal: true

require 'webrick'
require 'rainbow'
require 'nokogiri'
require 'neruda/index'
require 'neruda/org_file'

PUBLIC_FOLDER = Neruda::Config.settings['public_folder']
BLOG_PATH = Neruda::Config.settings['blog_path']

def run_webrick
  # Inspired by ruby un.rb library, which allows normally to start a
  # webrick server in one line: ruby -run -e httpd public_html -p 5000
  routes = Neruda::Config.settings['routes'] || {}
  options = { Port: Neruda::Config.settings['server_port'] || 5000,
              DocumentRoot: PUBLIC_FOLDER }
  s = WEBrick::HTTPServer.new(options)
  routes.each do |req, dest|
    s.mount_proc req do |_, res|
      displayfile = File.open(dest, 'r')
      res.body = displayfile.read
    end
  end
  ['TERM', 'QUIT', 'INT'].each { |sig| trap(sig, proc { s.shutdown }) }
  s.start
end

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

def customize_output(org_file, file_name)
  templates = Neruda::Config.settings['templates']
  return if templates.nil? || templates.empty?
  dom = Nokogiri::HTML(File.open(file_name, 'r'))
  templates.each do |t|
    next unless t.has_key?('selector') && t.has_key?('content')
    next if t.has_key?('path') && !File.fnmatch?(t['path'], file_name)
    apply_template(dom.css(t['selector']), t['type'] || 'after',
                   org_file.format(t['content']))
  end
  dom.write_to(File.open(file_name, 'w'))
end

def copy_resources(org_file, src_dir)
  return unless org_file.local_links.any?
  target_dir = PUBLIC_FOLDER + org_file.html_file.pathmap('%d')
  org_file.local_links.each do |l|
    lt_dir = File.dirname(l)
    if lt_dir != '.'
      lt_dir = "#{target_dir}/#{lt_dir}"
      mkdir_p(lt_dir) unless Dir.exist?(lt_dir)
    end
    cp "#{src_dir}/#{l}", "#{target_dir}/#{l}"
  end
end

def copy_theme_files
  theme = Neruda::Config.settings['theme']
  theme = 'default' if theme.nil?
  theme_path = nil
  [File.expand_path("../../themes/#{theme}", __dir__),
   "themes/#{theme}"].each do |t|
    next unless Dir.exist?(t)
    theme_path = t
    break
  end
  return false if theme_path.nil?
  rm_r "#{PUBLIC_FOLDER}/assets", secure: true, force: true
  mkdir_p "#{PUBLIC_FOLDER}/assets"
  cp_r "#{theme_path}/.", "#{PUBLIC_FOLDER}/assets"
  true
end

def emacs_command(file_name)
  default_emacs = 'emacs -Q -q --batch -nw -l ./org-config.el'
  emacs_command = Neruda::Config.settings['emacs'] || default_emacs
  [emacs_command, "--eval '(find-file \"#{file_name}\")'",
   '-f org-html-export-to-html'].join(' ')
end

def compile_to_html(src, dest)
  sh emacs_command(src)
  mkdir_p dest.pathmap('%d')
  mv src.ext('html'), dest
  org_file = Neruda::OrgFile.new(src)
  customize_output(org_file, dest)
  org_file
end

prerequisites_files = Neruda::OrgFile.expand_sources_list \
  Rake::FileList.new('src/**/*.org')
prerequisites_files.delete("#{PUBLIC_FOLDER}/#{BLOG_PATH}/index.html")

namespace :site do
  rule '.html' => ->(tt) { Neruda::OrgFile.source_for_target(tt) } do |t|
    src = t.prerequisites[0]
    org_file = compile_to_html(src, t.name)
    copy_resources(org_file, src.pathmap('%d'))
    print Rainbow('.').green unless Rake::FileUtilsExt.verbose_flag
  end

  desc 'Generates all index files'
  task :index do
    next unless Dir.exist?("src/#{BLOG_PATH}")
    mkdir_p ['src/tags', "#{PUBLIC_FOLDER}/feeds"]
    index = Neruda::Index.new(Dir.glob("src/#{BLOG_PATH}/*/content.org"))
    index.entries.each do |k|
      compile_to_html(index.write(k), index.index_public_path(k))
      index.write_atom(k)
      print Rainbow('.').blue unless Rake::FileUtilsExt.verbose_flag
    end
  end

  desc 'Publish chosen theme files'
  task :publish_theme do
    next unless copy_theme_files
    print Rainbow('.').magenta unless Rake::FileUtilsExt.verbose_flag
  end

  desc 'Convert all org files'
  task build: prerequisites_files.concat(['site:publish_theme'])

  desc 'Start a test server'
  task :preview do
    run_webrick
  end
end
