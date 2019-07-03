# frozen_string_literal: true

require 'webrick'
require 'nokogiri'
require 'digest/md5'
require 'neruda/index'
require 'neruda/utils'
require 'neruda/org_file'

# :nocov:
def run_webrick
  # Inspired by ruby un.rb library, which allows normally to start a
  # webrick server in one line: ruby -run -e httpd public_html -p 5000
  routes = Neruda::Config.settings['routes'] || {}
  options = { Port: Neruda::Config.settings['server_port'] || 5000,
              DocumentRoot: Neruda::Config.settings['public_folder'] }
  s = WEBrick::HTTPServer.new(options)
  routes.each do |req, dest|
    s.mount_proc req do |_, res|
      res.body = IO.read(dest)
    end
  end
  ['TERM', 'QUIT', 'INT'].each { |sig| trap(sig, proc { s.shutdown }) }
  s.start
end
# :nocov:

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

def customize_output(org_file, file_name)
  templates = Neruda::Config.settings['templates']
  return if templates.nil? || templates.empty?
  dom = open_dom(file_name)
  templates.each do |t|
    next unless t.has_key?('selector') && t.has_key?('content')
    if t.has_key?('path')
      check_path = [Neruda::Config.settings['public_folder'], t['path']].join
      next unless File.fnmatch?(check_path, file_name)
    end
    next if template_in_file?(dom.xpath('//head'), t['content'])
    apply_template(dom.css(t['selector']), t['type'] || 'after',
                   org_file.format(t['content']))
  end
  write_dom(file_name, dom)
end

def emacs_command(file_name = nil, verbose = true)
  default_emacs = 'emacs -Q -q --batch -nw -l ./org-config.el'
  emacs_command = [Neruda::Config.settings['emacs'] || default_emacs]
  emacs_command << '--eval \'(setq inhibit-message t)\'' unless verbose
  if file_name.nil?
    emacs_command << '--eval \'(org-publish "website")\''
  else
    file_name = File.expand_path(file_name)
    emacs_command.concat(["--eval '(find-file \"#{file_name}\")'",
                          '-f org-publish-current-file'])
  end
  emacs_command.join(' ')
end

namespace :site do
  desc 'Generates all index files'
  task :index do
    blog_path = Neruda::Config.settings['blog_path']
    next unless Dir.exist?("src/#{blog_path}")
    blog_pattern = Neruda::Config.settings['blog_pattern']
    index = Neruda::Index.new(Dir.glob("src/#{blog_pattern}"))
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
      sh emacs_command(nil, Rake::FileUtilsExt.verbose_flag)
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
      sh emacs_command(args[:target], Rake::FileUtilsExt.verbose_flag)
    end
  end

  # :nocov:
  desc 'Start a test server'
  task :preview do
    run_webrick
  end
  # :nocov:
end
