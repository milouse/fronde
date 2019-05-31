# frozen_string_literal: true

require 'webrick'
require 'nokogiri'
require 'neruda/index'
require 'neruda/org_file'

BLOG_SLUG = Neruda::Config.settings['blog_slug'] || 'blog'
PUBLIC_FOLDER = Neruda::Config.settings['public_folder'] || 'public_html'

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

def apply_template(dom, selector, position, content)
  elem = dom.css(selector)
  elem.each do |e|
    if position == 'before'
      e.add_previous_sibling content
    else
      e.add_next_sibling content
    end
  end
end

def customize_output(file_name)
  return nil unless File.exist?(file_name)
  templates = Neruda::Config.settings['templates']
  return nil if templates.nil? || templates.empty?
  org_file = Neruda::OrgFile.new(source_for_target(file_name))
  dom = Nokogiri::HTML(File.open(file_name, 'r'))
  templates.each do |t|
    next unless t.has_key?('selector') && t.has_key?('content')
    next if t.has_key?('path') && !File.fnmatch?(t['path'], file_name)
    apply_template(dom, t['selector'], t['type'] || 'after',
                   org_file.format(t['content']))
  end
  dom.write_to(File.open(file_name, 'w'))
end

def emacs_command(file_name)
  default_emacs = 'emacs -Q -q --batch -nw -l ./org-config.el'
  emacs_command = Neruda::Config.settings['emacs'] || default_emacs
  [emacs_command, "--eval '(find-file \"#{file_name}\")'",
   '-f org-html-export-to-html'].join(' ')
end

def source_for_target(file_name)
  src = file_name.ext('org')
  if /\/emacs\.d\//.match?(src)
    return File.expand_path(src.sub(/^#{PUBLIC_FOLDER}\//, '~/.'))
  end
  src.sub!(/^#{PUBLIC_FOLDER}\//, 'src/')
  return src if ['src/index.org', "src/#{BLOG_SLUG}/index.org"].include?(src)
  src.sub(/\/index\.org$/, '/content.org')
end

def target_for_source(file_name)
  src = file_name.ext('html')
  if /\/\.emacs\.d\//.match?(src)
    return "#{PUBLIC_FOLDER}/emacs.d/#{File.basename(src)}"
  end
  src.sub(/^src\//, "#{PUBLIC_FOLDER}/").sub(/\/content\.html$/, '/index.html')
end

prerequisites_files = Rake::FileList[
  'src/**/*.org',
  File.expand_path('~/.emacs.d/*.org')
].exclude(/comments\.org$/).map do |f|
  target_for_source f
end

namespace :site do
  rule '.html' => ->(target) { source_for_target(target) } do |t|
    src = t.prerequisites[0]
    sh emacs_command(src)
    target_dir = t.name.pathmap('%d')
    mkdir_p target_dir
    src_media = "#{src.pathmap('%d')}/media"
    cp_r(src_media, target_dir) if Dir.exist?(src_media)
    mv src.ext('html'), t.name
    customize_output(t.name)
  end

  desc 'Generates all index files'
  task :index do
    index = Neruda::Index.new(Dir.glob("src/#{BLOG_SLUG}/*/content.org"))
    index.entries.each do |k|
      slug = Neruda::Index.slug(k)
      File.open("src/#{BLOG_SLUG}/#{slug}.org", 'w') do |f|
        f.puts index.to_s(k)
      end
      Rake::Task["#{PUBLIC_FOLDER}/#{BLOG_SLUG}/#{slug}.html"].invoke
    end
  end

  desc 'Convert all org files'
  task build: prerequisites_files

  desc 'Start a test server'
  task :preview do
    run_webrick
  end

  desc 'Answer common questions to configure your website'
  task :config do
    config = Neruda::Config.settings.merge
    changed = false
    print "#{R18n.t.pablo.config.author_name(config['author'])} "
    author = STDIN.gets.strip
    if author != ''
      config['author'] = author
      changed = true
    end
    old_lang = config['lang'] || 'en'
    print "#{R18n.t.pablo.config.lang(old_lang)} "
    lang = STDIN.gets.strip
    if lang == '' && old_lang != config['lang']
      config['lang'] = old_lang
      changed = true
    elsif lang != ''
      config['lang'] = lang
      changed = true
    end
    Neruda::Config.save(config) if changed
  end
end
