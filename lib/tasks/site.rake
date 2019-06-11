# frozen_string_literal: true

require 'webrick'
require 'nokogiri'
require 'neruda/index'
require 'neruda/org_file'

BLOG_SLUG = Neruda::Config.settings['blog_slug']
PUBLIC_FOLDER = Neruda::Config.settings['public_folder']

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

def customize_output(org_file, file_name)
  templates = Neruda::Config.settings['templates']
  return if templates.nil? || templates.empty?
  dom = Nokogiri::HTML(File.open(file_name, 'r'))
  templates.each do |t|
    next unless t.has_key?('selector') && t.has_key?('content')
    next if t.has_key?('path') && !File.fnmatch?(t['path'], file_name)
    apply_template(dom, t['selector'], t['type'] || 'after',
                   org_file.format(t['content']))
  end
  dom.write_to(File.open(file_name, 'w'))
end

def copy_resources(org_file, src_dir, target_dir)
  return unless org_file.local_links.any?
  org_file.local_links.each do |l|
    lt_dir = File.dirname(l)
    if lt_dir != '.'
      lt_dir = "#{target_dir}/#{lt_dir}"
      mkdir_p(lt_dir) unless Dir.exist?(lt_dir)
    end
    cp "#{src_dir}/#{l}", "#{target_dir}/#{l}"
  end
end

def emacs_command(file_name)
  default_emacs = 'emacs -Q -q --batch -nw -l ./org-config.el'
  emacs_command = Neruda::Config.settings['emacs'] || default_emacs
  [emacs_command, "--eval '(find-file \"#{file_name}\")'",
   '-f org-html-export-to-html'].join(' ')
end

def compile_to_html(src, dest)
  sh emacs_command(src)
  mv src.ext('html'), dest
  org_file = Neruda::OrgFile.new(src)
  customize_output(org_file, dest)
  org_file
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
    target_dir = t.name.pathmap('%d')
    mkdir_p target_dir
    src = t.prerequisites[0]
    org_file = compile_to_html(src, t.name)
    copy_resources(org_file, src.pathmap('%d'), target_dir)
    print '.' unless Rake::FileUtilsExt.verbose_flag
  end

  desc 'Generates all index files'
  task :index do
    index = Neruda::Index.new(Dir.glob("src/#{BLOG_SLUG}/*/content.org"))
    index.entries.each do |k|
      slug = Neruda::Index.slug(k)
      src = "src/#{BLOG_SLUG}/#{slug}.org"
      File.open(src, 'w') do |f|
        f.puts index.to_s(k)
      end
      compile_to_html(src, "#{PUBLIC_FOLDER}/#{BLOG_SLUG}/#{slug}.html")
      slug = 'atom' if slug == 'index'
      atomdest = "#{PUBLIC_FOLDER}/#{BLOG_SLUG}/#{slug}.xml"
      File.open(atomdest, 'w') do |f|
        f.puts index.to_atom(k)
      end
      print '.' unless Rake::FileUtilsExt.verbose_flag
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
    print "#{R18n.t.pablo.config.author_name(config['author'])} "
    author = STDIN.gets.strip
    config['author'] = author if author != ''
    print "#{R18n.t.pablo.config.lang(config['lang'])} "
    lang = STDIN.gets.strip
    config['lang'] = lang if lang != ''
    Neruda::Config.save(config)
  end
end
