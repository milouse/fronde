# frozen_string_literal: true

require 'thread'
require 'webrick'
require 'rainbow'
require 'nokogiri'
require 'digest/md5'
require 'neruda/index'
require 'neruda/org_file'

def run_webrick
  # Inspired by ruby un.rb library, which allows normally to start a
  # webrick server in one line: ruby -run -e httpd public_html -p 5000
  routes = Neruda::Config.settings['routes'] || {}
  options = { Port: Neruda::Config.settings['server_port'] || 5000,
              DocumentRoot: Neruda::Config.settings['public_folder'] }
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

def template_in_file?(head, content)
  digest = Digest::MD5.hexdigest(content)
  check = " Neruda Template: #{digest} "
  head.children.to_a.filter(&:comment?).each do |c|
    return true if c.text == check
  end
  head.first.prepend_child("<!--#{check}-->\n")
  false
end

def customize_output(org_file, file_name)
  templates = Neruda::Config.settings['templates']
  return if templates.nil? || templates.empty?
  dom = Nokogiri::HTML(File.open(file_name, 'r'))
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
  dom.write_to(File.open(file_name, 'w'))
end

def build_indexes(index, verbose = true)
  index.entries.each do |k|
    src = index.write(k)
    warn "Generated index file #{src}" if verbose
    atom = index.write_atom(k)
    warn "Generated atom feed #{atom}" if verbose
  end
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

THROBBER_FRAMES = {
  'basic' => '-\|/',
  'basicdots' => '⋯⋱⋮⋰',
  'moon' => '🌑🌒🌓🌔🌕🌖🌗🌘',
  'clock' => '🕛🕐🕑🕒🕓🕔🕕🕖🕗🕘🕙🕚',
  'bricks' => '⣾⣽⣻⢿⡿⣟⣯⣷',
  'points' => '·⁘∷⁛∷⁘',
  'quadrant2' => '▙▛▜▟',
  'default' => ['⠁ ⠂ ⠄ ⡀ ⠄ ⠂ ⠁', '⠂ ⠁ ⠂ ⠄ ⡀ ⠄ ⠂', '⠄ ⠂ ⠁ ⠂ ⠄ ⡀ ⠄',
                '⡀ ⠄ ⠂ ⠁ ⠂ ⠄ ⡀', '⠄ ⡀ ⠄ ⠂ ⠁ ⠂ ⠄', '⠂ ⠄ ⡀ ⠄ ⠂ ⠁ ⠂']
}.freeze

def throbber(thread, message)
  model = Neruda::Config.settings['throbber'] || 'default'
  model = 'default' unless THROBBER_FRAMES.has_key?(model)
  frames = THROBBER_FRAMES[model]
  current = 0
  while thread.alive?
    sleep 0.1
    print "#{message} #{frames[current % frames.length]}\r"
    current += 1
  end
  done = Rainbow('done'.ljust(frames[0].length)).green
  print "#{message} #{done}\n"
end

namespace :site do
  desc 'Generates all index files'
  task :index do
    blog_path = Neruda::Config.settings['blog_path']
    next unless Dir.exist?("src/#{blog_path}")
    mkdir_p ['src/tags', "#{Neruda::Config.settings['public_folder']}/feeds"]
    index = Neruda::Index.new(Dir.glob("src/#{blog_path}/*/content.org"))
    verbose = Rake::FileUtilsExt.verbose_flag
    if verbose
      build_indexes(index)
      next
    end
    build = Thread.new do
      build_indexes(index, false)
    end
    throbber(build, 'Generating indexes:')
  end

  desc 'Customize HTML output for a given file'
  task :customize_output, :target do |_, args|
    if args[:target].nil?
      warn 'No source file given'
      next
    end
    verbose = Rake::FileUtilsExt.verbose_flag
    source = Neruda::OrgFile.source_for_target(args[:target])
    warn "Customizing file #{args[:target]} from #{source}" if verbose
    customize_output(Neruda::OrgFile.new(source), args[:target])
    print Rainbow('.').bold.green unless verbose
  end

  desc 'Convert one or all org files'
  task :build, :target do |_, args|
    if Rake::FileUtilsExt.verbose_flag
      sh emacs_command(args[:target])
      next
    end
    build = Thread.new do
      sh emacs_command(args[:target], false)
    end
    throbber(build, 'Publishing:')
  end

  desc 'Start a test server'
  task :preview do
    run_webrick
  end
end
