# frozen_string_literal: true

require 'yaml'

namespace :chapters do
  desc 'Identify all org files'
  task :list_all_orgs do
    org_to_convert = []
    File.unlink 'tmp/org_to_convert.yml' if File.exist? 'tmp/org_to_convert.yml'
    neruda_config = YAML.load_file('config/config.yml')
    chapters = neruda_config['chapters']
    next unless chapters.any?
    chapters.each do |file_radix|
      next if file_radix == 'index'
      filename = "private/orgs/#{file_radix}.org"
      org_to_convert << filename
    end
    next if org_to_convert.empty?
    Dir.mkdir 'tmp' unless Dir.exist? 'tmp'
    IO.write('tmp/org_to_convert.yml', org_to_convert.to_yaml)
  end

  desc 'Identify orphan (without epubs) org files'
  task list_orphaned_orgs: 'chapters:list_all_orgs' do
    next unless File.exist? 'tmp/org_to_convert.yml'
    org_to_convert = []
    chapters = YAML.load_file('tmp/org_to_convert.yml')
    next unless chapters.any?
    chapters.each do |filename|
      file_radix = File.basename(filename, '.org')
      epub_file = "private/epubs/#{file_radix}.epub"
      org_to_convert << filename unless File.exist? epub_file
    end
    next if org_to_convert.empty?
    IO.write('tmp/org_to_convert.yml', org_to_convert.to_yaml)
  end

  desc 'Convert org files from tmp/org_to_convert.yml to epubs'
  task :convert_org do
    next unless File.exist?('tmp/org_to_convert.yml')
    File.unlink 'tmp/epub_to_upload.yml' if File.exist? 'tmp/epub_to_upload.yml'
    org_to_convert = YAML.load_file('tmp/org_to_convert.yml')
    next if org_to_convert.empty?
    epub_to_upload = []
    org_to_convert.each do |filename|
      file_radix = File.basename(filename, '.org')
      next if file_radix == 'index'
      epub_file = "private/epubs/#{file_radix}.epub"
      epub_to_upload << epub_file
      sh 'pandoc', '-S', "-o #{epub_file}", filename
    end
    next if epub_to_upload.empty?
    IO.write('tmp/epub_to_upload.yml', epub_to_upload.to_yaml)
  end

  desc 'Build missing epubs'
  task build_epubs: ['chapters:list_orphaned_orgs', 'chapters:convert_org']

  namespace :build_epubs do
    desc '(re)Build all epub files for each org files'
    task force_all: ['chapters:list_all_orgs', 'chapters:convert_org']
  end

  desc 'Remove orphaned (epub without org files) epub files'
  task :purge do
    neruda_config = YAML.load_file('config/config.yml')
    final_org = neruda_config['book_filename'] || 'all'
    chapters = neruda_config['chapters']
    Dir.glob('private/orgs/*.org') do |filename|
      file_radix = File.basename(filename, '.org')
      unless chapters.include? file_radix
        STDERR.puts "WARNING: #{filename} exists but #{file_radix} " \
                    'is no more in the chapters list.'
      end
    end

    Dir.glob('private/epubs/*.epub') do |filename|
      file_radix = File.basename(filename, '.epub')
      next if file_radix == final_org
      org_file = "private/orgs/#{file_radix}.org"
      File.unlink filename unless File.exist? org_file
    end
  end

  desc 'Create/Update the chapters list index'
  task :index do
    require 'org-ruby'
    neruda_config = YAML.load_file('config/config.yml')
    next if neruda_config['chapters'].nil?
    chapters = []
    neruda_config['chapters'].each do |file_radix|
      filename = "private/orgs/#{file_radix}.org"
      next unless File.exist? filename
      f = Orgmode::Parser.load filename
      title = f.in_buffer_settings['TITLE']
      chapters << { slug: file_radix, title: title } unless title.nil?
    end
    IO.write('config/chapters.yml', chapters.to_yaml)
  end

  desc 'Create the org file of the complete book'
  task prepare_book: 'chapters:index' do
    chapters = YAML.load_file('config/chapters.yml')
    next if chapters.nil?

    neruda_config = YAML.load_file('config/config.yml')
    final_org = neruda_config['book_filename'] || 'all'
    final_org = "tmp/#{final_org}.org"

    Dir.mkdir 'tmp' unless Dir.exist? 'tmp'
    File.unlink final_org if File.exist? final_org

    org_file = File.open(final_org, 'a')
    org_file.write("#+title: #{neruda_config['title']}\n")
    org_file.write("#+author: #{neruda_config['author']}\n")
    org_file.write("#+rights: #{neruda_config['license']}\n")
    org_file.write("#+language: #{neruda_config['lang']}\n\n")

    chapters.each do |c|
      file_radix = c[:slug]
      filename = "private/orgs/#{file_radix}.org"
      next unless File.exist? filename
      file_content = IO.read(filename)
      file_content.gsub!(/^#\+date:.*$/mi, '')
      file_content.gsub!(/^#\+author:.*$/mi, '')
      file_content.gsub!(/^(\*+)\s+(.*)$/mi, '*\1 \2')
      file_content.gsub!(/^#\+title:\s?(.*)$/mi, '* \1')

      org_file.write(file_content + "\n")
    end
    org_file.close

    IO.write('tmp/org_to_convert.yml', [final_org].to_yaml)
  end

  task make_book: ['chapters:prepare_book', 'chapters:convert_org']
end
