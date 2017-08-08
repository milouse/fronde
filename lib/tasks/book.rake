# frozen_string_literal: true

require 'yaml'
require 'rainbow'

namespace :book do
  desc 'List various book elements'
  task :list, :list_type do |_, args|
    list_type = args[:list_type]
    next unless ['chapters', 'characters',
                 'notes', 'sceneries'].include?(list_type)
    Dir.glob("private/#{list_type}/*.org") do |filename|
      file_radix = File.basename(filename, '.org')
      title = file_radix.split('_').map(&:capitalize).join(' ')
      absolute_filename = File.join(Dir.pwd, filename)
      puts Rainbow(title).blue + ": #{absolute_filename}"
    end
  end

  desc 'Create the org file of the complete book'
  task prepare: 'chapters:index' do
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
      filename = "private/chapters/#{file_radix}.org"
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

  task make: ['book:prepare', 'chapters:convert_org']
end
