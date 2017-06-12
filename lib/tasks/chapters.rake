require 'yaml'

namespace :chapters do
  desc 'Identify orphan (without epubs) org files'
  task :list_orphaned_orgs do
    org_to_convert = []
    File.unlink 'tmp/org_to_convert.yml' if File.exist? 'tmp/org_to_convert.yml'
    on roles(:app) do
      within release_path do
        begin
          output = capture :ls, '-1', 'private/orgs/*.org'
        rescue SSHKit::Command::Failed
          warn 'You do not have pushed any org files. Aborting.'
          next
        end
        output.each_line do |filename|
          filename.delete!("\n")
          file_radix = File.basename(filename, '.org')
          next if file_radix == 'index'
          epub_file = "private/epubs/#{file_radix}.epub"
          unless test("[ -e '#{shared_path}/#{epub_file}' ]")
            org_to_convert << filename
          end
        end
      end
    end
    next if org_to_convert.empty?
    Dir.mkdir 'tmp' unless Dir.exist? 'tmp'
    IO.write('tmp/org_to_convert.yml', org_to_convert.to_yaml)
  end

  desc 'Identify all org files'
  task :list_all_orgs do
    org_to_convert = []
    File.unlink 'tmp/org_to_convert.yml' if File.exist? 'tmp/org_to_convert.yml'
    Dir.glob('private/orgs/*.org') do |filename|
      file_radix = File.basename(filename, '.org')
      next if file_radix == 'index'
      org_to_convert << filename
    end
    next if org_to_convert.empty?
    Dir.mkdir 'tmp' unless Dir.exist? 'tmp'
    IO.write('tmp/org_to_convert.yml', org_to_convert.to_yaml)
  end

  desc 'Convert org files from tmp/org_to_convert.yml to epubs'
  task :convert_org do
    next unless File.exist?('tmp/org_to_convert.yml')
    File.unlink 'tmp/epub_to_upload.yml' if File.exist? 'tmp/epub_to_upload.yml'
    org_to_convert = YAML.load_file('tmp/org_to_convert.yml')
    next if org_to_convert.empty?
    epub_to_upload = []
    run_locally do
      org_to_convert.each do |filename|
        file_radix = File.basename(filename, '.org')
        next if file_radix == 'index'
        epub_file = "private/epubs/#{file_radix}.epub"
        epub_to_upload << epub_file
        execute :pandoc, '-S', "-o #{epub_file}", filename
      end
    end
    next if epub_to_upload.empty?
    IO.write('tmp/epub_to_upload.yml', epub_to_upload.to_yaml)
  end

  desc 'Upload epub files listed in tmp/epub_to_upload.yml'
  task upload_epubs: 'chapters:convert_org' do
    next unless File.exist?('tmp/epub_to_upload.yml')
    epub_to_upload = YAML.load_file('tmp/epub_to_upload.yml')
    next if epub_to_upload.empty?
    on roles(:app) do
      epub_to_upload.each do |f|
        upload! f, "#{shared_path}/#{f}"
      end
    end
  end

  desc 'Build missing epubs'
  task build_epubs: ['chapters:list_orphaned_orgs', 'chapters:upload_epubs']

  namespace :build_epubs do
    desc '(re)Build all epub files for each org files'
    task force_all: ['chapters:list_all_orgs', 'chapters:upload_epubs']
  end

  desc 'Remove orphaned (epub without org files) epub files'
  task :purge do
    neruda_config = YAML.load_file('config/config.yml')
    final_org = neruda_config['book_filename'] || 'all'
    on roles(:app) do
      within release_path do
        epub_files = capture :ls, '-1', 'private/epubs/*.epub'
        epub_files.each_line do |filename|
          filename.delete!("\n")
          file_radix = File.basename(filename, '.epub')
          next if file_radix == final_org
          org_file = "private/orgs/#{file_radix}.org"
          unless test("[ -e '#{release_path}/#{org_file}' ]")
            execute :rm, filename
          end
        end
      end
    end
  end

  desc 'Create/Update the chapter list index (local task)'
  task :index do
    require 'yaml'
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
    on roles(:app) do
      upload! 'config/chapters.yml', "#{release_path}/config/chapters.yml"
      info 'chatpers.yml has been built and uploaded'
    end
  end

  desc 'Create the org file of the complete book'
  task prepare_book: 'chapters:index' do
    require 'org-ruby'
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

  desc 'Make the complete book'
  task make_book: ['chapters:prepare_book', 'chapters:upload_epubs']
end
