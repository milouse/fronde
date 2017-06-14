require 'yaml'

namespace :chapters do
  desc 'Upload epub files listed in tmp/epub_to_upload.yml'
  task :upload_epubs do
    next unless File.exist?('tmp/epub_to_upload.yml')
    epub_to_upload = YAML.load_file('tmp/epub_to_upload.yml')
    next if epub_to_upload.empty?
    on roles(:app) do
      epub_to_upload.each do |f|
        upload! f, "#{shared_path}/#{f}"
      end
    end
  end

  desc 'Upload the chapters list index'
  task upload_index: 'chapters:index' do
    on roles(:app) do
      upload! 'config/chapters.yml', "#{release_path}/config/chapters.yml"
      info 'chatpers.yml has been built and uploaded'
    end
  end

  desc 'Upload the complete book'
  task upload_book: ['chapters:make_book', 'chapters:upload_epubs']

  namespace :purge do
    desc 'Remove remote orphaned epub files'
    task :remote do
      neruda_config = YAML.load_file('config/config.yml')
      final_org = neruda_config['book_filename'] || 'all'
      on roles(:app) do
        within release_path do
          begin
            epub_files = capture :ls, '-1', 'private/epubs/*.epub'
          rescue SSHKit::Command::Failed
            warn 'No epub files found. Aborting.'
            next
          end
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
  end
end
