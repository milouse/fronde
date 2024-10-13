# frozen_string_literal: true

require 'json'
require 'time'
require 'yaml'
require 'net/http'
require 'fileutils'
require 'digest/sha1'

module Fronde
  module Sync
    # Everything needed to connect to neocities
    class Neocities
      PROTECTED_FILES = %w[index.html neocities.png not_found.html].freeze

      def initialize(connection_spec, public_folder, verbose: false, &block)
        @verbose = verbose
        @endpoint = @website_name = @authorization = nil
        extract_connection_details connection_spec
        @public_folder = public_folder
        @connection = init_connection
        return unless block

        yield self
        finish
      end

      def remote_list
        remote = call build_request('/list')
        JSON.parse(remote.body)['files'].map do |stat|
          stat['updated_at'] = Time.parse(stat['updated_at'])
          stat
        end
      end

      def local_list
        Dir.chdir(@public_folder) do
          Dir.glob('**/*').map { |file| neocities_stat(file) }
        end
      end

      def info
        info = call build_request('/info')
        JSON.parse(info.body)['info']
      end

      def finish
        @connection.finish if @connection.started?
      end

      def pull(test: false)
        file_list = remote_list
        finish
        orphans = select_orphans(file_list, local_list) do |path|
          warn "deleting #{path}" if @verbose

          "#{@public_folder}/#{path}"
        end
        File.unlink(*orphans) unless test
        download_all(file_list, test:)
        nil # Mute this method
      end

      def push(test: false)
        file_list = local_list
        remove_remote_orphans(file_list, test:)
        upload_all(file_list, test:)
        finish
      end

      private

      def neocities_stat(file)
        stat = File.stat(file)
        data = {
          'path' => file,
          'is_directory' => stat.directory?,
          'updated_at' => stat.mtime.round.utc
        }
        return data if data['is_directory']

        data['size'] = stat.size
        data['sha1_hash'] = Digest::SHA1.hexdigest File.read(file)
        data
      end

      def select_orphans(to_apply, current_list, &)
        paths_to_apply = to_apply.map { _1['path'] }
        current_paths = current_list.map { _1['path'] }
        (current_paths - paths_to_apply).filter_map(&)
      end

      def remove_remote_orphans(file_list, test: false)
        request = build_request '/delete', :post
        orphan_paths = select_orphans(file_list, remote_list) do |path|
          # Never remove the following files. If needed you can still
          # overwrite them. And in any case, neocities will not allow
          # the index.html file to be removed.
          next if PROTECTED_FILES.include? path

          warn "deleting #{path}" if @verbose
          path
        end
        request.form_data = { 'filenames[]' => orphan_paths }
        return if test

        call request
      end

      def download_all(file_list, test: false)
        publish_domain = "#{@website_name}.#{@endpoint.host}"
        Dir.chdir(@public_folder) do
          Net::HTTP.start(publish_domain, use_ssl: true) do |http|
            file_list.each do |file_data|
              path = file_data['path']
              file_data['uri'] = "https://#{publish_domain}/#{path}"
              download_file http, file_data, test:
            end
          end
        end
      end

      def download_file(http, file_data, test: false)
        path = file_data['path']
        if file_data['is_directory']
          warn "#{path}/" if @verbose
          FileUtils.mkdir_p path unless test
          return
        end

        warn path if @verbose

        content = fetch_file_content(
          http, file_data['uri'], file_data['sha1_hash']
        )
        return unless content && !test

        save_file path, content, file_data['updated_at']
      end

      def fetch_file_content(http, uri, sha1sum)
        # Neocities redirect HTML file to location without extension and
        # redirect index.html file to /
        uri = uri.delete_suffix('index.html').delete_suffix('.html')
        content = http.get(uri).body
        check = Digest::SHA1.hexdigest content
        return content if check == sha1sum

        warn "SHA1 hash differ for #{uri}"
      end

      def save_file(path, content, updated_at)
        File.write path, content
        FileUtils.touch path, mtime: updated_at
        path
      end

      def prepare_files_to_upload(file_list)
        Dir.chdir(@public_folder) do
          file_list.filter_map do |file_data|
            # No need to push intermediary directories, they are created
            # on the fly
            next if file_data['is_directory']

            path = file_data['path']
            warn path if @verbose
            [path, File.new(path)]
          end
        end
      end

      def upload_all(file_list, test: false)
        form_data = prepare_files_to_upload file_list
        return if test

        request = build_request '/upload', :post
        request.set_form form_data, 'multipart/form-data'
        call request
      end

      def extract_connection_details(connection_spec)
        # Do not put your password into the fronde config. The password
        # is expectfed to be found in a specific config file (not to be
        # shared).
        @website_name, endpoint = connection_spec.split('@', 2)
        endpoint ||= 'neocities.org'
        @endpoint = URI("https://#{endpoint}/api")
        # Will raise Errno::ENOENT if file does not exist
        credentials = YAML.load_file('.credentials')
        # Will raise KeyError if not set
        password = credentials.fetch("#{@website_name}_neocities_pass")
        authorization = [[@website_name, password].join(':')].pack('m0')
        @authorization = "Basic #{authorization}"
      end

      def build_request(path, method = :get)
        uri = @endpoint.dup
        uri.path += path
        klass = Kernel.const_get "Net::HTTP::#{method.to_s.capitalize}"
        klass.new uri
      end

      def call(request)
        request['Authorization'] = @authorization
        @connection.start unless @connection.started?
        outcome = @connection.request request
        return outcome if outcome.is_a? Net::HTTPSuccess

        raise JSON.parse(outcome.body).inspect
      end

      def init_connection
        http = Net::HTTP.new @endpoint.host, 443
        http.use_ssl = true
        http.verify_mode = OpenSSL::SSL::VERIFY_PEER
        http
      end
    end
  end
end
