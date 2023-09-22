# frozen_string_literal: true

require 'uri'
require 'net/http'
require 'fileutils'

module Fronde
  module CLI
    # Various utilitaries methods
    module Helpers
      def self.init_required_files
        FileUtils.cp(
          File.expand_path('./data/Rakefile', __dir__),
          'Rakefile'
        )
        return if File.exist? '.gitignore'

        FileUtils.cp(
          File.expand_path('./data/gitignore', __dir__),
          '.gitignore'
        )
      end

      def self.update_config(options)
        cnf = options.merge
        cnf.delete(:verbose)
        cnf.transform_keys!(&:to_s)
        Fronde::CONFIG.save(Fronde::CONFIG.settings.merge(cnf))
      end

      def self.launch_app_for_uri(uri)
        case current_os
        when 'windows'
          system 'start', uri
        when 'apple'
          system 'open', uri
        else
          system 'gio', 'open', uri
        end
      end

      # Try to discover the current host operating system.
      #
      # @return [String] either apple, windows or linux (default)
      def self.current_os
        if ENV['OS'] == 'Windows_NT' || RUBY_PLATFORM.include?('cygwin')
          return 'windows'
        end
        return 'apple' if RUBY_PLATFORM.include?('darwin')

        'linux'
      end

      # Download latest org-mode tarball.
      #
      # @param destination [String] where to save the org-mode tarball
      # @return [String] the downloaded org-mode version
      def self.download_org(destination = 'var/tmp')
        org_last_version = Fronde::CONFIG.org_last_version

        # :nocov:
        return if org_last_version.nil?

        # :nocov:
        # Remove version number in dest file to allow easy rake file
        # task naming
        dest_file = File.expand_path('org.tar.gz', destination)
        return if File.exist?(dest_file)
        tarball = "org-mode-release_#{org_last_version}.tar.gz"
        uri = URI("https://git.savannah.gnu.org/cgit/emacs/org-mode.git/snapshot/#{tarball}")
        # Will crash on purpose if anything goes wrong
        Net::HTTP.start(uri.host) do |http|
          request = Net::HTTP::Get.new uri

          http.request request do |response|
            File.open(dest_file, 'w') do |io|
              response.read_body { |chunk| io.write chunk }
            end
          end
        end
      end
    end
  end
end
