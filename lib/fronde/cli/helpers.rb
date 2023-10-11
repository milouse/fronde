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
    end
  end
end
