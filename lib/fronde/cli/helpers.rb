# frozen_string_literal: true

require 'fileutils'

module Fronde
  module CLI
    # Various utilitaries methods
    module Helpers
      def self.init_config_file(config)
        return if File.exist? 'config.yml'

        output = config[:output] || 'html'
        output = 'gemini' if output == 'gmi'
        data = { 'author' => config[:author],
                 'lang' => config[:lang],
                 'output' => output }
        source = File.expand_path './data/config.yml', __dir__
        template = Liquid::Template.parse(File.read(source))
        File.write('config.yml', template.render(data))
      end

      def self.init_rakefile
        FileUtils.cp(
          File.expand_path('./data/Rakefile', __dir__),
          'Rakefile'
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
