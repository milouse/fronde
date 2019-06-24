# frozen_string_literal: true

require 'yaml'

# Wrapper for configuration
module Neruda
  class Config
    class << self
      def settings
        return load_settings unless @config
        @config
      end

      def save(new_config)
        IO.write 'config.yml', new_config.to_yaml
        @config = new_config.freeze
      end

      def load_test(config)
        @config = config
        add_default_settings
      end

      def org_last_version
        return @org_version if @org_version
        index = open('https://orgmode.org/index.html', 'r').read
        last_ver = index.match(/https:\/\/orgmode\.org\/org-([0-9.]+)\.tar\.gz/)
        if last_ver.nil?
          warn 'Org last version not found'
          return nil
        end
        @org_version = last_ver[1]
      end

      private

      def load_settings
        @config = {}
        conf = 'config.yml'
        @config = YAML.load_file(conf) if File.exist? conf
        add_default_settings
        @config.freeze
      end

      def add_default_settings
        @config['lang'] ||= 'en'
        @config['domain'] ||= ''
        @config['public_folder'] ||= 'public_html'
        @config['blog_path'] ||= 'blog'
        @config['templates'] ||= []
      end
    end
  end
end
