# frozen_string_literal: true

require 'yaml'

# Wrapper for configuration
module Neruda
  class Config
    class << self
      def settings
        load_settings unless @config
        @config
      end

      def reload!
        load_settings
      end

      private

      def load_settings
        @config = {}
        conf = 'config.yml'
        @config = YAML.load_file(conf).freeze if File.exist? conf
        @config
      end
    end
  end
end
