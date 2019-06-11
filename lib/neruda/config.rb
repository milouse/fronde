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

      def save(new_config)
        IO.write 'config.yml', new_config.to_yaml
        @config = new_config.freeze
      end

      def load_test(config)
        @config = config.freeze
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
