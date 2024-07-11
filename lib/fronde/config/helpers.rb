# frozen_string_literal: true

require 'liquid'
require 'digest/md5'

module Fronde
  module Config
    # Various utilitaries methods
    module Helpers
      def self.extract_lang_from_env(default)
        (ENV['LANG'] || default).split('_', 2).first
      end

      def self.migrate(config)
        return config unless config.has_key?('public_folder')

        warn R18n.t.fronde.error.config.deprecated_public_folder
        old_pub_folder = config.delete('public_folder')
        return config if config.has_key?('html_public_folder')

        config['html_public_folder'] = old_pub_folder
        config
      end

      def self.ensure_expanded_paths(config)
        %w[html gemini].each do |what|
          key = "#{what}_public_folder"
          config[key] = File.expand_path config[key]
        end
        config
      end

      def self.render_liquid_template(content, data)
        template = Liquid::Template.parse(content)
        template.render(data)
      end
    end

    # Filter for liquid templates
    module Filters
      def cast_lisp_value(value, key)
        return 't' if value.is_a?(TrueClass)
        return 'nil' if value.nil? || value.is_a?(FalseClass)

        value = value.strip
        lisp_keywords = ['t', 'nil', '1', '-1', '0']
        if key.end_with?('-function') || lisp_keywords.include?(value)
          return value
        end

        value.gsub!('"', '\"')
        %("#{value}")
      end

      def md5(value)
        Digest::MD5.hexdigest(value)
      end
    end
  end
end

Liquid::Template.register_filter(Fronde::Config::Filters)
