# coding: utf-8
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
        @config['blog_slug'] ||= 'blog'
        @config['templates'] ||= []
        license = Neruda::Config.settings['license']
        license_meta = ''
        if license
          license = license.split("\n").map(&:strip).join(' ')
          license_meta = "<meta property=\"dc.rights\" content=\"#{license}\">"
        end
        twitter_handle = Neruda::Config.settings['twitter_account']
        twitter_meta = ''
        if twitter_handle
          twitter_meta = <<~TWITTER
            <meta name="twitter:creator" content="@#{twitter_handle}">
          TWITTER
        end
        title = Neruda::Config.settings['title'] || ''
        title = title.split("\n").map(&:strip).join(' ')
        metadata = <<~ENDMETA
          <meta name="description" lang="%l" content="TBD">
          <link rel="schema.dc" href="http://purl.org/dc/elements/1.1/">
          <meta property="dc.publisher" content="%a">
          <meta property="dc.type" content="text">
          <meta property="dc.format" content="text/html">
          <meta property="dc.title" lang="%l" content="%t">
          <meta property="dc.description" lang="%l" content="TBD">
          <meta property="dc.language" content="%l">
          <meta property="dc.date" content="%I">
          #{license_meta}

          <meta name="twitter:card" content="summary">
          #{twitter_meta}
          <meta property="og:type" content="article">
          <meta property="og:title" content="%t">
          <meta property="og:article:published_time" content="%I">
          <meta property="og:url" content="%u">
          <meta property="og:locale" content="%l">
          <meta property="og:description" content="TBD">
          <meta property="og:site_name" content="#{title}">
        ENDMETA
        @config['templates'] << {
          'type' => 'before',
          'selector' => 'title',
          'content' => metadata
        }
      end
    end
  end
end
