# frozen_string_literal: true

require 'yaml'
require 'r18n-core'
require 'singleton'

require_relative 'config/lisp'
require_relative 'source'

module Fronde
  module Config
    # Wrapper for configuration
    #
    # This class is a singleton interface, which share the static website
    # being build settings among different steps or tasks.
    #
    # It expects the website author to holds their custom settings in a
    # YAML file named ~config.yml~ available at the root of their
    # project.
    #
    # For example, with the given config file:
    #
    # #+begin_src
    # ---
    # title: My website
    # author: Alice Doe
    # #+end_src
    #
    # Settings will be available like this:
    #
    # #+begin_src
    # Fronde::CONFIG.get('author')
    # => "Alice Doe"
    # #+end_src
    class Store
      include Singleton

      attr_reader :sources

      def initialize
        @default_settings = {
          'author' => (ENV['USER'] || ''),
          'domain' => '',
          'lang' => Fronde::Config::Helpers.extract_lang_from_env('en'),
          'html_public_folder' => 'public_html',
          'gemini_public_folder' => 'public_gmi',
          'templates' => [], 'theme' => 'default'
        }.freeze
        @org_version = @sources = nil
        @config = load_settings
        # Do not load sources now to avoid dependency loop on config
      end

      include Fronde::Config::Lisp

      # Access the current website settings
      #
      # @return [Hash] the website settings
      def settings
        @config
      end

      # Return a named setting.
      #
      # ~setting~ may be a ~String~ or an ~Array~.
      #
      # If the given ~setting~ name is an Array, this method will
      # behave as ~Hash::dig~.
      #
      # If no value is found for the given setting, ~default~ will be
      # returned.
      #
      # @param setting [String, Array] the setting to get
      # @param default the default value to use if ~setting~ is absent
      # @return the setting value or nil
      def get(setting, default = nil)
        if setting.is_a? Array
          value = @config.dig(*setting)
        else
          value = @config[setting]
        end
        value || default
      end

      # Reset settings
      #
      # This method is handy for testing purpose. Next call to
      # {file:Fronde/Config.html#get-class_method get} or
      # {file:Fronde/Config.html#settings-class_method settings} will
      # force reload the settings from the config file
      #
      # @return nil
      def reset
        # Reload config, taking default settings into account
        @config = load_settings
        @org_version = @sources = nil
        @sources = load_sources
      end

      # Load the given settings as if they comes from the ~config.yml~ file.
      #
      # This method is handy for testing purpose. Next call to
      # {file:Fronde/Config.html#get-class_method get},
      # {file:Fronde/Config.html#sources-class_method sources} or
      # {file:Fronde/Config.html#settings-class_method settings} will
      # use these new settings.
      #
      # @param config [Hash] the settings to artificially load
      # @return [Fronde::Config::Store] self
      def load_test(config)
        @config = @default_settings.merge config
        @org_version = @sources = nil
        @sources = load_sources
        self
      end

      # Return the qualified projects sources list.
      #
      # @return [Array] the fully qualified projects sources list
      def load_sources
        return @sources if @sources
        sources = build_sources
        if sources.any?(&:blog?)
          sources << Fronde::Source::Html.new(
            'path' => 'tags', 'recursive' => false,
            'has_assets' => false
          )
        end
        @sources = remove_inclusion(remove_duplicate(sources))
      end

      private

      def load_settings
        conf_file = 'config.yml'
        user_conf = {}
        if File.exist? conf_file
          user_conf = YAML.load_file(conf_file)
          user_conf = Fronde::Config::Helpers.migrate(user_conf)
        end
        user_conf = @default_settings.merge(user_conf)
        Fronde::Config::Helpers.ensure_expanded_paths(user_conf).freeze
      end

      def build_sources
        default_sources = [{ 'path' => 'src', 'target' => '.' }]
        get('sources', default_sources).filter_map do |source_conf|
          config = Source.canonical_config source_conf.dup
          unless config['path']
            warn R18n.t.fronde.error.source.no_path(source: config.inspect)
            next
          end
          Source.new_from_config config
        end
      end

      def remove_duplicate(sources)
        check_paths = {}
        sources.each do |source|
          type = source.type
          check_paths[type] ||= {}
          path = source['path']
          # Avoid duplicate
          if check_paths[type].has_key?(path)
            warn(
              R18n.t.fronde.error.source.duplicate(
                source: path, type: type
              )
            )
            next
          end
          check_paths[type][path] = source
        end
        check_paths
      end

      def remove_inclusion(check_paths)
        check_paths.map do |type, paths|
          paths.filter_map do |path, source|
            # Ensure that the current source does not embed another one
            # or is not embedde into another one.
            possible_matchs = check_paths.select do |other_path, other_source|
              # This is a problem only if the other source is recursive
              other_path.start_with?(path) && other_source.recursive?
            end.keys
            # At this point we cannot have any duplicate, as they must
            # have been removed in the previous filter_map. Thus we
            # cannot have only one possible config per path. It is then
            # safe to assume we can remove the current path from the
            # possible_paths array as this item is the one currently
            # checked.
            possible_matchs.delete(path)
            # We only keep the configuration if it is not recursive (so
            # no problem to have other config targeting folder under
            # it), or if no other config already target it.
            next source if possible_matchs.empty? || !source.recursive?

            warn(
              R18n.t.fronde.error.source.inclusion(
                source: source, possible_matchs: possible_matchs, type: type
              )
            )
          end
        end.flatten
      end
    end
  end

  CONFIG = Config::Store.instance
end

R18n.default_places = File.expand_path('../../locales', __dir__)
R18n::Filters.on(:named_variables)
R18n.set Fronde::CONFIG.get('lang', 'en')

Fronde::CONFIG.load_sources
