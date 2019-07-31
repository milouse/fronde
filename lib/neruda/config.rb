# frozen_string_literal: true

require 'yaml'
require 'neruda/config/lisp_config'

module Neruda
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
  # Neruda::Config.settings['author']
  # => "Alice Doe"
  # #+end_src
  class Config
    extend Neruda::LispConfig

    class << self
      # Access the current website settings
      #
      # If the settings have not been loaded yet, this method is
      # responsible for calling the one, which actually loads them.
      #
      # @return [Hash] the website settings
      def settings
        return load_settings unless @config
        @config
      end

      # Save the settings given as a parameter to the ~config.yml~ file.
      #
      # Not only this method overwrite the old settings, but it replace
      # the current shared settings with the ones given in
      # parameter. Later call to
      # {file:Neruda/Config.html#settings-class_method settings}
      # will, obviously, use these new settings.
      #
      # @param new_config [Hash] the settings to save
      # @return [Hash] the new settings after save
      def save(new_config)
        IO.write 'config.yml', new_config.to_yaml
        @config = new_config.freeze
      end

      # Load the given settings as if they comes from the ~config.yml~ file.
      #
      # This method is handy for testing purpose. Later call to
      # {file:Neruda/Config.html#settings-class_method settings} will
      # use these new settings.
      #
      # @param config [Hash] the settings to artificially load
      # @return [Hash] the new settings
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
        @config['blog_path'] ||= 'blog'
        @config['blog_pattern'] ||= '**/*.org'
        @config['templates'] ||= []
      end
    end
  end
end
