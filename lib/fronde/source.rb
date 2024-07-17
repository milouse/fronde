# frozen_string_literal: true

module Fronde
  # Wrapper for each possible project source.
  # Must be subclassed by specific source type (like Gemini or HTML)
  class Source
    def initialize(source_config)
      @config = {
        'recursive' => true, 'is_blog' => false,
        'domain' => CONFIG.get('domain'), 'atom_feed' => '',
        'org-options' => {
          'section-numbers' => 'nil', 'with-toc' => 'nil'
        }
      }.merge(source_config)
      clean_config
      org_publish_options
      render_heading
    end

    def [](key)
      @config[key]
    end

    def []=(key, value)
      @config[key] = value
    end

    def type
      @config['type']
    end

    def recursive?
      !!@config['recursive']
    end

    def blog?
      !!@config['is_blog']
    end

    def to_h
      @config
    end

    def source_for?(file_name)
      relative_file_path = file_name.delete_prefix "#{@config['path']}/"
      # file_name does not begin with source path.
      return false if relative_file_path == file_name

      # Looks like a file at a deeper level, but current source is not
      # recursive.
      return false if relative_file_path.include?('/') && !recursive?

      # We don’t check file if the file really exist as the current
      # check may be done before the file is actually written.
      true
    end

    def source_for(file_name)
      relative_file_path = file_name.delete_prefix "#{publication_path}/"
      # file_name does not begin with source path.
      return if relative_file_path == file_name

      # Looks like a file at a deeper level, but current source is not
      # recursive.
      return if relative_file_path.include?('/') && !recursive?

      # Looks like a match. But does a source file for this one actually
      # exists?
      relative_source_path = relative_file_path.sub(
        /#{@config['ext']}\z/, '.org'
      )
      source_path = File.join(@config['path'], relative_source_path)
      return unless File.file?(source_path)

      source_path
    end

    def target_for(file_name)
      target = File.expand_path file_name
      target.delete_prefix! "#{Dir.pwd}/"
      target.sub!(/\.org\z/, @config['ext'])
      project_relative_path = @config['path'].delete_prefix("#{Dir.pwd}/")
      target.delete_prefix! "#{project_relative_path}/"
      public_absolute_path + target
    end

    def exclude_file?(file_name)
      # Obviously excluding index itself for blogs
      return true if file_name == File.join(@config['path'], 'index.org')

      exclusion_rules = @config['exclude']
      return false unless exclusion_rules

      file_name.match? exclusion_rules
    end

    def org_config
      name = @config['name']
      [{ 'name' => name, 'attributes' => org_project_config },
       { 'name' => "#{name}-assets", 'attributes' => org_assets_config }]
    end

    # Return the publication absolute path on file system.
    #
    # The returned string never end with a slash (/).
    #
    # Use {Fronde::Source#public_absolute_path} to get the absolute path
    # of this project, as seen from a web browser.
    #
    # @return [String] the absolute path to the target dir of this project
    def publication_path
      return @config['publication_path'] if @config['publication_path']

      publish_in = [File.expand_path(@config['folder']), @config['target']]
      @config['publication_path'] = publish_in.join('/').delete_suffix('/')
    end

    # Return the absolute path as seen in User Agent.
    #
    # The returned string always end with a slash (/).
    #
    # Use {Fronde::Source#publication_path} to locate published file on
    # the file system.
    #
    # @return [String] the absolute path to this project
    def public_absolute_path
      return @config['public_absolute_path'] if @config['public_absolute_path']

      @config['public_absolute_path'] = "/#{@config['target']}/".squeeze('/')
    end

    class << self
      def canonical_config(config)
        config = { 'path' => config } if config.is_a?(String)
        config['type'] ||= 'html'
        config
      end

      def new_from_config(config)
        klass_name = config['type'].capitalize
        klass = Kernel.const_get("::Fronde::Source::#{klass_name}")
        klass.new(config)
      end
    end

    private

    def clean_config
      fill_in_specific_config
      @config['name'] ||= @config['path'].sub(%r{^[.~]*/}, '').tr('/.', '-')
      @config['title'] ||= @config['path']
      @config['target'] ||= File.basename(@config['path']).delete_prefix '.'
      @config['target'] = '' if @config['target'] == '.'
      @config['path'] = File.expand_path(@config['path'])
      @config['theme'] ||= CONFIG.get('theme', 'default')
      # Blog are necessarily recursive to allow publication of tags and feeds
      @config['recursive'] = true if @config['is_blog']
    end

    def org_publish_options
      type = @config['type']
      options = org_default_options.merge(
        @config['org-options'],
        CONFIG.get("org-#{type}", {}),
        @config["org-#{type}"] || {}
      )
      @config['org-options'] = options
    end

    def render_heading
      %w[head head-extra preamble postamble].each do |kind|
        heading_key = "#{@config['type']}-#{kind}"
        heading = @config.dig 'org-options', heading_key
        next unless heading

        @config['org-options'][heading_key] =
          heading.gsub('%F', @config['atom_feed'])
                 .gsub('%h', @config['domain'])
                 .gsub('%o', @config['theme'])
      end
    end

    def org_project_config
      attributes = {
        'base-directory' => @config['path'],
        'base-extension' => 'org',
        'publishing-directory' => publication_path,
        'recursive' => @config['recursive'],
        'fronde-base-uri' => "#{@config['domain']}#{public_absolute_path}"
      }.merge(@config['org-options'])
      exclude = @config['exclude']
      attributes['exclude'] = exclude if exclude
      attributes.sort.to_h # Have lisp config sorted
    end

    def org_assets_config
      {
        'base-directory' => @config['path'],
        'base-extension' => %w[gif jpg png svg pdf].join('\\\\|'),
        'publishing-directory' => publication_path,
        'publishing-function' => 'org-publish-attachment',
        'recursive' => @config['recursive']
      }
    end
  end
end

require_relative 'source/gemini'
require_relative 'source/html'
