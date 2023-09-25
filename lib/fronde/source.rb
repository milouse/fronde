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
          'section-numbers' => 'nil',
          'with-toc' => 'nil'
        }
      }
      @config.merge! source_config
      specific_config
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

    def path
      @config['path']
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
      return nil if relative_file_path == file_name

      # Looks like a file at a deeper level, but current source is not
      # recursive.
      return nil if relative_file_path.include?('/') && !recursive?

      # Looks like a match. But does a source file for this one actually
      # exists?
      relative_source_path = relative_file_path.sub(
        /#{@config['ext']}\z/, '.org'
      )
      source_path = File.join(@config['path'], relative_source_path)
      return nil unless File.file?(source_path)

      source_path
    end

    def target_for(file_name)
      target = File.expand_path file_name
      target.delete_prefix! "#{Dir.pwd}/"
      target.sub!(/\.org\z/, @config['ext'])
      project_relative_path = @config['path'].delete_prefix("#{Dir.pwd}/")
      target.delete_prefix! "#{project_relative_path}/"
      return target if @config['target'] == '.'

      "#{@config['target']}/#{target}"
    end

    def org_config
      name = @config['name']
      [{ 'name' => name,
         'attributes' => org_project_config },
       { 'name' => "#{name}-assets",
         'attributes' => org_assets_config }]
    end

    class << self
      def canonical_config(config)
        config = { 'path' => config } if config.is_a?(String)
        config['type'] ||= 'html'
        config
      end

      def new_from_config(config)
        klass_name = config['type'].capitalize
        klass = Kernel.const_get("::Fronde::#{klass_name}Source")
        klass.new(config)
      end
    end

    private

    # Return the full path to the publication path of a given project
    #   configuration.
    #
    # @param project [Hash] a project configuration (as extracted from
    #   the ~sources~ key)
    # @return [String] the full path to the target dir of this project
    def publication_path
      publish_in = [Dir.pwd, @config['folder']]
      target = @config['target']
      publish_in << target unless target == '.'
      publish_in.join('/')
    end

    def clean_config
      @config['name'] ||= File.basename(@config['path']).sub(/^\./, '')
      @config['path'] = File.expand_path(@config['path'])
      @config['target'] ||= @config['name']
      @config['theme'] ||= CONFIG.get('theme', 'default')
    end

    def org_publish_options
      options = @config['org-options']
      type = @config['type']
      heading_key = "#{type}-head"
      klass = self.class
      options[heading_key] = klass.org_default_head
      options["#{type}-postamble"] = klass.org_default_postamble

      options.merge!(
        org_default_options,
        CONFIG.get("org-#{type}", {}),
        @config["org-#{type}"] || {}
      )
      @config['org-options'] = options
    end

    def render_heading
      heading_key = "#{@config['type']}-head"
      heading = @config.dig 'org-options', heading_key
      return unless heading

      @config['org-options'][heading_key] = \
        Config::Helpers.render_liquid_template(
          heading, to_h
        )
    end

    def org_project_config
      attributes = {
        'base-directory' => @config['path'],
        'base-extension' => 'org',
        'publishing-directory' => publication_path,
        'recursive' => @config['recursive']
      }.merge(@config['org-options'])
      exclude = @config['exclude']
      attributes['exclude'] = exclude if exclude
      attributes
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

Dir['source/*.rb', base: __dir__].each { |f| require_relative f }
