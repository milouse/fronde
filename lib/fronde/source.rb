# frozen_string_literal: true

module Fronde
  # Wrapper for each possible project source.
  class Source
    def initialize(source_config)
      @config = {
        'recursive' => true, 'is_blog' => false,
        'type' => 'html', 'ext' => '.html',
        'domain' => CONFIG.get('domain'),
        'folder' => CONFIG.get('html_public_folder'),
        'atom_feed' => '', 'org-html' => {},
        'org-options' => {
          'section-numbers' => 'nil',
          'with-toc' => 'nil',
          'publishing-function' => 'org-html-publish-to-html'
        }
      }
      @config.merge! source_config
      clean_config
      org_publish_options
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
      def org_default_gemini_postamble
        format(
          "📅 %<date>s\n📝 %<author>s %<creator>s",
          author: R18n.t.fronde.org.postamble.written_by,
          creator: R18n.t.fronde.org.postamble.with_emacs,
          date: R18n.t.fronde.org.postamble.last_modification
        )
      end

      def org_default_html_postamble
        <<~POSTAMBLE
          <p><span class="author">#{R18n.t.fronde.org.postamble.written_by}</span>
          #{R18n.t.fronde.org.postamble.with_emacs_html}</p>
          <p class="date">#{R18n.t.fronde.org.postamble.last_modification}</p>
          <p class="validation">%v</p>
        POSTAMBLE
      end

      def org_default_html_head
        <<~HTMLHEAD
          <link rel="stylesheet" type="text/css" media="screen"
                href="{{ domain }}/assets/{{ theme }}/css/style.css">
          <link rel="stylesheet" type="text/css" media="screen"
                href="{{ domain }}/assets/{{ theme }}/css/htmlize.css">
          {{ atom_feed }}
        HTMLHEAD
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
      publish_in = [Dir.pwd]
      if @config['type'] == 'gemini'
        publish_in << CONFIG.get('gemini_public_folder')
      else
        publish_in << CONFIG.get('html_public_folder')
      end
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
      if @config['type'] == 'gemini'
        @config['ext'] = '.gmi'
        @config['folder'] = CONFIG.get('gemini_public_folder')
        options['gemini-postamble'] = Source.org_default_gemini_postamble
        options['publishing-function'] = 'org-gmi-publish-to-gemini'
        return
      end
      options.merge!(
        org_default_html_options,
        CONFIG.get('org-html', {}),
        @config['org-html'] || {}
      )
      if @config['is_blog']
        @config['atom_feed'] = <<~ATOMFEED
          <link rel="alternate" type="application/atom+xml" title="Atom 1.0"
                href="#{@config['domain']}/feeds/index.xml" />
        ATOMFEED
      end
      html_head = options['html-head']
      if html_head
        options['html-head'] = \
          Config::Helpers.render_liquid_template(
            html_head, to_h
          )
      end
      @config['org-options'] = options
    end

    def org_default_html_options
      defaults = {
        'html-postamble' => Source.org_default_html_postamble,
        'html-head' => '{{ atom_feed }}',
        'html-head-include-default-style' => 't',
        'html-head-include-scripts' => 't'
      }
      return defaults if @config['theme'] == 'default'

      defaults['html-head'] = Source.org_default_html_head
      defaults['html-head-include-default-style'] = 'nil'
      defaults['html-head-include-scripts'] = 'nil'
      defaults
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
