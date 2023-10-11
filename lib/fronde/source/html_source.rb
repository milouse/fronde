# frozen_string_literal: true

module Fronde
  class Source::Html < Source
    def org_config
      config = super
      config[0]['theme'] = @config['theme']
      config
    end

    class << self
      def org_default_postamble
        <<~POSTAMBLE
          <p><span class="author">#{R18n.t.fronde.org.postamble.written_by}</span>
          #{R18n.t.fronde.org.postamble.with_emacs_html}</p>
          <p class="date">#{R18n.t.fronde.org.postamble.last_modification}</p>
          <p class="validation">%v</p>
        POSTAMBLE
      end

      def org_default_head
        '{{ atom_feed }}'
      end
    end

    private

    def specific_config
      @config.merge!(
        'type' => 'html', 'ext' => '.html', 'mime_type' => 'text/html',
        'folder' => CONFIG.get('html_public_folder')
      )
    end

    def org_publish_options
      if @config['is_blog']
        @config['atom_feed'] = <<~ATOMFEED
          <link rel="alternate" type="application/atom+xml" title="#{@config['title']}"
                href="#{@config['domain']}/feeds/index.xml" />
        ATOMFEED
      end
      super
    end

    def org_default_options
      defaults = {
        'publishing-function' => 'org-html-publish-to-html',
        'html-head-include-default-style' => 't',
        'html-head-include-scripts' => 't'
      }
      return defaults if @config['theme'] == 'default'

      defaults.merge(
        'html-head-include-default-style' => 'nil',
        'html-head-include-scripts' => 'nil',
        'html-head' => <<~HTMLHEAD
          <link rel="stylesheet" type="text/css" media="screen"
                href="{{ domain }}/assets/{{ theme }}/css/style.css">
          <link rel="stylesheet" type="text/css" media="screen"
                href="{{ domain }}/assets/{{ theme }}/css/htmlize.css">
          {{ atom_feed }}
        HTMLHEAD
      )
    end
  end
end
