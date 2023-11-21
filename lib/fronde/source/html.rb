# frozen_string_literal: true

module Fronde
  class Source
    # Specific settings for HTML {Fronde::Source}
    class Html < Source
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
      end

      private

      def fill_in_specific_config
        @config.merge!(
          'type' => 'html', 'ext' => '.html', 'mime_type' => 'text/html',
          'folder' => CONFIG.get('html_public_folder')
        )
      end

      def org_publish_options
        if @config['is_blog']
          @config['atom_feed'] = <<~ATOMFEED
            <link rel="alternate" type="application/atom+xml" title="#{@config['title']}"
                  href="#{@config['domain']}#{public_absolute_path}feeds/index.xml" />
          ATOMFEED
        end
        super
      end

      def org_default_options # rubocop:disable Metrics/MethodLength
        defaults = {
          'publishing-function' => 'org-html-publish-to-html',
          'html-head-include-default-style' => 't',
          'html-head-include-scripts' => 't',
          'html-head' => '%F',
          'html-postamble' => Html.org_default_postamble
        }
        return defaults if @config['theme'] == 'default'

        defaults.merge(
          'html-head-include-default-style' => 'nil',
          'html-head-include-scripts' => 'nil',
          'html-head' => <<~HTMLHEAD
            <link rel="stylesheet" type="text/css" media="screen"
                  href="%h/assets/%o/css/style.css">
            <link rel="stylesheet" type="text/css" media="screen"
                  href="%h/assets/%o/css/htmlize.css">
            %F
          HTMLHEAD
        )
      end
    end
  end
end
