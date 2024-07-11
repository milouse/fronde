# frozen_string_literal: true

module Fronde
  class Source
    # Specific settings for Gemini {Fronde::Source}
    class Gemini < Source
      class << self
        def org_default_postamble
          format(
            "📅 %<date>s\n📝 %<author>s %<creator>s",
            author: R18n.t.fronde.org.postamble.written_by,
            creator: R18n.t.fronde.org.postamble.with_emacs,
            date: R18n.t.fronde.org.postamble.last_modification
          )
        end
      end

      private

      def fill_in_specific_config
        @config.merge!(
          'type' => 'gemini', 'ext' => '.gmi', 'mime_type' => 'text/gemini',
          'folder' => CONFIG.get('gemini_public_folder')
        )
      end

      def org_default_options
        { 'publishing-function' => 'org-gmi-publish-to-gemini',
          'gemini-head' => '',
          'gemini-postamble' => Gemini.org_default_postamble }
      end
    end
  end
end
