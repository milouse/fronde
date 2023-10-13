# frozen_string_literal: true

module Fronde
  class Source::Gemini < Source
    def blog?
      # TODO: See how to support blog/indexes with gemini
      false
    end

    class << self
      def org_default_postamble
        format(
          "📅 %<date>s\n📝 %<author>s %<creator>s",
          author: R18n.t.fronde.org.postamble.written_by,
          creator: R18n.t.fronde.org.postamble.with_emacs,
          date: R18n.t.fronde.org.postamble.last_modification
        )
      end

      def org_default_head
        ''
      end
    end

    private

    def specific_config
      @config.merge!(
        'type' => 'gemini', 'ext' => '.gmi', 'mime_type' => 'text/gemini',
        'folder' => CONFIG.get('gemini_public_folder')
      )
    end

    def org_default_options
      { 'publishing-function' => 'org-gmi-publish-to-gemini' }
    end
  end
end
