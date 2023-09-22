# frozen_string_literal: true

require_relative '../config'
require_relative '../emacs'
require_relative '../utils'

module Fronde
  # This module holds HTML formatter methods for the {Fronde::OrgFile}
  # class.
  module OrgFileHtmlizer
    private

    # Format {Fronde::OrgFile#keywords} list in an HTML listing.
    #
    # @return [String] the HTML keywords list
    def keywords_to_html
      domain = Fronde::CONFIG.get('domain')
      klist = @data[:keywords].map do |k|
        <<~KEYWORDLINK
          <li class="keyword">
            <a href="#{domain}/tags/#{Fronde::Utils.slug(k)}.html">#{k}</a>
          </li>
        KEYWORDLINK
      end.join
      "<ul class=\"keywords-list\">#{klist}</ul>"
    end
  end
end
