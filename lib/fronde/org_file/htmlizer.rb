# frozen_string_literal: true

require 'fronde/config'
require 'fronde/emacs'

module Fronde
  # This module holds HTML formatter methods for the {Fronde::OrgFile}
  # class.
  module OrgFileHtmlizer
    private

    # Format {Fronde::OrgFile#keywords} list in an HTML listing.
    #
    # @return [String] the HTML keywords list
    def keywords_to_html
      domain = Fronde::Config.settings['domain']
      klist = @keywords.map do |k|
        <<~KEYWORDLINK
          <li class="keyword">
            <a href="#{domain}/tags/#{Fronde::OrgFile.slug(k)}.html">#{k}</a>
          </li>
        KEYWORDLINK
      end.join
      "<ul class=\"keywords-list\">#{klist}</ul>"
    end

    # Format {Fronde::OrgFile#date} as a HTML `time` tag.
    #
    # @return [String] the HTML `time` tag
    def date_to_html(dateformat = :full)
      return '<time></time>' if @date.nil?
      "<time datetime=\"#{@date.rfc3339}\">#{datestring(dateformat)}</time>"
    end

    # Format {Fronde::OrgFile#author} in a HTML `span` tag with a
    #   specific class.
    #
    # @return [String] the author HTML `span`
    def author_to_html
      "<span class=\"author\">#{@author}</span>"
    end
  end
end
