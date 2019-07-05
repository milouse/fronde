# frozen_string_literal: true

module Neruda
  # This module holds HTML formatter methods for the {Neruda::OrgFile}
  # class.
  module OrgFileHtmlizer
    private

    # Format {Neruda::OrgFile#keywords} list in an HTML listing.
    #
    # @return [String] the HTML keywords list
    def keywords_to_html
      klist = @keywords.map do |k|
        <<~KEYWORDLINK
          <li class="keyword">
            <a href="/tags/#{Neruda::OrgFile.slug(k)}.html">#{k}</a>
          </li>
        KEYWORDLINK
      end.join
      "<ul class=\"keywords-list\">#{klist}</ul>"
    end

    # Format {Neruda::OrgFile#date} as a HTML `time` tag.
    #
    # @return [String] the HTML `time` tag
    def date_to_html(dateformat = :full)
      return '' if @date.nil?
      "<time datetime=\"#{@date.rfc3339}\">#{datestring(dateformat)}</time>"
    end

    # Format {Neruda::OrgFile#author} in a HTML `span` tag with a
    #   specific class.
    #
    # @return [String] the author HTML `span`
    def author_to_html
      return '' if @author == ''
      "<span class=\"author\">#{@author}</span>"
    end
  end
end
