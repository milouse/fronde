# frozen_string_literal: true

require 'neruda/config'
require 'neruda/emacs'

module Neruda
  # This module holds HTML formatter methods for the {Neruda::OrgFile}
  # class.
  module OrgFileHtmlizer
    # Publish the current file
    #
    # @return [Boolean, nil] the underlying ~system~ method return value
    def publish
      Neruda::Emacs.new(
        file_path: @file, verbose: @options[:verbose]
      ).publish
    end

    private

    # Format {Neruda::OrgFile#keywords} list in an HTML listing.
    #
    # @return [String] the HTML keywords list
    def keywords_to_html
      domain = Neruda::Config.settings['domain']
      klist = @keywords.map do |k|
        <<~KEYWORDLINK
          <li class="keyword">
            <a href="#{domain}/tags/#{Neruda::OrgFile.slug(k)}.html">#{k}</a>
          </li>
        KEYWORDLINK
      end.join
      "<ul class=\"keywords-list\">#{klist}</ul>"
    end

    # Format {Neruda::OrgFile#date} as a HTML `time` tag.
    #
    # @return [String] the HTML `time` tag
    def date_to_html(dateformat = :full)
      return '<time></time>' if @date.nil?
      "<time datetime=\"#{@date.rfc3339}\">#{datestring(dateformat)}</time>"
    end

    # Format {Neruda::OrgFile#author} in a HTML `span` tag with a
    #   specific class.
    #
    # @return [String] the author HTML `span`
    def author_to_html
      "<span class=\"author\">#{@author}</span>"
    end
  end
end
