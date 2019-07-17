# frozen_string_literal: true

require 'time'
require 'fileutils'
require 'neruda/config'
require 'neruda/index'
require 'neruda/org_file/class_methods'
require 'neruda/org_file/extracter'
require 'neruda/org_file/htmlizer'

module Neruda
  # Handles org files.
  #
  # This class is responsible for reading or writing existing or new org
  # files, and formating their content to be used on the generated
  # website.
  class OrgFile
    # @return [String] the title of the current org document, taken from
    #   the ~#+title:~ header.
    attr_reader :title

    # @return [DateTime] the date and time of the current org document,
    #   taken from the ~#+date:~ header.
    attr_reader :date

    # The author of the current org document, taken from the ~#+author:~
    #   header.
    #
    # If the current document doesn't have any authorship information,
    # the one from the ~config.yml~ file will be used instead
    #
    # @return [String] the author name
    attr_reader :author

    # @return [Array] the keywords list of the current org document,
    #   taken from the ~#+keywords:~ header.
    attr_reader :keywords

    # The locale of the current org document, taken from the
    #   ~#+language:~ header.
    #
    # If the current document doesn't have any language information, the
    # one from the ~config.yml~ file will be used instead, or "en" by
    # default.
    #
    # @return [String] the document lang
    attr_reader :lang

    # @return [String] the relative path to the source of this document.
    attr_reader :file

    # @return [String] the relative path to the generated html file of
    #   this document.
    attr_reader :html_file

    # @return [String] the url of this document, build from the ~domain~
    #   settings and the above {#html_file @html_file} attribute.
    attr_reader :url

    # @return [String] the description of this org document, taken from
    #   the ~#+description:~ header.
    attr_reader :excerpt

    extend Neruda::OrgFileClassMethods

    include Neruda::OrgFileExtracter
    include Neruda::OrgFileHtmlizer

    # Prepares the file named by ~file_name~ for read and write
    #   operations.
    #
    # If the file ~file_name~ does not exist, the new instance may be
    # populated by data given in the ~opts~ parameter.
    #
    # @example
    #     File.exist? './test.org'
    #     => true
    #     o = Neruda::OrgFile.new('./test.org')
    #     => #<Neruda::OrgFile @file='./test.org'...>
    #     o.title
    #     => "This is an existing test file"
    #     File.exist? '/tmp/does_not_exist.org'
    #     => false
    #     o = Neruda::OrgFile.new('/tmp/does_not_exist.org')
    #     => #<Neruda::OrgFile @file='/tmp/does_not_exist.org'...>
    #     o.title
    #     => ""
    #     File.exist? '/tmp/other.org'
    #     => false
    #     o = Neruda::OrgFile.new('/tmp/other.org', title: 'New file')
    #     => #<Neruda::OrgFile @file='/tmp/other.org'...>
    #     o.title
    #     => "New file"
    #
    # @param file_name [String] path to the corresponding org mode file
    # @param opts [Hash] optional data to initialize new org file
    # @option opts [String] title ('') the title of the new org file
    # @option opts [String] author (system user or '') the author of the
    #   document
    # @return [Neruda::OrgFile] the new instance of Neruda::OrgFile
    def initialize(file_name, opts = {})
      if file_name.nil? || file_name == ''
        raise ArgumentError, 'file_name is nil'
      end
      @file = file_name
      @html_file = Neruda::OrgFile.html_file(@file)
      @url = Neruda::OrgFile.html_file_with_domain(@file)
      if File.exist?(@file)
        extract_data
      else
        init_empty_file(opts)
      end
    end

    # Returns a String representation of the document date, which aims
    #   to be used to sort several OrgFiles.
    #
    # The format used for the key is ~%Y%m%d%H%M%S~. If the current
    # OrgFile instance does not have a date, this mehod return
    # ~00000000000000~. If the current OrgFile instance does not have
    # time information, the date is padded with zeros.
    #
    # @example with the org header ~#+date: <2019-07-03 Wed 20:52:49>~
    #     org_file.date
    #     => #<DateTime: 2019-07-03T20:52:49+02:00...>
    #     org_file.timekey
    #     => "20190703205349"
    #
    # @example with the org header ~#+date: <2019-07-03 Wed>~
    #     org_file.date
    #     => #<DateTime: 2019-07-03T00:00:00+02:00...>
    #     org_file.timekey
    #     => "20190703000000"
    #
    # @example with no date header in the org file
    #     org_file.date
    #     => nil
    #     org_file.timekey
    #     => "00000000000000"
    #
    # @return [String] the document key
    def timekey
      return '00000000000000' if @date.nil?
      @date.strftime('%Y%m%d%H%M%S')
    end

    # Returns the current OrgFile instance DateTime as a String.
    #
    # This method accepts three values for the ~dateformat~ parameter:
    #
    # - ~:full~ (or ~:long~) :: outputs a complete date and time
    #   representation, localized through R18n;
    # - ~:short~ :: outputs a short date representation (without time),
    #   localized with R18n;
    # - ~:rfc3339~ :: outputs the RFC 3339 date and time representation,
    #   used in atom feed.
    #
    # @param dateformat [Symbol] the format to use to convert DateTime
    #   into String
    # @return [String] the document DateTime string representation
    def datestring(dateformat = :full)
      return '' if @date.nil?
      return R18n.l @date.to_date if dateformat == :short
      if dateformat == :human
        if @date.strftime('%H%M%S') == '000000'
          return R18n.l @date.to_date, :human
        end
        return R18n.l @date, :human
      end
      return @date.rfc3339 if dateformat == :rfc3339
      dateformat == :full if dateformat == :long
      R18n.l @date, :full
    end

    # Formats given ~string~ with values of the current OrgFile.
    #
    # This method expects to find percent-tags in the given ~string~ and
    # replace them by their corresponding value.
    #
    # *** Format:
    #
    # - %a :: the raw author name
    # - %A :: the HTML rendering of the author name, equivalent to
    #         ~<span class="author">%a</span>~
    # - %d :: the ~:short~ date HTML representation, equivalent
    #         to ~<time datetime="%I">%i</time>~
    # - %D :: the ~:full~ date and time HTML representation
    # - %i :: the raw ~:short~ date and time
    # - %I :: the raw ~:rfc3339~ date and time
    # - %k :: the keywords separated by a comma
    # - %K :: the HTML list rendering of the keywords
    # - %l :: the lang of the document
    # - %L :: the license information, taken from the
    #         {Neruda::Config#settings}
    # - %t :: the title of the document
    # - %u :: the web path to the related published HTML document
    # - %x :: the raw description (eXcerpt)
    # - %X :: the description, enclosed in an HTML ~p~ tag, equivalent
    #         to ~<p>%x</p>~
    #
    # @example
    #     org_file.format("Article written by %a the %d")
    #     => "Article written by Alice Smith the Wednesday 3rd July"
    #
    # @return [String] the given ~string~ after replacement occurs
    def format(string)
      license = Neruda::Config.settings['license'] || ''
      string.gsub('%a', @author)
            .gsub('%A', author_to_html)
            .gsub('%d', date_to_html(:short))
            .gsub('%D', date_to_html)
            .gsub('%i', datestring(:short))
            .gsub('%I', datestring(:rfc3339))
            .gsub('%k', @keywords.join(', '))
            .gsub('%K', keywords_to_html)
            .gsub('%l', @lang)
            .gsub('%L', license.gsub(/\s+/, ' ').strip)
            .gsub('%t', @title)
            .gsub('%u', @html_file)
            .gsub('%x', @excerpt)
            .gsub('%X', "<p>#{@excerpt}</p>")
    end

    # Writes the current OrgFile content to the underlying file.
    #
    # The intermediate parent folders are created if necessary.
    #
    # @return [Integer] the length written (as returned by the
    #   underlying ~IO.write~ method call)
    def write
      file_dir = File.dirname @file
      FileUtils.mkdir_p file_dir unless Dir.exist? file_dir
      IO.write @file, @content
    end

    private

    def init_empty_file(opts)
      @title = opts[:title] || ''
      @date = DateTime.now
      @author = opts[:author] || default_author
      @keywords = []
      @lang = Neruda::Config.settings['lang']
      @excerpt = ''
      @content = <<~ORG
        #+title: #{@title}
        #+date: <#{@date.strftime('%Y-%m-%d %a. %H:%M:%S')}>
        #+author: #{@author}
        #+language: #{@lang}

      ORG
    end
  end
end
