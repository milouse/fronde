# frozen_string_literal: true

require 'time'
require 'fileutils'
# neruda/config is required by htmlizer
require 'neruda/org_file/htmlizer'
require 'neruda/org_file/extracter'
require 'neruda/org_file/class_methods'
require 'neruda/index'
require 'neruda/version'

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

    # @return [String] the subtitle of the current org document, taken
    #   from the ~#+subtitle:~ header.
    attr_reader :subtitle

    # @return [DateTime] the date and time of the current org document,
    #   taken from the ~#+date:~ header.
    attr_reader :date

    # @return [Boolean] wether a time has been extracted from the
    #   current org document ~#+date:~ header.
    attr_reader :notime

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

    # @return [String] the description of this org document, taken from
    #   the ~#+description:~ header.
    attr_reader :excerpt

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

    # @return [String] the project owning this document.
    attr_reader :project

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
    # @param file_name [String] path to the corresponding Org file
    # @param opts [Hash] optional data to initialize new Org file
    # @option opts [String] title ('') the title of the new Org file
    # @option opts [String] author (system user or '') the author of the
    #   document
    # @option opts [Boolean] verbose (false) if the
    #   {Neruda::OrgFileHtmlizer#publish publish} method should output
    #   emacs process messages
    # @option opts [String] project the project owning this file
    #   must be stored
    # @return [Neruda::OrgFile] the new instance of Neruda::OrgFile
    def initialize(file_name, opts = {})
      file_name = nil if file_name == ''
      @file = file_name
      @html_file = nil
      @url = nil
      @project = opts.delete :project
      @options = opts
      build_html_file_and_url
      if @file && File.exist?(@file)
        extract_data
      else
        init_empty_file
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
    # @param year [Boolean] wether or not the ~:full~ format must
    #   contain the year
    # @return [String] the document DateTime string representation
    def datestring(dateformat = :full, year: true)
      return '' if @date.nil?
      return R18n.l @date.to_date if dateformat == :short
      return @date.rfc3339 if dateformat == :rfc3339
      locale = R18n.get.locale
      long_fmt = R18n.t.neruda.index.full_date_format(
        date: locale.format_date_full(@date, year)
      )
      unless @notime
        long_fmt = R18n.t.neruda.index.full_date_with_time_format(
          date: long_fmt, time: locale.time_format.delete('_').strip
        )
      end
      locale.strftime(@date, long_fmt)
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
    # - %n :: the Neruda name and version
    # - %N :: the Neruda name and version with a link to the project
    #         home on the name
    # - %s :: the subtitle of the document
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
    # rubocop:disable Metrics/MethodLength
    # rubocop:disable Layout/LineLength
    def format(string)
      string.gsub('%a', @author)
            .gsub('%A', author_to_html)
            .gsub('%d', date_to_html(:short))
            .gsub('%D', date_to_html)
            .gsub('%i', datestring(:short))
            .gsub('%I', datestring(:rfc3339))
            .gsub('%k', @keywords.join(', '))
            .gsub('%K', keywords_to_html)
            .gsub('%l', @lang)
            .gsub('%L', (Neruda::Config.settings['license'] || '').gsub(/\s+/, ' ').strip)
            .gsub('%n', "Neruda #{Neruda::VERSION}")
            .gsub('%N', "<a href=\"https://git.umaneti.net/neruda/about/\">Neruda</a> #{Neruda::VERSION}")
            .gsub('%s', @subtitle)
            .gsub('%t', @title)
            .gsub('%u', @html_file || '')
            .gsub('%x', @excerpt)
            .gsub('%X', "<p>#{@excerpt}</p>")
    end
    # rubocop:enable Layout/LineLength
    # rubocop:enable Metrics/MethodLength

    # Writes the current OrgFile content to the underlying file.
    #
    # The intermediate parent folders are created if necessary.
    #
    # @return [Integer] the length written (as returned by the
    #   underlying ~IO.write~ method call)
    def write
      raise TypeError, 'no conversion from nil file name to path.' if @file.nil?
      file_dir = File.dirname @file
      FileUtils.mkdir_p file_dir unless Dir.exist? file_dir
      IO.write @file, @content
    end

    private

    def build_html_file_and_url
      return if @file.nil?
      @html_file = Neruda::OrgFile.target_for_source(
        @file, @project, with_public_folder: false
      )
      @url = "#{Neruda::Config.settings['domain']}/#{@html_file}"
    end

    def init_empty_file
      @title = @options[:title] || ''
      @subtitle = ''
      @date = DateTime.now
      @notime = false
      @author = @options[:author] || Neruda::Config.settings['author']
      @keywords = []
      @lang = @options[:lang] || Neruda::Config.settings['lang']
      @excerpt = ''
      body = @options[:content] || ''
      @content = @options[:raw_content] || <<~ORG
        #+title: #{@title}
        #+date: <#{@date.strftime('%Y-%m-%d %a. %H:%M:%S')}>
        #+author: #{@author}
        #+language: #{@lang}

        #{body}
      ORG
    end
  end
end
