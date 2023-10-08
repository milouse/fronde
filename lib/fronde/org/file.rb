# frozen_string_literal: true

require 'time'
require_relative '../../ext/nil_time'
require_relative '../../ext/time'
using TimePatch

require 'nokogiri'
require 'fileutils'

require_relative '../config'
require_relative '../index'
require_relative '../version'
require_relative 'file_extracter'

module Fronde
  module Org
    # Handles org files.
    #
    # This class is responsible for reading or writing existing or new
    # org files, and formating their content to be used on the generated
    # website.
    class File
      # @return [String] the relative path to the source of this
      #   document.
      attr_reader :file

      # @return [Hash] the project owning this document.
      attr_reader :project

      include FileExtracter

      # Prepares the file named by ~file_name~ for read and write
      #   operations.
      #
      # If the file ~file_name~ does not exist, the new instance may be
      # populated by data given in the ~opts~ parameter.
      #
      # @example
      #     File.exist? './test.org'
      #     => true
      #     o = Fronde::Org::File.new('./test.org')
      #     => #<Fronde::Org::File @file='./test.org'...>
      #     o.title
      #     => "This is an existing test file"
      #     File.exist? '/tmp/does_not_exist.org'
      #     => false
      #     o = Fronde::Org::File.new('/tmp/does_not_exist.org')
      #     => #<Fronde::Org::File @file='/tmp/does_not_exist.org'...>
      #     o.title
      #     => ""
      #     File.exist? '/tmp/other.org'
      #     => false
      #     o = Fronde::Org::File.new('/tmp/other.org', title: 'New file')
      #     => #<Fronde::Org::File @file='/tmp/other.org'...>
      #     o.title
      #     => "New file"
      #
      # @param file_name [String] path to the corresponding Org file
      # @param opts [Hash] optional data to initialize new Org file
      # @option opts [String] title ('') the title of the new Org file
      # @option opts [String] author (system user or '') the author of
      #   the document
      # @return [Fronde::Org::File] the new instance of
      #   Fronde::Org::File
      def initialize(file_name, opts = {})
        file_name ||= ''
        @file = ::File.expand_path file_name
        @options = opts
        @project = find_source
        @data = {}
        if ::File.file?(@file)
          extract_data
        else
          init_empty_file
        end
      end

      # Returns a String representation of the document date, which aims
      #   to be used to sort several Org::Files.
      #
      # The format used for the key is ~%Y%m%d%H%M%S~. If the current
      # Org::File instance does not have a date, this mehod return
      # ~00000000000000~. If the current Org::File instance does not
      # have time information, the date is padded with zeros.
      #
      # @example with the org header ~#+date: <2019-07-03 Wed 20:52:49>~
      #     org_file.date
      #     => #<Time: 2019-07-03T20:52:49+02:00...>
      #     org_file.timekey
      #     => "20190703205349"
      #
      # @example with the org header ~#+date: <2019-07-03 Wed>~
      #     org_file.date
      #     => #<Time: 2019-07-03T00:00:00+02:00...>
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
        return '00000000000000' if @data[:date].is_a? NilTime

        @data[:date].strftime('%Y%m%d%H%M%S')
      end

      # Formats given ~string~ with values of the current Org::File.
      #
      # This method expects to find percent-tags in the given ~string~
      # and replace them by their corresponding value.
      #
      # It reuses the same tags than the ~org-html-format-spec~ method.
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
      # - %I :: the raw ~:iso8601~ date and time
      # - %k :: the keywords separated by a comma
      # - %K :: the HTML list rendering of the keywords
      # - %l :: the lang of the document
      # - %L :: the license information, taken from the
      #         {Fronde::Config#settings}
      # - %n :: the Fronde name and version
      # - %N :: the Fronde name and version with a link to the project
      #         home on the name
      # - %s :: the subtitle of the document
      # - %t :: the title of the document
      # - %u :: the URL to the related published HTML document
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
        string.gsub('%a', @data[:author])
              .gsub('%A', "<span class=\"author\">#{@data[:author]}</span>")
              .gsub('%d', @data[:date].l18n_short_date_html)
              .gsub('%D', @data[:date].l18n_long_date_html)
              .gsub('%i', @data[:date].l18n_short_date_string)
              .gsub('%I', @data[:date].xmlschema)
              .gsub('%k', @data[:keywords].join(', '))
              .gsub('%K', keywords_to_html)
              .gsub('%l', @data[:lang])
              .gsub('%L', Fronde::CONFIG.get('license', '').gsub(/\s+/, ' ').strip)
              .gsub('%n', "Fronde #{Fronde::VERSION}")
              .gsub('%N', "<a href=\"https://git.umaneti.net/fronde/about/\">Fronde</a> #{Fronde::VERSION}")
              .gsub('%s', @data[:subtitle])
              .gsub('%t', @data[:title])
              .gsub('%u', @data[:url] || '')
              .gsub('%x', @data[:excerpt])
              .gsub('%X', "<p>#{@data[:excerpt]}</p>")
      end
      # rubocop:enable Layout/LineLength
      # rubocop:enable Metrics/MethodLength

      # Writes the current Org::File content to the underlying file.
      #
      # The intermediate parent folders are created if necessary.
      #
      # @return [Integer] the length written (as returned by the
      #   underlying ~File.write~ method call)
      def write
        if ::File.directory? @file
          if @data[:title] == ''
            raise R18n.t.fronde.error.org_file.no_file_or_title
          end
          @file = ::File.join @file, "#{Slug.slug(@data[:title])}.org"
        else
          file_dir = ::File.dirname @file
          FileUtils.mkdir_p file_dir
        end
        ::File.write @file, @data[:content]
      end

      def method_missing(method_name, *args, &block)
        reader_method = method_name.to_s.delete_suffix('=').to_sym
        if @data.has_key? reader_method
          return @data[reader_method] if reader_method == method_name

          return @data[reader_method] = args.first
        end
        super
      end

      def respond_to_missing?(method_name, include_private = false)
        return true if @data.has_key? method_name

        reader_method = method_name.to_s.delete_suffix('=').to_sym
        return true if @data.has_key? reader_method

        super
      end

      def to_h
        fields = %w[author excerpt keywords timekey title url]
        data = fields.to_h { |key| [key, send(key)] }
        data['mime_type'] = @project['mime_type']
        data['html_body'] = extract_html_body
        pub_date = @data[:date]
        data['published'] = pub_date.l18n_long_date_string(with_year: false)
        data['published_xml'] = pub_date.xmlschema
        data['updated_xml'] = @data[:updated].xmlschema
        data
      end

      private

      def find_source
        if ::File.extname(@file) == '.org'
          source = find_source_for_org_file
        else
          source = find_source_for_publication_file
        end
        warn R18n.t.fronde.error.org_file.no_project(file: @file) unless source
        source
      end

      def find_source_for_org_file
        Fronde::CONFIG.sources.find do |project|
          project.source_for? @file
        end
      end

      def find_source_for_publication_file
        Fronde::CONFIG.sources.find do |project|
          org_file = project.source_for @file
          next unless org_file

          @file = org_file
        end
      end

      def init_empty_file
        @data = {
          title: @options[:title] || '',
          subtitle: '',
          date: Time.now,
          author: @options[:author] || Fronde::CONFIG.get('author'),
          keywords: [],
          lang: @options[:lang] || Fronde::CONFIG.get('lang'),
          excerpt: '',
          pub_file: nil,
          url: nil
        }
        body = @options[:content] || ''
        @data[:content] = @options[:raw_content] || <<~ORG
          #+title: #{@data[:title]}
          #+date: <#{@data[:date].strftime('%Y-%m-%d %a. %H:%M:%S')}>
          #+author: #{@data[:author]}
          #+language: #{@data[:lang]}

          #{body}
        ORG
      end

      # Format {Fronde::Org::File#keywords} list in an HTML listing.
      #
      # @return [String] the HTML keywords list
      def keywords_to_html
        domain = Fronde::CONFIG.get('domain')
        klist = @data[:keywords].map do |k|
          %(<li class="keyword">
            <a href="#{domain}/tags/#{Slug.slug(k)}.html">#{k}</a>
              </li>)
        end.join
        %(<ul class="keywords-list">#{klist}</ul>)
      end
    end
  end
end
