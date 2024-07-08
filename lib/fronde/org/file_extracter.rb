# frozen_string_literal: true

using TimePatch

require_relative '../../ext/time_no_time'

module Fronde
  module Org
    # This module holds extracter methods for the {Fronde::Org::File}
    #   class.
    module FileExtracter
      private

      # Main method, which will call the other to initialize an
      #   {Fronde::Org::File} instance.
      def extract_data
        @data = { content: ::File.read(@file), pub_file: nil, url: nil }
        %i[title subtitle date author keywords lang excerpt].each do |param|
          @data[param] = send(:"extract_#{param}")
        end
        return unless @project

        @data[:updated] = ::File.mtime(@file)
        @data[:pub_file] = @project.target_for @file
        @data[:url] = Fronde::CONFIG.get('domain') + @data[:pub_file]
      end

      def extract_date
        timerx = '([0-9:]{5})(?::([0-9]{2}))?'
        daterx = /^#\+date: *<([0-9-]{10}) [\w.]+(?: #{timerx})?> *$/i
        match = daterx.match(@data[:content])
        return NilTime.new if match.nil?

        return TimeNoTime.parse_no_time(match[1]) if match[2].nil?

        Time.strptime(
          "#{match[1]} #{match[2]}:#{match[3] || '00'}",
          '%Y-%m-%d %H:%M:%S'
        )
      end

      def extract_title
        match = /^#\+title:(.+)$/i.match(@data[:content])
        if match.nil?
          # Avoid to leak absolute path
          project_relative_path = @file.sub %r{^#{Dir.pwd}/}, ''
          return project_relative_path
        end
        match[1].strip
      end

      def extract_subtitle
        match = /^#\+subtitle:(.+)$/i.match(@data[:content])
        (match && match[1].strip) || ''
      end

      def extract_author
        match = /^#\+author:(.+)$/i.match(@data[:content])
        (match && match[1].strip) || Fronde::CONFIG.get('author')
      end

      def extract_keywords
        match = /^#\+keywords:(.+)$/i.match(@data[:content])
        (match && match[1].split(',').map(&:strip)) || []
      end

      def extract_lang
        match = /^#\+language:(.+)$/i.match(@data[:content])
        (match && match[1].strip) || Fronde::CONFIG.get('lang')
      end

      def extract_excerpt
        @data[:content].scan(/^#\+description:(.+)$/i).map do |line|
          line.first.strip
        end.join(' ')
      end

      def extract_published_body
        pub_file = @data[:pub_file]
        # Always return something, even when not published yet
        return @data[:excerpt] unless pub_file && @project

        project_type = @project.type
        pub_folder = Fronde::CONFIG.get("#{project_type}_public_folder")
        file_name = pub_folder + pub_file
        return @data[:excerpt] unless ::File.exist? file_name

        return ::File.read(file_name) if project_type == 'gemini'

        read_html_body file_name
      end

      def read_html_body(file_name)
        dom = ::File.open(file_name, 'r') { |file| Nokogiri::HTML file }
        body = dom.css('div#content')
        body.css('header').unlink # Remove the main title
        body.to_s
      end
    end
  end
end
