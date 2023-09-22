# frozen_string_literal: true

using TimePatch

module Fronde
  # This module holds extracter methods for the {Fronde::OrgFile} class.
  module OrgFileExtracter
    private

    # Main method, which will call the other to initialize an
    #   {Fronde::OrgFile} instance.
    def extract_data
      @data = { content: File.read(@file), pub_file: nil, url: nil }
      %i[title subtitle date author keywords lang excerpt].each do |param|
        @data[param] = send("extract_#{param}".to_sym)
      end
      return unless @project

      @data[:updated] = File.mtime(@file)
      @data[:pub_file] = @project.target_for @file
      @data[:url] = "#{CONFIG.get('domain')}/#{@data[:pub_file]}"
    end

    def extract_date
      timerx = '([0-9:]{5})(?::([0-9]{2}))?'
      daterx = /^#\+date: *<([0-9-]{10}) [\w.]+(?: #{timerx})?> *$/i
      match = daterx.match(@data[:content])
      return NilTime.new if match.nil?

      notime = match[2].nil?
      if notime
        time = '00:00:00'
      else
        time = "#{match[2]}:#{match[3] || '00'}"
      end
      date = Time.strptime("#{match[1]} #{time}", '%Y-%m-%d %H:%M:%S')
      date.no_time = notime
      date
    end

    def extract_title
      match = /^#\+title:(.+)$/i.match(@data[:content])
      if match.nil?
        # Avoid to leak absolute path
        project_relative_path = @file.sub(/^#{Dir.pwd}\//, '')
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
      (match && match[1].strip) || CONFIG.get('author')
    end

    def extract_keywords
      match = /^#\+keywords:(.+)$/i.match(@data[:content])
      (match && match[1].split(',').map(&:strip)) || []
    end

    def extract_lang
      match = /^#\+language:(.+)$/i.match(@data[:content])
      (match && match[1].strip) || CONFIG.get('lang')
    end

    def extract_excerpt
      @data[:content].scan(/^#\+description:(.+)$/i).map do |line|
        line.first.strip
      end.join(' ')
    end
  end
end
