# frozen_string_literal: true

module Neruda
  # This module holds extracter methods for the {Neruda::OrgFile} class.
  module OrgFileExtracter
    private

    # Main method, which will call the other to initialize an
    #   {Neruda::OrgFile} instance.
    def extract_data
      @content = IO.read @file
      @title = extract_title
      @date = extract_date
      @author = extract_author
      @keywords = extract_keywords
      @lang = extract_lang
      @excerpt = extract_excerpt
    end

    def extract_date
      m = /^#\+date: *<([0-9-]{10}) [\w.]+(?: ([0-9:]{8}))?> *$/i
          .match(@content)
      return nil if m.nil?
      time = m[2] || '00:00:00'
      DateTime.strptime("#{m[1]} #{time}", '%Y-%m-%d %H:%M:%S')
    end

    def extract_title
      m = /^#\+title:(.+)$/i.match(@content)
      return @file if m.nil?
      m[1].strip
    end

    def default_author
      Neruda::Config.settings['author'] || ENV['USER'] || ''
    end

    def extract_author
      m = /^#\+author:(.+)$/i.match(@content)
      return default_author if m.nil?
      m[1].strip
    end

    def extract_keywords
      m = /^#\+keywords:(.+)$/i.match(@content)
      return [] if m.nil?
      m[1].split(',').map(&:strip)
    end

    def extract_lang
      m = /^#\+language:(.+)$/i.match(@content)
      return Neruda::Config.settings['lang'] if m.nil?
      m[1].strip
    end

    def extract_excerpt
      @content.scan(/^#\+description:(.+)$/i).map { |l| l[0].strip }.join(' ')
    end
  end
end
