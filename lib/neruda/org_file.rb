# frozen_string_literal: true

require 'time'
require 'neruda/config'
require 'neruda/index'

# Eases org files handling and decoration
module Neruda
  class OrgFile
    attr_reader :title, :date, :keywords, :lang, :local_links

    def initialize(file_name)
      @file = file_name
      @content = File.open(file_name, 'r').read
      @title = extract_title
      @date = extract_date
      @keywords = extract_keywords
      @lang = extract_lang
      @local_links = extract_relative_links
    end

    def timekey
      return '00000000000000' if @date.nil?
      @date.strftime('%Y%m%d%H%M%S')
    end

    def timestring(dateformat = :full)
      return '' if @date.nil?
      return R18n.l @date.to_date if dateformat == :short
      return @date.rfc3339 if dateformat == :rfc3339
      dateformat == :full if dateformat == :long
      R18n.l @date, :full
    end

    def datestring
      timestring(:short)
    end

    def format(string)
      string.gsub('%k', @keywords.join(', '))
        .gsub('%K', keywords_to_html)
        .gsub('%d', date_to_html(:short))
        .gsub('%D', date_to_html)
        .gsub('%i', timestring(:rfc3339))
        .gsub('%a', Neruda::Config.settings['author'])
        .gsub('%A', author_to_html)
        .gsub('%t', @title)
        .gsub('%l', @lang)
    end

    private

    def extract_date
      begin
        m = /^#\+date: <([0-9-]{10}) \w+ ([0-9:]{8})>$/.match(@content)
      rescue ArgumentError
        warn "Error retrieving date for #{@file}"
        m = nil
      end
      return nil if m.nil?
      DateTime.strptime("#{m[1]} #{m[2]}", '%Y-%m-%d %H:%M:%S')
    end

    def extract_title
      begin
        m = /^#\+title: (.+)$/.match(@content)
      rescue ArgumentError
        warn "Error retrieving title for #{@file}"
        m = nil
      end
      return @file if m.nil?
      m[1]
    end

    def extract_keywords
      begin
        m = /^#\+keywords: (.+)$/.match(@content)
      rescue ArgumentError
        warn "Error retrieving keywords for #{@file}"
        m = nil
      end
      return [] if m.nil?
      m[1].split(',').map(&:strip)
    end

    def extract_lang
      begin
        m = /^#\+language: (.+)$/.match(@content)
      rescue ArgumentError
        warn "Error retrieving lang for #{@file}"
        m = nil
      end
      return (Neruda::Config.settings['lang'] || 'en') if m.nil?
      m[1]
    end

    def extract_relative_links
      files = []
      path = File.dirname(@file)
      @content.scan(/\[\[file:(?:\.\/)?([^\]]+)\]/).each do |m|
        files << m[0] if File.exist? "#{path}/#{m[0]}"
      end
      files
    end

    def keywords_to_html
      klist = @keywords.map do |k|
        <<~KEYWORDLINK
        <li class="keyword">
            <a href="../#{Index.slug(k)}.html">#{k}</a>
        </li>
      KEYWORDLINK
      end.join
      "<ul class=\"keywords-list\">#{klist}</ul>"
    end

    def date_to_html(dateformat = :full)
      return '' if @date.nil?
      "<time datetime=\"#{@date.rfc3339}\">#{timestring(dateformat)}</time>"
    end

    def author_to_html
      "<span class=\"author\">#{Neruda::Config.settings['author']}</span>"
    end
  end
end
