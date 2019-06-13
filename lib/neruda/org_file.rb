# frozen_string_literal: true

require 'time'
require 'neruda/config'
require 'neruda/index'

# Eases org files handling and decoration
module Neruda
  class OrgFile
    attr_reader :title, :date, :author, :keywords,
                :lang, :local_links, :file, :html_file

    def initialize(file_name)
      @file = file_name
      @html_file = Neruda::OrgFile.html_file_with_domain(@file)
      @content = File.open(file_name, 'r').read
      @title = extract_title
      @date = extract_date
      @author = extract_author
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
            .gsub('%i', timestring(:short))
            .gsub('%I', timestring(:rfc3339))
            .gsub('%a', @author)
            .gsub('%A', author_to_html)
            .gsub('%t', @title)
            .gsub('%l', @lang)
            .gsub('%u', @html_file)
    end

    class << self
      def html_file_with_domain(file_name)
        domain = Neruda::Config.settings['domain']
        pubfolder = Neruda::Config.settings['public_folder']
        path = Neruda::OrgFile.target_for_source(file_name)
        domain + path.gsub(/^#{pubfolder}\//, '/')
      end

      def source_for_target(file_name)
        file_name.sub!(/\.html$/, '.org')
        pubfolder = Neruda::Config.settings['public_folder']
        file_name.sub!(/^#{pubfolder}\//, 'src/')
        blogpath = Neruda::Config.settings['blog_path']
        return file_name unless /\/#{blogpath}\//.match?(file_name)
        file_name.sub(/\/index\.org$/, '/content.org')
        puts 'trolol', file_name
        file_name
      end

      def target_for_source(file_name)
        file_name = file_name.sub(/\.org$/, '.html')
        pubfolder = Neruda::Config.settings['public_folder']
        if /^src\//.match?(file_name)
          file_name.sub!(/^src\//, "#{pubfolder}/")
        else
          subfolder = File.basename(File.dirname(file_name))
          leaf = File.basename(file_name)
          file_name = "#{pubfolder}/#{subfolder}/#{leaf}"
        end
        blogpath = Neruda::Config.settings['blog_path']
        return file_name unless /\/#{blogpath}\//.match?(file_name)
        file_name.sub(/\/content\.html$/, '/index.html')
      end

      def expand_sources_list(rake_list)
        return [] unless rake_list
        conf = Neruda::Config.settings
        unless conf['external_sources'].nil?
          rake_list = rake_list.include(conf['external_sources'])
        end
        (conf['exclude_sources'] || []).each do |x|
          rake_list = rake_list.exclude Regexp.new(x)
        end
        rake_list.map { |s| Neruda::OrgFile.target_for_source(s) }
      end
    end

    private

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

    def extract_author
      m = /^#\+author:(.+)$/i.match(@content)
      return Neruda::Config.settings['author'] || '' if m.nil?
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
            <a href="../#{Neruda::Index.slug(k)}.html">#{k}</a>
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
      return '' if @author == ''
      "<span class=\"author\">#{@author}</span>"
    end
  end
end
