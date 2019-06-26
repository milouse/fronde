# frozen_string_literal: true

require 'time'
require 'neruda/config'
require 'neruda/index'

# Eases org files handling and decoration
module Neruda
  class OrgFile
    attr_reader :title, :date, :author, :keywords, :lang,
                :file, :html_file, :url, :excerpt

    def initialize(file_name, opts = {})
      @file = file_name
      @html_file = Neruda::OrgFile.html_file(@file)
      @url = Neruda::OrgFile.html_file_with_domain(@file)
      if File.exist?(@file)
        extract_data
      else
        init_empty_file(opts)
      end
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
            .gsub('%x', @excerpt)
            .gsub('%X', "<p>#{@excerpt}</p>")
    end

    def write
      IO.write @file, @content
    end

    def slug
      Neruda::OrgFile.slug(@title)
    end

    class << self
      def html_file(file_name)
        path = Neruda::OrgFile.target_for_source(file_name)
        pubfolder = Neruda::Config.settings['public_folder']
        path.sub(/^#{pubfolder}\//, '/')
      end

      def html_file_with_domain(file_name)
        Neruda::Config.settings['domain'] + html_file(file_name)
      end

      def source_for_target(file_name)
        # file_name may be frozen...
        src = file_name.sub(/\.html$/, '.org')
        pubfolder = Neruda::Config.settings['public_folder']
        src.sub!(/^#{pubfolder}\//, 'src/')
        blogpath = Neruda::Config.settings['blog_path']
        return src unless /\/#{blogpath}\//.match?(src)
        src.sub(/\/index\.org$/, '/content.org')
      end

      def target_for_source(file_name)
        # file_name may be frozen...
        target = file_name.sub(/\.org$/, '.html')
        pubfolder = Neruda::Config.settings['public_folder']
        return target.sub(/^src\//, "#{pubfolder}/") if /^src\//.match?(target)
        subfolder = File.basename(File.dirname(target))
        leaf = File.basename(target)
        "#{pubfolder}/#{subfolder}/#{leaf}"
      end

      def file_name(title, for_blog = false)
        title = 'new' if title.nil? || title == ''
        filename = Neruda::OrgFile.slug title
        return "src/#{filename}.org" unless for_blog
        blog_path = Neruda::Config.settings['blog_path']
        "src/#{blog_path}/#{filename}/content.org"
      end

      def slug(title)
        title.downcase.gsub(' ', '-')
             .encode('ascii', fallback: ->(k) { translit(k) })
             .gsub(/[^\w-]/, '').gsub(/-$/, '')
      end

      private

      def translit(char)
        return 'a' if ['á', 'à', 'â', 'ä', 'ǎ', 'ã', 'å'].include?(char)
        return 'e' if ['é', 'è', 'ê', 'ë', 'ě', 'ẽ'].include?(char)
        return 'i' if ['í', 'ì', 'î', 'ï', 'ǐ', 'ĩ'].include?(char)
        return 'o' if ['ó', 'ò', 'ô', 'ö', 'ǒ', 'õ'].include?(char)
        return 'u' if ['ú', 'ù', 'û', 'ü', 'ǔ', 'ũ'].include?(char)
        return 'y' if ['ý', 'ỳ', 'ŷ', 'ÿ', 'ỹ'].include?(char)
        return 'c' if char == 'ç'
        return 'n' if char == 'ñ'
        '-'
      end
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
