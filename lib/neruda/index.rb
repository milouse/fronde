# coding: utf-8
# frozen_string_literal: true

require 'cgi'
require 'digest/md5'
require 'neruda/config'
require 'neruda/org_file'

# Generates OrgFile listings around their keywords
module Neruda
  class Index
    def initialize(file_list)
      @sources = file_list
      @index = { 'index' => [] }
      generate
    end

    def entries
      @index.keys
    end

    def to_s(index_name = 'index')
      content = [header(index_name).strip]
      last_year = nil
      @index[index_name].each do |article|
        year = article.timekey.slice(0, 4)
        if year != last_year
          content << ''
          content << title(year)
          last_year = year
        end
        path = File.basename(File.dirname(article.file))
        content << "- #{article.datestring}: [[./#{path}][#{article.title}]]"
      end
      content.join("\n")
    end

    def to_atom(index_name = 'index')
      content = [atom_header(index_name)]
      @index[index_name][0...3].each do |article|
        content << atom_entry(article)
      end
      content.join("\n") + '</feed>'
    end

    class << self
      def slug(title)
        title.downcase.encode('ascii', fallback: ->(k) { translit(k) })
             .gsub(' ', '-').gsub(/[^\w-]/, '').gsub(/-$/, '')
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

    def generate
      @sources.each do |f|
        next unless File.exist?(f)
        add_to_indexes(Neruda::OrgFile.new(f))
      end
      sort!
    end

    def add_to_indexes(article)
      @index['index'] << article
      article.keywords.each do |k|
        @index[k] = [] unless @index.has_key?(k)
        @index[k] << article
      end
    end

    def sort!
      @index.each do |k, i|
        @index[k] = i.sort { |a, b| b.timekey <=> a.timekey }
      end
    end

    def header(title = nil)
      title = Neruda::Config.settings['title'] if title == 'index'
      author = Neruda::Config.settings['author']
      <<~HEADER
        #+title: #{title}
        #+author: #{author}
      HEADER
    end

    def title(year)
      year = R18n.t('Unsorted') if year == '00000000000000'
      <<~ENDPROP
        * #{year}
        :PROPERTIES:
        :UNNUMBERED: notoc
        :END:
      ENDPROP
    end

    def atom_header(title = nil)
      domain = Neruda::Config.settings['domain']
      if Neruda::Config.settings['TEST'] == 'test'
        upddate = '---testupdate---'
      else
        upddate = DateTime.now.rfc3339
      end
      title = CGI.escapeHTML(title)
      <<~ENDATOM
        <?xml version="1.0" encoding="utf-8"?>
        <feed xmlns="http://www.w3.org/2005/Atom"
              xmlns:dc="http://purl.org/dc/elements/1.1/"
              xmlns:wfw="http://wellformedweb.org/CommentAPI/"
              xml:lang="#{Neruda::Config.settings['lang']}">

        <title>#{title}</title>
        <link href="#{domain}/atom.xml" rel="self" type="application/atom+xml"/>
        <link href="#{domain}" rel="alternate" type="text/html" title="#{title}"/>
        <updated>#{upddate}</updated>
        <author><name>#{Neruda::Config.settings['author'] || ''}</name></author>
        <id>urn:md5:#{Digest::MD5.hexdigest(domain)}</id>
        <generator uri="https://fossil.deparis.io/neruda">Neruda</generator>
      ENDATOM
    end

    def atom_entry(article)
      keywords = article.keywords.map do |k|
        "<dc:subject>#{CGI.escapeHTML(k)}</dc:subject>"
      end.join
      keywords += "\n  " if keywords != ''
      <<~ENDENTRY
        <entry>
          <title>#{CGI.escapeHTML(article.title)}</title>
          <link href="#{article.html_file}" rel="alternate" type="text/html"
                title="#{CGI.escapeHTML(article.title)}"/>
          <id>urn:md5:#{Digest::MD5.hexdigest(article.timekey)}</id>
          <published>#{article.timestring(:rfc3339)}</published>
          <author><name>#{CGI.escapeHTML(article.author)}</name></author>
          #{keywords}<content type="html"></content>
        </entry>
      ENDENTRY
    end
  end
end
