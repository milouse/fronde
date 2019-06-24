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
      @slugs = { 'index' => 'index' }
      @blog_path = Neruda::Config.settings['blog_path']
      @pubdir = Neruda::Config.settings['public_folder']
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
        link = "[[..#{article.html_file}][#{article.title}]]"
        content << "** #{article.datestring}: #{link}\n\n#{article.format('%x')}"
      end
      content.join("\n")
    end

    def to_atom(index_name = 'index')
      content = [atom_header(index_name)]
      @index[index_name][0...10].each do |article|
        content << atom_entry(article)
      end
      content.join("\n") + '</feed>'
    end

    def write(index_name)
      src = index_source_path(index_name)
      File.open(src, 'w') do |f|
        f.puts to_s(index_name)
      end
      src
    end

    def write_atom(index_name)
      slug = @slugs[index_name]
      atomdest = "#{@pubdir}/feeds/#{slug}.xml"
      File.open(atomdest, 'w') do |f|
        f.puts to_atom(index_name)
      end
      atomdest
    end

    def index_public_path(index_name)
      slug = @slugs[index_name]
      dest = [@pubdir, 'tags', "#{slug}.html"]
      dest[1] = @blog_path if slug == 'index'
      dest.join('/')
    end

    private

    def index_source_path(index_name)
      slug = @slugs[index_name]
      src = ['src', 'tags', "#{slug}.org"]
      src[1] = @blog_path if slug == 'index'
      src.join('/')
    end

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
        unless @index.has_key?(k)
          @index[k] = []
          @slugs[k] = Neruda::OrgFile.slug k
        end
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
      slug = Neruda::OrgFile.slug(title)
      tagurl = "#{domain}/tags/#{slug}.html"
      if title == 'index'
        title = Neruda::Config.settings['title']
        tagurl = "#{domain}/#{@blog_path}"
      end
      title_esc = CGI.escapeHTML(title)
      <<~ENDATOM
        <?xml version="1.0" encoding="utf-8"?>
        <feed xmlns="http://www.w3.org/2005/Atom"
              xmlns:dc="http://purl.org/dc/elements/1.1/"
              xmlns:wfw="http://wellformedweb.org/CommentAPI/"
              xml:lang="#{Neruda::Config.settings['lang']}">

        <title>#{title_esc}</title>
        <link href="#{domain}/feeds/#{slug}.xml" rel="self" type="application/atom+xml"/>
        <link href="#{tagurl}" rel="alternate" type="text/html" title="#{title_esc}"/>
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
      title_esc = CGI.escapeHTML(article.title)
      <<~ENDENTRY
        <entry>
          <title>#{title_esc}</title>
          <link href="#{article.url}" rel="alternate" type="text/html"
                title="#{title_esc}"/>
          <id>urn:md5:#{Digest::MD5.hexdigest(article.timekey)}</id>
          <published>#{article.timestring(:rfc3339)}</published>
          <author><name>#{CGI.escapeHTML(article.author)}</name></author>
          #{keywords}<content type="html">#{CGI.escapeHTML(article.format('%X'))}</content>
        </entry>
      ENDENTRY
    end
  end
end
