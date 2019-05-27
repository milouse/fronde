# coding: utf-8
# frozen_string_literal: true

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
      content = [header(index_name)]
      last_year = nil
      @index[index_name].each do |k|
        year = k[:key].slice(0, 4)
        if year != last_year
          content << title(year)
          last_year = year
        end
        content << "- #{k[:date]} [[#{k[:path]}][#{k[:title]}]]"
      end
      content.join("\n")
    end

    class << self
      def slug(title)
        title.encode('ascii', fallback: ->(k) { translit(k) })
          .gsub(/[^\w-]/, '').downcase
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
        art = file_info(f)
        next if art.nil?
        add_to_indexes(art)
      end
      sort!
    end

    def file_info(file_name)
      dirname = File.basename(File.dirname(file_name))
      return nil unless File.exist?(file_name)
      org_file = Neruda::OrgFile.new(file_name)
      {
        path: "./#{dirname}",
        title: org_file.title,
        key: org_file.timekey,
        date: "#{org_file.datestring}:",
        keywords: org_file.keywords
      }
    end

    def add_to_indexes(file_hash)
      @index['index'] << file_hash
      file_hash[:keywords].each do |k|
        @index[k] = [] unless @index.has_key?(k)
        @index[k] << file_hash
      end
    end

    def sort!
      @index.each do |k, i|
        @index[k] = i.sort { |a, b| b[:key] <=> a[:key] }
      end
    end

    def header(title = nil)
      title = Neruda::Config.settings['blog_title'] if title == 'index'
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
  end
end
