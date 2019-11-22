# frozen_string_literal: true

require 'fileutils'
require 'digest/md5'
require 'neruda/config'
require 'neruda/org_file'
require 'neruda/index/atom_generator'
require 'neruda/index/org_generator'

module Neruda
  # Generates website indexes and atom feeds for all the org documents
  #   keywords.
  class Index
    attr_reader :date

    include Neruda::IndexAtomGenerator
    include Neruda::IndexOrgGenerator

    def initialize(file_list = nil)
      @blog_path = Neruda::Config.settings['blog_path']
      @pubdir = Neruda::Config.settings['public_folder']
      @index = { 'index' => [] }
      @tags_names = {}
      @date = DateTime.now
      if @blog_path.nil?
        @sources = []
      else
        @sources = sources_list(file_list)
        filter_and_prefix_sources!
        @sources.each { |f| add_to_indexes(Neruda::OrgFile.new(f)) }
        sort!
      end
    end

    def entries
      @index.keys
    end

    def write_all(verbose = true)
      @index.keys.each do |k|
        write_org(k)
        warn "Generated index file for #{k}" if verbose
        write_atom(k)
        warn "Generated atom feed for #{k}" if verbose
      end
      write_org_lists
      warn 'Generated all tags index' if verbose
    end

    def sort_by(kind)
      if [:name, :weight].include?(kind)
        return sort_tags_by_name_and_weight["by_#{kind}".to_sym]
      end
      raise ArgumentError, "#{kind} not in [:name, :weight]"
    end

    private

    def sources_list(file_list)
      return file_list unless file_list.nil?
      Dir.glob(Neruda::Config.settings['blog_pattern'],
               base: "src/#{@blog_path}")
    end

    def filter_and_prefix_sources!
      exclude = Neruda::Config.settings['exclude_pattern']
      sources = []
      @sources.each do |f|
        next if f == 'index.org'
        if File.exist?(f)
          file_path = f
        else
          file_path = "src/#{@blog_path}/#{f}"
          next unless File.exist?(file_path)
        end
        next if exclude && file_path.match(exclude)
        sources << file_path
      end
      @sources = sources
    end

    def add_to_indexes(article)
      @index['index'] << article
      article.keywords.each do |k|
        slug = Neruda::OrgFile.slug k
        @tags_names[slug] = k # Overwrite is permitted
        @index[slug] = [] unless @index.has_key?(slug)
        @index[slug] << article
      end
    end

    def sort!
      @index.each do |k, i|
        @index[k] = i.sort { |a, b| b.timekey <=> a.timekey }
      end
    end

    def sort_tags_by_name_and_weight
      tags_sorted = {}
      all_keys = @index.keys.reject { |k| k == 'index' }
      tags_sorted[:by_name] = all_keys.sort
      tags_sorted[:by_weight] = all_keys.sort do |a, b|
        @index[b].length <=> @index[a].length
      end
      tags_sorted
    end
  end
end
