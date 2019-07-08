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
      @slugs = { 'index' => 'index' }
      @date = DateTime.now
      @sources = sources_list(file_list)
      filter_and_prefix_sources!
      @sources.each { |f| add_to_indexes(Neruda::OrgFile.new(f)) }
      sort!
    end

    def entries
      @index.keys
    end

    def write_all(verbose = true)
      entries.each do |k|
        src = write_org(k)
        warn "Generated index file #{src}" if verbose
        atom = write_atom(k)
        warn "Generated atom feed #{atom}" if verbose
      end
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

    def index_source_path(index_name)
      slug = @slugs[index_name]
      src = ['src', 'tags', "#{slug}.org"]
      src[1] = @blog_path if slug == 'index'
      src.join('/')
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
  end
end
