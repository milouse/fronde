# frozen_string_literal: true

require 'fileutils'
require 'digest/md5'
require_relative 'slug'
require_relative 'config'
require_relative 'org/file'
require_relative 'index/atom_generator'
require_relative 'index/org_generator'

module Fronde
  # Generates website indexes and atom feeds for all the org documents
  #   keywords.
  class Index
    attr_reader :date

    include IndexAtomGenerator
    include IndexOrgGenerator

    def initialize(publication_format = 'html')
      @pub_format = publication_format
      @sources = filter_sources
      @index = { 'index' => [] }
      @projects = {}
      @tags_names = {}
      @date = Time.now
      generate_feeds
      sort_feeds!
    end

    def entries
      @index.keys.reject { |tag| tag == 'index' }
    end

    def blog_homes
      @sources.filter_map do |project|
        next unless Dir.exist?(project['path'])
        [format('%<root>s/index.org', root: project['path']),
         project]
      end.to_h
    end

    def empty?
      @index['index'].empty?
    end

    def write_all(verbose: true)
      @index.each_key do |tag|
        write_org(tag)
        warn R18n.t.fronde.index.index_generated(tag: tag) if verbose
        write_atom(tag)
        warn R18n.t.fronde.index.atom_generated(tag: tag) if verbose
      end
      write_all_blog_home(verbose)
    end

    def sort_by(kind)
      accepted_values = %i[name weight]
      if accepted_values.include?(kind)
        tags_sorted = sort_tags_by_name_and_weight["by_#{kind}".to_sym]
        # Reverse in order to have most important or A near next prompt
        # and avoid to scroll to find the beginning of the list.
        return tags_sorted.map do |tag|
          @tags_names[tag] + " (#{@index[tag].length})"
        end.reverse
      end
      error_msg = R18n.t.fronde.error.index.wrong_sort_kind(
        kind: kind, accepted_values: accepted_values.inspect
      )
      raise ArgumentError, error_msg
    end

    private

    def filter_sources
      CONFIG.sources.select do |project|
        project['type'] == @pub_format && project.blog?
      end
    end

    def generate_feeds
      @sources.each do |project|
        file_pattern = '*.org'
        file_pattern = "**/#{file_pattern}" if project['recursive']
        Dir.glob(file_pattern, base: project['path']).map do |index_file|
          org_file = File.join(project['path'], index_file)
          next if project.exclude_file? org_file

          add_to_indexes(Org::File.new(org_file))
        end
      end
    end

    def add_to_project_index(article)
      project = article.project
      project_name = project['name']
      @projects[project_name] ||= []
      @projects[project_name] << article
    end

    def add_to_indexes(article)
      @index['index'] << article
      add_to_project_index article
      article.keywords.each do |tag|
        slug = Slug.slug tag
        @tags_names[slug] = tag # Overwrite is permitted
        @index[slug] ||= []
        @index[slug] << article
      end
    end

    def sort_feeds!
      sorter = proc { |tag, idx| [tag, idx.sort_by(&:timekey).reverse] }
      @index = @index.to_h(&sorter)
      @projects = @projects.to_h(&sorter)
    end

    def sort_tags_by_name_and_weight
      tags_sorted = {}
      all_keys = entries
      tags_sorted[:by_name] = all_keys.sort
      tags_sorted[:by_weight] = all_keys.sort_by do |tag|
        @index[tag].length
      end.reverse
      tags_sorted
    end

    def save?
      return true unless empty?
      @sources.each do |project|
        return true if Dir.exist?(project['path'])
      end
      false
    end
  end
end
