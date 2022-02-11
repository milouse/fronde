# frozen_string_literal: true

require 'fileutils'
require 'digest/md5'
require 'fronde/config'
require 'fronde/org_file'
require 'fronde/index/atom_generator'
require 'fronde/index/org_generator'

module Fronde
  # Generates website indexes and atom feeds for all the org documents
  #   keywords.
  class Index
    attr_reader :date

    include Fronde::IndexAtomGenerator
    include Fronde::IndexOrgGenerator

    def initialize
      @index = { 'index' => [] }
      @projects = {}
      @tags_names = {}
      @date = DateTime.now
      feed
      sort!
    end

    def entries
      @index.keys.reject { |k| k == 'index' }
    end

    def empty?
      @index['index'].empty?
    end

    def write_all(verbose: true)
      @index.each_key do |k|
        write_org(k)
        warn "Generated index file for #{k}" if verbose
        write_atom(k)
        warn "Generated atom feed for #{k}" if verbose
      end
      write_all_blog_home(verbose)
    end

    def sort_by(kind)
      if [:name, :weight].include?(kind)
        tags_sorted = sort_tags_by_name_and_weight["by_#{kind}".to_sym]
        # Reverse in order to have most important or A near next prompt
        # and avoid to scroll to find the beginning of the list.
        return tags_sorted.map do |k|
          @tags_names[k] + " (#{@index[k].length})"
        end.reverse
      end
      raise ArgumentError, "#{kind} not in [:name, :weight]"
    end

    private

    def feed
      Fronde::Config.sources.each do |project|
        next unless project['is_blog']
        if project['recursive']
          file_pattern = '**/*.org'
        else
          file_pattern = '*.org'
        end
        Dir.glob(file_pattern, base: project['path']).map do |s|
          org_file = File.join(project['path'], s)
          next if exclude_file?(org_file, project)
          add_to_indexes(
            Fronde::OrgFile.new(org_file, project: project)
          )
        end
      end
    end

    def add_to_project_index(article)
      project = article.project
      @projects[project['name']] ||= []
      @projects[project['name']] << article
    end

    def add_to_indexes(article)
      @index['index'] << article
      add_to_project_index article
      article.keywords.each do |k|
        slug = Fronde::OrgFile.slug k
        @tags_names[slug] = k # Overwrite is permitted
        @index[slug] ||= []
        @index[slug] << article
      end
    end

    def sort!
      @index.each do |k, i|
        @index[k] = i.sort { |a, b| b.timekey <=> a.timekey }
      end
      @projects.each do |k, i|
        @projects[k] = i.sort { |a, b| b.timekey <=> a.timekey }
      end
    end

    def sort_tags_by_name_and_weight
      tags_sorted = {}
      all_keys = entries
      tags_sorted[:by_name] = all_keys.sort
      tags_sorted[:by_weight] = all_keys.sort do |a, b|
        @index[b].length <=> @index[a].length
      end
      tags_sorted
    end

    def exclude_file?(file_path, project)
      # Obviously excluding index itself for blogs
      return true if file_path == File.join(project['path'], 'index.org')
      return false unless project['exclude']
      file_path.match? project['exclude']
    end

    def save?
      return true unless empty?
      Fronde::Config.sources.each do |project|
        return true if project['is_blog'] && Dir.exist?(project['path'])
      end
      false
    end
  end
end
