# frozen_string_literal: true

require_relative 'slug'
require_relative 'config'
require_relative 'org/file'

module Fronde
  # Generates website indexes and atom feeds for all the org documents
  #   keywords.
  class Index
    attr_reader :date

    def initialize(project)
      @project = project
      @index = { 'index' => [] }
      @entries = []
      @tags_names = {}
      @date = Time.now
      generate_feeds if @project.blog?
      sort_feeds!
    end

    def all_tags
      @index.keys.reject { |tag| tag == 'index' }
    end

    def empty?
      @index['index'].empty?
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

    class << self
      def all_html_blog_index(&block)
        all_blogs = CONFIG.sources.filter_map do |project|
          next unless project['type'] == 'html' && project.blog?

          Index.new(project)
        end
        return all_blogs unless block

        all_blogs.each(&block)
      end
    end

    private

    def generate_feeds
      file_pattern = '*.org'
      file_pattern = "**/#{file_pattern}" if @project['recursive']
      Dir.glob(file_pattern, base: @project['path']).map do |index_file|
        # Obviously don't parse tags
        next if index_file.start_with?('tags/')

        org_file = File.join(@project['path'], index_file)
        next if @project.exclude_file? org_file

        add_to_indexes(Org::File.new(org_file))
      end
    end

    def add_to_indexes(article)
      @index['index'] << article
      @entries << article
      article.keywords.each do |tag|
        slug = Slug.slug tag
        @tags_names[slug] = tag # Overwrite is permitted
        @index[slug] ||= []
        @index[slug] << article
      end
    end

    def sort_feeds!
      @index.transform_values! do |articles|
        articles.sort_by(&:timekey).reverse
      end
      @entries = @entries.sort_by(&:timekey).reverse
    end

    def sort_tags_by_name_and_weight
      all_keys = all_tags
      {
        by_name: all_keys.sort,
        by_weight: all_keys.sort_by { @index[_1].length }.reverse
      }
    end
  end
end

require_relative 'index/org_generator'
require_relative 'index/atom_generator'
