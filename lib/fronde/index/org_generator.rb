# frozen_string_literal: true

using TimePatch

module Fronde
  # Reopen Index class to embed methods responsible for generating an
  #   org file for a given index.
  class Index
    def blog_home_page
      org_index @project['title'], '__HOME_PAGE__', @entries.map(&:to_h)
    end

    def to_org(index_name = 'index')
      return all_tags_index if index_name == 'index'

      org_index(
        @tags_names[index_name], index_name,
        (@index[index_name] || []).map(&:to_h)
      )
    end
    alias_method :to_s, :to_org

    def write_org(index_name)
      slug = Slug.slug index_name
      orgdest = "#{@project['path']}/tags/#{slug}.org"
      File.write orgdest, to_org(index_name)
    end

    def write_all_org(verbose: true)
      FileUtils.mkdir_p "#{@project['path']}/tags"
      @index.each_key do |tag|
        write_org(tag)
        warn R18n.t.fronde.index.index_generated(tag: tag) if verbose
      end
      write_blog_home_page(verbose)
    end

    private

    # Render an Org index file.
    #
    # @param title [String] the title of the current org index
    # @param slug [String] the slug of the current org index
    # @param entries [Array] the article to list in this file
    # @return [String] the org file content as a String
    def org_index(title, slug, entries)
      entries.map! do |article|
        published = article['published']
        unless published == ''
          article['published'] = R18n.t.fronde.index.published_on published
        end
        article
      end
      Config::Helpers.render_liquid_template(
        File.read(File.expand_path('./data/template.org', __dir__)),
        'title' => title,
        'slug' => slug,
        'project_path' => @project.public_absolute_path,
        'domain' => Fronde::CONFIG.get('domain'),
        'lang' => Fronde::CONFIG.get('lang'),
        'author' => Fronde::CONFIG.get('author'),
        'unsorted' => R18n.t.fronde.index.unsorted,
        'entries' => entries
      )
    end

    def all_tags_index
      indexes = sort_tags_by_name_and_weight.map do |title, tags|
        all_tags = tags.map do |tag|
          {
            'slug' => tag, 'title' => @tags_names[tag],
            'weight' => @index[tag].length
          }
        end
        { 'title' => R18n.t.fronde.index.send(title), 'tags' => all_tags }
      end
      Config::Helpers.render_liquid_template(
        File.read(File.expand_path('./data/all_tags.org', __dir__)),
        'title' => R18n.t.fronde.index.all_tags,
        'lang' => Fronde::CONFIG.get('lang'),
        'author' => Fronde::CONFIG.get('author'),
        'domain' => Fronde::CONFIG.get('domain'),
        'project_path' => @project.public_absolute_path,
        'indexes' => indexes
      )
    end

    def write_blog_home_page(verbose)
      orgdest = format('%<root>s/index.org', root: @project['path'])
      if verbose
        warn R18n.t.fronde.org.generate_blog_index(name: @project['name'])
      end
      File.write(orgdest, blog_home_page)
    end
  end
end
