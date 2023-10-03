# frozen_string_literal: true

using TimePatch

module Fronde
  # Embeds methods responsible for generating an org file for a given
  #   index.
  module IndexOrgGenerator
    def project_home_page(project_name)
      org_index(
        project_name,
        (@projects[project_name] || []).map(&:to_h)
      )
    end

    def to_org(index_name = 'index')
      return all_tags_index if index_name == 'index'

      org_index(
        @tags_names[index_name],
        (@index[index_name] || []).map(&:to_h)
      )
    end
    alias_method :to_s, :to_org

    def write_org(index_name)
      return unless save?
      slug = Slug.slug index_name
      FileUtils.mkdir_p 'tags'
      content = to_org index_name
      orgdest = "tags/#{slug}.org"
      File.write(orgdest, content)
    end

    private

    # Render an Org index file.
    #
    # @param title [String] the title of the current org index
    # @param entries [Array] the article to list in this file
    # @return [String] the org file content as a String
    def org_index(title, entries)
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
        'indexes' => indexes
      )
    end

    def write_all_blog_home(verbose)
      @sources.each do |project|
        next unless Dir.exist?(project['path'])
        if verbose
          warn R18n.t.fronde.org.generate_blog_index(name: project['name'])
        end
        orgdest = format('%<root>s/index.org', root: project['path'])
        File.write(orgdest, project_home_page(project['name']))
      end
    end
  end
end
