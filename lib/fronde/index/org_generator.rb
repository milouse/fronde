# frozen_string_literal: true

require_relative '../utils'
using TimePatch

module Fronde
  # Embeds methods responsible for generating an org file for a given
  #   index.
  module IndexOrgGenerator
    def to_org(index_name = 'index', is_project: false)
      return project_home_page(index_name) if is_project
      return all_tags_index if index_name == 'index'
      [org_header(index_name),
       org_articles(@index[index_name])].join("\n")
    end
    alias_method :to_s, :to_org

    def write_org(index_name)
      return unless save?
      slug = Fronde::Utils.slug index_name
      FileUtils.mkdir_p 'tags'
      content = to_org index_name
      orgdest = "tags/#{slug}.org"
      File.write(orgdest, content)
    end

    private

    def project_home_page(project_name)
      content = [org_header(project_name, is_tag: false)]
      if @projects[project_name]&.any?
        content += org_articles(@projects[project_name])
      end
      content.join("\n")
    end

    def write_all_blog_home(verbose)
      @sources.each do |project|
        next unless Dir.exist?(project['path'])
        if verbose
          warn R18n.t.fronde.org.generate_blog_index(name: project['name'])
        end
        orgdest = format('%<root>s/index.org', root: project['path'])
        File.write(orgdest, to_org(project['name'], is_project: true))
      end
    end

    def all_tags_index
      content = [
        org_header(R18n.t.fronde.index.all_tags, is_tag: false)
      ]
      sort_tags_by_name_and_weight.each do |t, tags|
        content << ''
        content << org_title(R18n.t.fronde.index.send(t), 'index-tags')
        next if tags.empty?
        tags.each do |k|
          content << "- #{tag_published_url(k)} (#{@index[k].length})"
        end
      end
      content.join("\n")
    end

    def tag_published_url(tag_name)
      domain = Fronde::CONFIG.get('domain')
      title = @tags_names[tag_name]
      tag_link = "#{domain}/tags/#{tag_name}.html"
      "[[#{tag_link}][#{title}]]"
    end

    def org_header(title = nil, is_tag: true)
      if is_tag
        title = @tags_names[title]
      elsif title.nil? || title == 'index'
        title = Fronde::CONFIG.get('title')
      end
      <<~HEADER.strip
        #+title: #{title}
        #+author: #{Fronde::CONFIG.get('author')}
        #+language: #{Fronde::CONFIG.get('lang')}
      HEADER
    end

    def org_articles(articles_list)
      last_year = nil
      articles_list.map do |article|
        year_title = ''
        year = article.timekey.slice(0, 4)
        if year != last_year
          year_title = format("\n%<title>s\n", title: org_title(year))
          last_year = year
        end
        year_title + org_entry(article)
      end
    end

    def org_entry(article)
      line = "- *[[#{article.url}][#{article.title}]]*"
      art_date = article.date.l18n_long_date_string(with_year: false)
      unless art_date == ''
        published = R18n.t.fronde.index.published_on art_date
        line += " / #{published}"
      end
      line += " \\\\\n  #{article.excerpt}" if article.excerpt != ''
      line
    end

    def org_title(year, html_class = 'index-year')
      year = R18n.t.fronde.index.unsorted if year == '0000'
      <<~ENDPROP
        * #{year}
        :PROPERTIES:
        :HTML_CONTAINER_CLASS: #{html_class}
        :UNNUMBERED: notoc
        :END:
      ENDPROP
    end
  end
end
