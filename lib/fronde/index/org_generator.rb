# frozen_string_literal: true

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
      slug = Fronde::OrgFile.slug index_name
      FileUtils.mkdir 'tags' unless Dir.exist? 'tags'
      content = to_org index_name
      orgdest = "tags/#{slug}.org"
      IO.write(orgdest, content)
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
      Fronde::Config.sources.each do |project|
        next unless project['is_blog']
        next unless Dir.exist?(project['path'])
        warn "Generated blog home for #{project['name']}" if verbose
        orgdest = format('%<root>s/index.org', root: project['path'])
        IO.write(orgdest, to_org(project['name'], is_project: true))
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
      domain = Fronde::Config.settings['domain']
      title = @tags_names[tag_name]
      tag_link = "#{domain}/tags/#{tag_name}.html"
      "[[#{tag_link}][#{title}]]"
    end

    def org_header(title = nil, is_tag: true)
      if is_tag
        title = @tags_names[title]
      elsif title.nil? || title == 'index'
        title = Fronde::Config.settings['title']
      end
      <<~HEADER.strip
        #+title: #{title}
        #+author: #{Fronde::Config.settings['author']}
        #+language: #{Fronde::Config.settings['lang']}
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
      if article.date
        art_date = article.datestring(:full, year: false)
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
