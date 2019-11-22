# frozen_string_literal: true

module Neruda
  # Embeds methods responsible for generating an org file for a given
  #   index.
  module IndexOrgGenerator
    def to_org(index_name = 'index')
      content = [org_header(index_name).strip]
      last_year = nil
      @index[index_name].each do |article|
        year = article.timekey.slice(0, 4)
        if year != last_year
          content << ''
          content << org_title(year)
          last_year = year
        end
        content << org_entry(article)
      end
      content.join("\n")
    end
    alias_method :to_s, :to_org

    def write_org(index_name)
      return 0 if @blog_path.nil?
      FileUtils.mkdir_p 'src/tags' unless index_name == 'index'
      src = index_source_path(index_name)
      IO.write(src, to_org(index_name))
    end

    def write_org_lists
      return 0 if @blog_path.nil?
      domain = Neruda::Config.settings['domain']
      content = [org_header(R18n.t.neruda.index.all_tags)]
      sort_tags_by_name_and_weight.each do |t, tags|
        content << ''
        content << org_title(R18n.t.neruda.index.send(t), 'index-tags')
        tags.each do |k|
          title = @tags_names[k] || k
          link = "[[#{domain}/tags/#{k}.html][#{title}]]"
          content << "- #{link} (#{@index[k].length})"
        end
      end
      FileUtils.mkdir_p 'src/tags'
      src = 'src/tags/index.org'
      IO.write(src, content.join("\n"))
    end

    private

    def index_source_path(index_name)
      slug = Neruda::OrgFile.slug index_name
      src = ['src', 'tags', "#{slug}.org"]
      src[1] = @blog_path if slug == 'index'
      src.join('/')
    end

    def org_header(title = nil)
      if title.nil? || title == 'index'
        title = Neruda::Config.settings['title']
      elsif @tags_names.has_key?(title)
        title = @tags_names[title]
      end
      <<~HEADER
        #+title: #{title}
        #+author: #{Neruda::Config.settings['author']}
        #+language: #{Neruda::Config.settings['lang']}
      HEADER
    end

    def org_entry(article)
      line = "- *[[..#{article.html_file}][#{article.title}]]*"
      if article.date
        art_date = article.datestring(:full, false)
        published = R18n.t.neruda.index.published_on art_date
        line += " / #{published}"
      end
      line += " \\\\\n  #{article.excerpt}" if article.excerpt != ''
      line
    end

    def org_title(year, html_class = 'index-year')
      year = R18n.t.neruda.index.unsorted if year == '0000'
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
