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
        line = ["- #{article.datestring}:",
                "[[..#{article.html_file}][#{article.title}]]"]
        line << "-- #{article.excerpt}" if article.excerpt != ''
        content << line.join(' ')
      end
      content.join("\n")
    end
    alias_method :to_s, :to_org

    def write_org(index_name)
      FileUtils.mkdir_p 'src/tags' unless index_name == 'index'
      src = index_source_path(index_name)
      IO.write(src, to_org(index_name))
      src
    end

    private

    def org_header(title = nil)
      title = Neruda::Config.settings['title'] if title == 'index'
      author = Neruda::Config.settings['author']
      <<~HEADER
        #+title: #{title}
        #+author: #{author}
      HEADER
    end

    def org_title(year)
      year = R18n.t('Unsorted') if year == '00000000000000'
      <<~ENDPROP
        * #{year}
        :PROPERTIES:
        :HTML_CONTAINER_CLASS: index-year
        :UNNUMBERED: notoc
        :END:
      ENDPROP
    end
  end
end
