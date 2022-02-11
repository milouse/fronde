# frozen_string_literal: true

require 'json'
require 'open-uri'
require 'fronde/version'

def fetch_org_version
  # Retrieve last org version from git repository tags page.
  tag_rx = Regexp.new(
    '<a href=\'/cgit/emacs/org-mode.git/tag/\?h=' \
    '(?<tag>release_(?<number>[^\']+))\'>\k<tag></a>'
  )
  versions = URI(
    'https://git.savannah.gnu.org/cgit/emacs/org-mode.git/refs/'
  ).open.readlines.map do |line|
    line.match(tag_rx) { |matchdata| matchdata[:number] }
  end
  versions.compact.first
end

module Fronde
  # This module contains utilitary methods to ease ~org-config.el~
  # file generation
  module LispConfig
    # Fetch and return the last published version of Org.
    #
    # @return [String] the new x.x.x version string of Org
    def org_last_version
      return @org_version if @org_version
      if File.exist?('var/tmp/last_org_version')
        @org_version = File.read('var/tmp/last_org_version')
        return @org_version
      end
      @org_version = fetch_org_version
      FileUtils.mkdir_p 'var/tmp'
      File.write('var/tmp/last_org_version', @org_version)
      @org_version
    end

    # Generate emacs lisp configuration file for Org and write it.
    #
    # This method saves the generated configuration in the file
    # ~org-config.el~ at the root of your project, overwriting it if it
    # existed already.
    #
    # @return [Integer] the length written (as returned by the
    #   underlying ~File.write~ method call)
    # rubocop:disable Metrics/MethodLength
    def write_org_lisp_config(with_tags: false)
      projects = org_generate_projects(with_tags: with_tags)
      workdir = Dir.pwd
      content = File.read(File.expand_path('./org-config.el', __dir__))
                    .gsub('__VERSION__', Fronde::VERSION)
                    .gsub('__WORK_DIR__', workdir)
                    .gsub('__FRONDE_DIR__', __dir__)
                    .gsub('__ORG_VER__', org_last_version)
                    .gsub(
                      '__ALL_PROJECTS__',
                      projects.values.join("\n        ")
                    )
                    .gsub('__THEME_CONFIG__', org_default_theme_config)
                    .gsub('__ALL_PROJECTS_NAMES__', project_names(projects))
                    .gsub('__LONG_DATE_FMT__', r18n_full_datetime_format)
                    .gsub('__AUTHOR_EMAIL__', get('author_email', ''))
                    .gsub('__AUTHOR_NAME__', get('author'))
      FileUtils.mkdir_p "#{workdir}/var/lib"
      File.write("#{workdir}/var/lib/org-config.el", content)
    end
    # rubocop:enable Metrics/MethodLength

    # Generate emacs directory variables file.
    #
    # This method generate the file ~.dir-locals.el~, which is
    # responsible to load fronde Org settings when visiting an Org file
    # of this fronde instance.
    #
    # @return [Integer] the length written (as returned by the
    #   underlying ~File.write~ method call)
    def write_dir_locals
      workdir = Dir.pwd
      # rubocop:disable Layout/LineLength
      File.write(
        "#{workdir}/.dir-locals.el",
        "((org-mode . ((eval . (load-file \"#{workdir}/var/lib/org-config.el\")))))"
      )
      # rubocop:enable Layout/LineLength
    end

    private

    def r18n_full_datetime_format
      locale = R18n.get.locale
      date_fmt = R18n.t.fronde.index.full_date_format(
        date: locale.full_format
      )
      date_fmt = locale.year_format.sub('_', date_fmt)
      time_fmt = locale.time_format.delete('_').strip
      R18n.t.fronde.index.full_date_with_time_format(
        date: date_fmt, time: time_fmt
      )
    end

    def ruby_to_lisp_boolean(value)
      return 't' if value == true
      'nil'
    end

    def project_names(projects)
      names = projects.keys.map do |p|
        ["\"#{p}\"", "\"#{p}-assets\""]
      end.flatten
      names << "\"theme-#{get('theme')}\"" unless get('theme') == 'default'
      sources.each do |s|
        # Default theme defined in settings is already included
        next unless s['theme'] && s['theme'] != get('theme')
        # Never include theme named 'default' as it does not rely on any
        # file to export.
        next if s['theme'] == 'default'
        theme = "\"theme-#{s['theme']}\""
        next if names.include? theme
        names << theme
      end
      names.join(' ')
    end

    # Return the full path to the publication path of a given project
    #   configuration.
    #
    # @param project [Hash] a project configuration (as extracted from
    #   the ~sources~ key)
    # @return [String] the full path to the target dir of this project
    def publication_path(project)
      publish_in = [Dir.pwd]
      if project['type'] == 'gemini'
        publish_in << get('gemini_public_folder')
      else
        publish_in << get('html_public_folder')
      end
      publish_in << project['target'] unless project['target'] == '.'
      publish_in.join('/')
    end

    # Return the publication function needed for a given project
    #   configuration.
    #
    # @param project [Hash] a project configuration (as extracted from
    #   the ~sources~ key)
    # @return [String] the org publication function name
    def publication_function(project)
      case project['type']
      when 'gemini'
        'org-gmi-publish-to-gemini'
      else
        'org-html-publish-to-html'
      end
    end

    def org_project(project_name, opts)
      shared_lines = org_project_shared_lines(opts)
      project_config = [
        org_project_config(project_name, opts, shared_lines),
        org_assets_config(project_name, shared_lines)
      ]
      themeconf = org_theme_config(opts['theme'])
      project_config << themeconf unless themeconf == ''
      project_config.join("\n        ")
    end

    def org_default_html_postamble
      <<~POSTAMBLE
        <p><span class="author">#{R18n.t.fronde.org.postamble.written_by}</span>
        #{R18n.t.fronde.org.postamble.with_emacs_html}</p>
        <p class="date">#{R18n.t.fronde.org.postamble.last_modification}</p>
        <p class="validation">%v</p>
      POSTAMBLE
    end

    def org_default_gemini_postamble
      format(
        "📅 %<date>s\n📝 %<author>s %<creator>s",
        author: R18n.t.fronde.org.postamble.written_by,
        creator: R18n.t.fronde.org.postamble.with_emacs,
        date: R18n.t.fronde.org.postamble.last_modification
      )
    end

    def org_default_html_head
      <<~HTMLHEAD
        <link rel="stylesheet" type="text/css" media="screen"
              href="__DOMAIN__/assets/__THEME__/css/style.css">
        <link rel="stylesheet" type="text/css" media="screen"
              href="__DOMAIN__/assets/__THEME__/css/htmlize.css">
        __ATOM_FEED__
      HTMLHEAD
    end

    def org_default_html_options(project)
      defaults = {
        'html-postamble' => org_default_html_postamble,
        'html-head' => '__ATOM_FEED__',
        'html-head-include-default-style' => 't',
        'html-head-include-scripts' => 't'
      }
      curtheme = project['theme'] || get('theme')
      return defaults if curtheme.nil? || curtheme == 'default'
      defaults['html-head'] = org_default_html_head
      defaults['html-head-include-default-style'] = 'nil'
      defaults['html-head-include-scripts'] = 'nil'
      defaults
    end

    def org_publish_options(project)
      defaults = {
        'section-numbers' => 'nil',
        'with-toc' => 'nil'
      }
      if project['type'] == 'gemini'
        defaults['gemini-postamble'] = org_default_gemini_postamble
      else
        defaults.merge!(
          org_default_html_options(project),
          get('org-html', {}),
          project['org-html'] || {}
        )
      end
      defaults.merge(project['org-options'] || {})
    end

    def expand_vars_in_html_head(head, project)
      curtheme = project['theme'] || get('theme')
      # Head may be frozen when coming from settings
      head = head.gsub('__THEME__', curtheme)
                 .gsub('__DOMAIN__', get('domain'))
      return head.gsub('__ATOM_FEED__', '') unless project['is_blog']
      atomfeed = <<~ATOMFEED
        <link rel="alternate" type="application/atom+xml" title="Atom 1.0"
              href="#{get('domain')}/feeds/index.xml" />
      ATOMFEED
      head.gsub('__ATOM_FEED__', atomfeed)
    end

    def cast_lisp_value(value)
      return 't' if value.is_a?(TrueClass)
      return 'nil' if value.nil? || value.is_a?(FalseClass)
      value.strip.gsub(/"/, '\"')
    end

    def build_project_org_headers(project)
      orgtplopts = org_publish_options(project)
      lisp_keywords = ['t', 'nil', '1', '-1', '0'].freeze
      orgtplopts.map do |k, v|
        v = expand_vars_in_html_head(v, project) if k == 'html-head'
        val = cast_lisp_value(v)
        if lisp_keywords.include? val
          ":#{k} #{val}"
        else
          ":#{k} \"#{val}\""
        end
      end
    end

    def org_generate_projects(with_tags: false)
      projects = {}
      projects_sources = sources
      if with_tags
        tags_conf = build_source('tags')
        tags_conf['recursive'] = false
        projects_sources << tags_conf
      end
      projects_sources.each do |opts|
        opts['org_headers'] = build_project_org_headers(opts)
        projects[opts['name']] = org_project(opts['name'], opts)
      end
      projects
    end

    def org_default_theme_config
      theme_config = org_theme_config(get('theme'))
      return theme_config if theme_config == ''
      format("\n        %<conf>s", conf: theme_config)
    end

    def org_theme_config(theme)
      return '' if theme.nil? || theme == 'default'
      workdir = Dir.pwd
      [
        format('("theme-%<theme>s"', theme: theme),
        format(' :base-directory "%<wd>s/themes/%<theme>s"',
               wd: workdir, theme: theme),
        # rubocop:disable Layout/LineLength
        ' :base-extension "jpg\\\\\\|gif\\\\\\|png\\\\\\|js\\\\\\|css\\\\\\|otf\\\\\\|ttf\\\\\\|woff2?"',
        # rubocop:enable Layout/LineLength
        ' :recursive t',
        format(' :publishing-directory "%<wd>s/%<pub>s/assets/%<theme>s"',
               wd: workdir, pub: get('html_public_folder'), theme: theme),
        ' :publishing-function org-publish-attachment)'
      ].join("\n        ").strip
    end

    def org_project_shared_lines(project)
      [
        format(':base-directory "%<path>s"', path: project['path']),
        format(
          ':publishing-directory "%<path>s"',
          path: publication_path(project)
        ),
        format(
          ':recursive %<rec>s',
          rec: ruby_to_lisp_boolean(project['recursive'])
        )
      ]
    end

    def org_project_config(project_name, project, shared_lines)
      project_lines = [
        format('"%<name>s"', name: project_name),
        ':base-extension "org"',
        format(
          ':publishing-function %<fun>s',
          fun: publication_function(project)
        )
      ] + shared_lines + project['org_headers']
      if project['exclude']
        project_lines << format(
          ':exclude "%<value>s"', value: project['exclude']
        )
      end
      format('(%<pr>s)', pr: project_lines.join("\n         "))
    end

    def org_assets_config(project_name, shared_lines)
      assets_lines = [
        format('"%<name>s-assets"', name: project_name),
        ':base-extension "jpg\\\\\\|gif\\\\\\|png\\\\\\|svg\\\\\\|pdf"',
        ':publishing-function org-publish-attachment'
      ] + shared_lines
      format('(%<assets>s)', assets: assets_lines.join("\n         "))
    end
  end
end
