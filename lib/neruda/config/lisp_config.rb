# frozen_string_literal: true

require 'open-uri'
require 'neruda/version'

module Neruda
  # This module contains utilitary methods to ease ~org-config.el~
  # file generation
  module LispConfig
    # Fetch and return the last published version of Org.
    #
    # @return [String] the new x.x.x version string of Org
    def org_last_version
      return @org_version if @org_version
      if File.exist?('tmp/__last_org_version__')
        @org_version = IO.read('tmp/__last_org_version__')
        return @org_version
      end
      index = URI('https://orgmode.org/index.html').open.read
      last_ver = index.match(/https:\/\/orgmode\.org\/org-([0-9.]+)\.tar\.gz/)
      # :nocov:
      if last_ver.nil?
        warn 'Org last version not found'
        return nil
      end
      FileUtils.mkdir_p 'tmp'
      IO.write('tmp/__last_org_version__', last_ver[1])
      # :nocov:
      @org_version = last_ver[1]
    end

    # Generate emacs lisp configuration file for Org and write it.
    #
    # This method saves the generated configuration in the file
    # ~org-config.el~ at the root of your project, overwriting it if it
    # existed already.
    #
    # @return [Integer] the length written (as returned by the
    #   underlying ~IO.write~ method call)
    def write_org_lisp_config(with_tags: false)
      projects = org_generate_projects(with_tags: with_tags)
      workdir = Dir.pwd
      content = IO.read(File.expand_path('./org-config.el', __dir__))
                  .gsub('__VERSION__', Neruda::VERSION)
                  .gsub('__WORK_DIR__', workdir)
                  .gsub('__NERUDA_DIR__', __dir__)
                  .gsub('__ORG_VER__', org_last_version)
                  .gsub('__ALL_PROJECTS__', all_projects(projects))
                  .gsub('__THEME_CONFIG__', org_default_theme_config)
                  .gsub('__ALL_PROJECTS_NAMES__', project_names(projects))
                  .gsub('__LONG_DATE_FMT__', r18n_full_datetime_format)
                  .gsub('__AUTHOR_EMAIL__', settings['author_email'] || '')
                  .gsub('__AUTHOR_NAME__', settings['author'])
      IO.write("#{workdir}/org-config.el", content)
    end

    # Generate emacs directory variables file.
    #
    # This method generate the file ~.dir-locals.el~, which is
    # responsible to load neruda Org settings when visiting an Org file
    # of this neruda instance.
    #
    # @return [Integer] the length written (as returned by the
    #   underlying ~IO.write~ method call)
    def write_dir_locals
      workdir = Dir.pwd
      IO.write(
        "#{workdir}/.dir-locals.el",
        "((org-mode . ((eval . (load-file \"#{workdir}/org-config.el\")))))"
      )
    end

    private

    def r18n_full_datetime_format
      locale = R18n.get.locale
      date_fmt = R18n.t.neruda.index.full_date_format(
        date: locale.full_format
      )
      date_fmt = locale.year_format.sub('_', date_fmt)
      time_fmt = locale.time_format.delete('_').strip
      R18n.t.neruda.index.full_date_with_time_format(
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
      unless settings['theme'] == 'default'
        names << "\"theme-#{settings['theme']}\""
      end
      sources.each do |s|
        # Default theme defined in settings is already included
        next unless s['theme'] && s['theme'] != settings['theme']
        # Never include theme named 'default' as it does not rely on any
        # file to export.
        next if s['theme'] == 'default'
        theme = "\"theme-#{s['theme']}\""
        next if names.include? theme
        names << theme
      end
      names.join(' ')
    end

    def all_projects(projects)
      projects.values.join("\n").strip
              .gsub(/\n\s*\n/, "\n")
              .gsub(/\n/, "\n        ")
    end

    # Return the full path to the publication path of a given project
    #   configuration.
    #
    # @param project [Hash] a project configuration (as extracted from
    #   the ~sources~ key)
    # @return [String] the full path to the target dir of this project
    def publication_path(project)
      publish_in = [Dir.pwd, settings['public_folder']]
      publish_in << project['target'] unless project['target'] == '.'
      publish_in.join('/')
    end

    def org_project(project_name, opts)
      publish_in = publication_path(opts)
      other_lines = [
        format(':recursive %<value>s',
               value: ruby_to_lisp_boolean(opts['recursive']))
      ]
      if opts['exclude']
        other_lines << format(':exclude "%<value>s"',
                              value: opts['exclude'])
      end
      themeconf = org_theme_config(opts['theme'])
      <<~ORGPROJECT
        ("#{project_name}"
         :base-directory "#{opts['path']}"
         :base-extension "org"
         #{other_lines.join("\n ")}
         :publishing-directory "#{publish_in}"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         #{opts['org_headers']})
        ("#{project_name}-assets"
         :base-directory "#{opts['path']}"
         :base-extension "jpg\\\\\\|gif\\\\\\|png\\\\\\|svg\\\\\\|pdf"
         #{other_lines[0]}
         :publishing-directory "#{publish_in}"
         :publishing-function org-publish-attachment)
        #{themeconf}
      ORGPROJECT
    end

    def org_default_postamble
      <<~POSTAMBLE
        <p><span class="author">#{R18n.t.neruda.org.postamble.written_by}</span>
        #{R18n.t.neruda.org.postamble.with_emacs}</p>
        <p class="date">#{R18n.t.neruda.org.postamble.last_modification}</p>
        <p class="validation">%v</p>
      POSTAMBLE
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
      curtheme = project['theme'] || settings['theme']
      if curtheme.nil? || curtheme == 'default'
        return { 'html-head' => '__ATOM_FEED__',
                 'html-postamble' => org_default_postamble,
                 'html-head-include-default-style' => 't',
                 'html-head-include-scripts' => 't' }
      end
      { 'html-head' => org_default_html_head,
        'html-postamble' => org_default_postamble,
        'html-head-include-default-style' => 'nil',
        'html-head-include-scripts' => 'nil' }
    end

    def expand_vars_in_html_head(head, project)
      curtheme = project['theme'] || settings['theme']
      # Head may be frozen when coming from settings
      head = head.gsub('__THEME__', curtheme)
                 .gsub('__DOMAIN__', settings['domain'])
      return head.gsub('__ATOM_FEED__', '') unless project['is_blog']
      atomfeed = <<~ATOMFEED
        <link rel="alternate" type="application/atom+xml" title="Atom 1.0"
              href="#{settings['domain']}/feeds/index.xml" />
      ATOMFEED
      head.gsub('__ATOM_FEED__', atomfeed)
    end

    def build_project_org_headers(project)
      orgtplopts = org_default_html_options(project).merge(
        settings['org-html'] || {}, project['org-html'] || {}
      )
      orgtpl = []
      truthy_val = ['t', 'nil', '1'].freeze
      orgtplopts.each do |k, v|
        v = expand_vars_in_html_head(v, project) if k == 'html-head'
        val = v.strip.gsub(/"/, '\"')
        if truthy_val.include? val
          orgtpl << ":#{k} #{val}"
        else
          orgtpl << ":#{k} \"#{val}\""
        end
      end
      orgtpl.join("\n ")
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
      theme_config = org_theme_config(settings['theme'])
      return theme_config if theme_config == ''
      output = theme_config.split("\n").map do |line|
        "        #{line}"
      end
      format("\n%<conf>s", conf: output.join("\n"))
    end

    def org_theme_config(theme)
      return '' if theme.nil? || theme == 'default'
      workdir = Dir.pwd
      <<~THEMECONFIG
        ("theme-#{theme}"
         :base-directory "#{workdir}/themes/#{theme}"
         :base-extension "jpg\\\\\\|gif\\\\\\|png\\\\\\|js\\\\\\|css\\\\\\|otf\\\\\\|ttf\\\\\\|woff2?"
         :recursive t
         :publishing-directory "#{workdir}/#{settings['public_folder']}/assets/#{theme}"
         :publishing-function org-publish-attachment)
      THEMECONFIG
    end
  end
end
