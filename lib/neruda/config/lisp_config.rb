# frozen_string_literal: true

require 'open-uri'

module Neruda
  # This module contains utilitary methods to ease ~org-config.el~
  # file generation
  module LispConfig
    # Fetch and return the last published version of org mode.
    #
    # @return [String] the new x.x.x version string of org mode
    def org_last_version
      return @org_version if @org_version
      index = open('https://orgmode.org/index.html', 'r').read
      last_ver = index.match(/https:\/\/orgmode\.org\/org-([0-9.]+)\.tar\.gz/)
      # :nocov:
      if last_ver.nil?
        warn 'Org last version not found'
        return nil
      end
      # :nocov:
      @org_version = last_ver[1]
    end

    # Generate emacs lisp configuration file for org mode and write it.
    #
    # This method saves the generated configuration in the file
    # ~org-config.el~ at the root of your project, overwriting it if it
    # existed already.
    #
    # @return [Integer] the length written (as returned by the
    #   underlying ~IO.write~ method call)
    def write_org_lisp_config
      projects = org_generate_projects
      workdir = Dir.pwd
      content = IO.read(File.expand_path('./org-config.el', __dir__))
                  .gsub('__WORK_DIR__', workdir)
                  .gsub('__NERUDA_DIR__', __dir__)
                  .gsub('__ORG_VER__', org_last_version)
                  .gsub('__ALL_PROJECTS__', all_projects(projects).strip)
                  .gsub('__THEME_CONFIG__', org_theme_config.strip)
                  .gsub('__ALL_PROJECTS_NAMES__', project_names(projects))
                  .gsub('__LONG_DATE_FMT__', r18n_full_datetime_format)
                  .gsub('__AUTHOR_EMAIL__', settings['author_email'] || '')
                  .gsub('__AUTHOR_NAME__', settings['author'])
      IO.write("#{workdir}/org-config.el", content)
    end

    # Generate emacs directory variables file.
    #
    # This method generate the file ~.dir-locals.el~, which is
    # responsible to load neruda org mode settings when visiting an
    # org file of this neruda instance.
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

    def project_names(projects)
      projects.keys.map { |p| ["\"#{p}\"", "\"#{p}-assets\""] }
              .flatten.join(' ')
    end

    def all_projects(projects)
      projects.values.join("\n").strip
              .gsub(/\n\n/, "\n")
              .gsub(/\n/, "\n        ")
    end

    def org_project(project_name, opts)
      orgtpl = opts['org_headers']
      base_directory = File.expand_path(opts['path'])
      publish_in = [Dir.pwd, settings['public_folder']]
      publish_in << project_name unless project_name == 'neruda'
      publish_in = publish_in.join('/')
      recline = [opts['recursive'] || 't']
      default_ex_ptrn = settings['exclude_pattern']
      if opts['exclude']
        recline << ":exclude \"#{opts['exclude']}\""
      elsif project_name == 'neruda' && default_ex_ptrn
        recline << ":exclude \"#{default_ex_ptrn}\""
      end
      <<~ORGPROJECT
        ("#{project_name}"
         :base-directory "#{base_directory}"
         :base-extension "org"
         :recursive #{recline.join("\n ")}
         :publishing-directory "#{publish_in}"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         #{orgtpl})
        ("#{project_name}-assets"
         :base-directory "#{base_directory}"
         :base-extension "jpg\\\\\\|gif\\\\\\|png\\\\\\|svg\\\\\\|pdf"
         :recursive #{recline[0]}
         :publishing-directory "#{publish_in}"
         :publishing-function org-publish-attachment)
      ORGPROJECT
    end

    def org_default_theme_options
      postamble = <<~POSTAMBLE
        <p><span class="author">#{R18n.t.neruda.org.postamble.written_by}</span>
        #{R18n.t.neruda.org.postamble.with_emacs}</p>
        <p class="date">#{R18n.t.neruda.org.postamble.last_modification}</p>
        <p class="validation">%v</p>
      POSTAMBLE
      { 'html-head' => build_html_head.strip,
        'html-postamble' => postamble.strip,
        'html-head-include-default-style' => 't',
        'html-head-include-scripts' => 'nil' }
    end

    def org_templates
      orgtplopts = org_default_theme_options.merge
      orgtplopts.merge!(settings['org-html'] || {})
      orgtpl = []
      orgtplopts.each do |k, v|
        val = v.strip.gsub(/"/, '\"')
        if ['t', 'nil', '1'].include? val
          orgtpl << ":#{k} #{val}"
        else
          orgtpl << ":#{k} \"#{val}\""
        end
      end
      orgtpl.join("\n ")
    end

    def org_external_projects_opts(seed, orgtpl)
      opts = { 'org_headers' => orgtpl }
      if seed.is_a? String
        opts['path'] = seed
      elsif seed.is_a? Hash
        opts.merge! seed
      end
      opts
    end

    def org_generate_projects
      orgtpl = org_templates
      default_project = org_project(
        'neruda', 'org_headers' => orgtpl, 'path' => './src'
      )
      projects = { 'neruda' => default_project }
      settings['external_sources']&.each do |s|
        opts = org_external_projects_opts(s, orgtpl)
        next unless opts.has_key?('path')
        pname = File.basename(opts['path']).sub(/^\./, '')
        projects[pname] = org_project(pname, opts)
      end
      projects
    end

    def org_theme_config
      curtheme = settings['theme'] || 'default'
      workdir = Dir.pwd
      if curtheme == 'default'
        sourcedir = File.expand_path('../../../', __dir__)
      else
        sourcedir = workdir
      end
      <<~THEMECONFIG
        ("theme"
                 :base-directory "#{sourcedir}/themes/#{curtheme}"
                 :base-extension "jpg\\\\\\|gif\\\\\\|png\\\\\\|js\\\\\\|css\\\\\\|otf\\\\\\|ttf\\\\\\|woff2?"
                 :recursive t
                 :publishing-directory "#{workdir}/#{settings['public_folder']}/assets"
                 :publishing-function org-publish-attachment)
      THEMECONFIG
    end

    def build_html_head
      stylesheet = <<~CSS
        <link rel="stylesheet" type="text/css" media="screen"
              href="#{settings['domain']}/assets/css/style.css">
        <link rel="stylesheet" type="text/css" media="screen"
              href="#{settings['domain']}/assets/css/htmlize.css">
      CSS
      return stylesheet if settings['blog_path'].nil?
      <<~ATOM
        #{stylesheet.strip}
        <link rel="alternate" type="application/atom+xml" title="Atom 1.0"
              href="#{settings['domain']}/feeds/index.xml" />
      ATOM
    end
  end
end
