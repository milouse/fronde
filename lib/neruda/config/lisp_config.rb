# frozen_string_literal: true

module Neruda
  # This module contains utilitary methods to ease ~org-config.el~
  # file generation
  module LispConfig
    def write_org_lisp_config
      projects = org_generate_projects
      workdir = Dir.pwd
      content = IO.read(File.expand_path('./org-config.el', __dir__))
                  .gsub('__WORK_DIR__', workdir)
                  .gsub('__ORG_VER__', org_last_version)
                  .gsub('__ALL_PROJECTS__', all_projects(projects).strip)
                  .gsub('__THEME_CONFIG__', org_theme_config.strip)
                  .gsub('__ALL_PROJECTS_NAMES__', project_names(projects))
                  .gsub('__LONG_DATE_FMT__', R18n.t.neruda.long_date_format)
      IO.write("#{workdir}/org-config.el", content)
    end

    private

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
      publish_in << project_name unless project_name == 'org'
      publish_in = publish_in.join('/')
      recline = [opts['recursive'] || 't']
      default_ex_ptrn = settings['exclude_pattern']
      if opts['exclude']
        recline << ":exclude \"#{opts['exclude']}\""
      elsif project_name == 'org' && default_ex_ptrn
        recline << ":exclude \"#{default_ex_ptrn}\""
      end
      <<~ORGPROJECT
        ("#{project_name}"
         :base-directory "#{base_directory}"
         :base-extension "org"
         :recursive #{recline.join("\n ")}
         :publishing-directory "#{publish_in}"
         :publishing-function pablo-publish-to-html-and-customize-output
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
      stylesheet = <<~CSS
        <link rel="stylesheet" type="text/css" media="screen"
              href="#{settings['domain']}/assets/css/style.css">
      CSS
      postamble = <<~POSTAMBLE
        <p><span class="author">#{R18n.t.neruda.org.postamble.written_by}</span>
        #{R18n.t.neruda.org.postamble.with_emacs}</p>
        <p class="date">#{R18n.t.neruda.org.postamble.last_modification}</p>
        <p class="validation">%v</p>
      POSTAMBLE
      { 'html-head' => stylesheet.strip,
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

    def org_generate_projects
      orgtpl = org_templates
      projects = { 'org' => org_project('org', 'org_headers' => orgtpl,
                                               'path' => './src') }
      settings['external_sources']&.each do |s|
        opts = { 'org_headers' => orgtpl }
        if s.is_a? String
          opts['path'] = s
        elsif s.is_a? Hash
          opts.merge! s
        end
        next unless opts.has_key?('path')
        pname = File.basename(opts['path']).sub(/^\./, '')
        projects[pname] = org_project(pname, opts)
      end
      projects
    end

    def org_theme_config
      curtheme = settings['theme'] || 'default'
      workdir = Dir.pwd
      sourcedir = workdir
      sourcedir = File.expand_path('../../../', __dir__) if curtheme == 'default'
      <<~THEMECONFIG
        ("theme"
                 :base-directory "#{sourcedir}/themes/#{curtheme}"
                 :base-extension "jpg\\\\\\|gif\\\\\\|png\\\\\\|js\\\\\\|css\\\\\\|otf\\\\\\|ttf\\\\\\|woff2?"
                 :recursive t
                 :publishing-directory "#{workdir}/#{settings['public_folder']}/assets"
                 :publishing-function org-publish-attachment)
      THEMECONFIG
    end
  end
end