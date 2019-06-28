# frozen_string_literal: true

require 'open-uri'
# Neruda::Config is required by Neruda::Utils
require 'neruda/utils'

def org_project(project_name, opts)
  orgtpl = opts['org_headers']
  base_directory = File.expand_path(opts['path'])
  publish_in = [Dir.pwd, Neruda::Config.settings['public_folder']]
  publish_in << project_name unless project_name == 'org'
  publish_in = publish_in.join('/')
  recline = [opts['recursive'] || 't']
  default_ex_ptrn = Neruda::Config.settings['exclude_pattern']
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
     :base-extension "jpg\\\\|gif\\\\|png\\\\|svg\\\\|pdf"
     :recursive #{recline[0]}
     :publishing-directory "#{publish_in}"
     :publishing-function org-publish-attachment)
  ORGPROJECT
end

def org_templates
  orgtpl = []
  Neruda::Config.settings['org-html']&.each do |k, v|
    orgtpl << ":#{k} \"#{v.strip.gsub(/"/, '\"')}\""
  end
  orgtpl.join("\n ")
end

def generate_org_projects
  orgtpl = org_templates
  projects = { 'org' => org_project('org', 'org_headers' => orgtpl,
                                           'path' => './src') }
  Neruda::Config.settings['external_sources']&.each do |s|
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

def org_config
  projects = generate_org_projects
  project_names = projects.keys.map { |p| ["\"#{p}\"", "\"#{p}-assets\""] }
                          .flatten.join(' ')
  all_projects = projects.values.join("\n").gsub(/\n\n/, "\n")
                         .gsub(/\n/, "\n        ")
  workdir = Dir.pwd
  <<~ORGCONFIG
    ;; Needed for nice htmlize
    (package-initialize)
    ;; Load org mode
    (add-to-list 'load-path "#{workdir}/org-#{Neruda::Config.org_last_version}/lisp")
    (require 'org)

    (org-link-set-parameters "i18n"
                             :export #'org-i18n-export
                             :follow #'org-i18n-follow)

    (defun org-i18n-export (link description format)
      "Export a i18n link"
      (let* ((splitted-link (split-string link "|"))
             (path (car splitted-link))
             (desc (or description path))
             (lang (car (cdr splitted-link))))
        (pcase format
          (`html (if lang
                     (format "<a href=\\"%s\\" hreflang=\\"%s\\">%s</a>"
                             path lang desc)
                   (format "<a href=\\"%s\\">%s</a>" path desc)))
          (`latex (format "\\\\href{%s}{%s}" path desc))
          (`ascii (format "%s (%s)" desc path))
          (_ path))))

    (defun org-i18n-follow (link)
      "Visit a i18n link"
      (browse-url (car (split-string link "|"))))

    (defun pablo-publish-to-html-and-customize-output (plist filename pub-dir)
      "Wrap the `org-html-publish-to-html' function and customize its output.

    FILENAME is the filename of the Org file to be published.  PLIST
    is the property list for the given project.  PUB-DIR is the
    publishing directory.

    Return output file name."
      (let* ((html-file (org-html-publish-to-html plist filename pub-dir))
             (workdir "#{workdir}/")
             (relative-html-file (substring html-file (length workdir)))
             (command (concat "rake 'site:customize_output[" relative-html-file "]'")))
        (message (replace-regexp-in-string "\\n$" "" (shell-command-to-string command)))
        html-file))

    (setq make-backup-files nil
          enable-local-variables :all
          org-publish-timestamp-directory "#{workdir}/tmp/"
          org-id-locations-file "#{workdir}/tmp/org-id-locations"
          org-confirm-babel-evaluate nil
          org-html-doctype "html5"
          org-html-html5-fancy t
          org-html-head-include-default-style nil
          org-html-head-include-scripts nil
          org-html-metadata-timestamp-format "%a %d %B %Y à %H:%M"
          org-html-text-markup-alist '((bold . "<strong>%s</strong>")
                                       (code . "<code>%s</code>")
                                       (italic . "<em>%s</em>")
                                       (strike-through . "<del>%s</del>")
                                       (underline . "<span class=\\"underline\\">%s</span>")
                                       (verbatim . "<code>%s</code>"))
          org-publish-project-alist
          `(#{all_projects.strip}
            ("theme"
             :base-directory "#{workdir}/themes/#{Neruda::Config.settings['theme']}"
             :base-extension "jpg\\\\|gif\\\\|png\\\\|js\\\\|css\\\\|otf\\\\|ttf\\\\|woff2?"
             :recursive t
             :publishing-directory "#{workdir}/#{Neruda::Config.settings['public_folder']}/assets"
             :publishing-function org-publish-attachment)
            ("website" :components (#{project_names} "theme"))))
  ORGCONFIG
end

namespace :org do
  desc 'Download last version of org-mode'
  task :download do
    verbose = Rake::FileUtilsExt.verbose_flag
    org_version = Neruda::Config.org_last_version
    next if Neruda::Config.org_last_version.nil?
    next if Dir.exist?("org-#{org_version}/lisp")
    tarball = "org-#{org_version}.tar.gz"
    curl = ['curl', '--progress-bar', '-O',
            "https://orgmode.org/#{tarball}"]
    make = ['make', '-C', "org-#{org_version}"]
    unless verbose
      curl[1] = '-s'
      make << '-s'
      make << 'EMACSQ="emacs -Q --eval \'(setq inhibit-message t)\'"'
    end
    build = Thread.new do
      sh curl.join(' ')
      sh "tar xzf #{tarball}"
      File.unlink tarball
      sh make.join(' ')
      sh((make + ['compile']).join(' '))
      sh((make + ['autoloads']).join(' '))
    end
    if verbose
      build.join
      warn "org-mode #{org_version} has been locally installed"
    else
      Neruda::Utils.throbber(build, 'Installing org mode:')
    end
  end

  file 'org-config.el' do
    next if Neruda::Config.org_last_version.nil?
    IO.write('org-config.el', org_config)
  end

  desc 'Install org'
  task install: ['org:download', 'org-config.el'] do
    mkdir_p "#{Neruda::Config.settings['public_folder']}/assets"
    mkdir_p 'src'
  end
end
