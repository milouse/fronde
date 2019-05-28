# coding: utf-8
# frozen_string_literal: true

require 'open-uri'
require 'neruda/config'

def org_last_version
  index = open('https://orgmode.org/index.html', 'r').read
  last_ver = index.match(/https:\/\/orgmode\.org\/org-([0-9.]+)\.tar\.gz/)
  return last_ver[1] unless last_ver.nil?
  warn 'Org last version not found'
  nil
end

ORG_VERSION = org_last_version

def org_config(orgtpl)
  <<~ORGCONFIG
    (package-initialize)
    (add-to-list 'load-path "./org-#{ORG_VERSION}/lisp")

    (setq org-html-doctype "html5"
          org-html-html5-fancy t
          org-html-head-include-default-style nil
          org-html-head-include-scripts nil
          org-html-metadata-timestamp-format "%a %d %B %Y à %H:%M"
          #{orgtpl.join("\n      ")}
          org-export-with-toc nil
          org-html-text-markup-alist '((bold . "<strong>%s</strong>")
                                       (code . "<code>%s</code>")
                                       (italic . "<em>%s</em>")
                                       (strike-through . "<del>%s</del>")
                                       (underline . "<span class=\\"underline\\">%s</span>")
                                       (verbatim . "<code>%s</code>")))
  ORGCONFIG
end

namespace :org do
  desc 'Download last version of org-mode'
  task :download do
    next if Dir.exist?("org-#{ORG_VERSION}/lisp")
    next if ORG_VERSION.nil?
    tarball = "org-#{ORG_VERSION}.tar.gz"
    sh "curl --progress-bar -O https://orgmode.org/#{tarball}"
    sh "tar xzf #{tarball}"
    File.unlink tarball
    sh "make -C org-#{ORG_VERSION}"
    sh "make -C org-#{ORG_VERSION} compile"
    sh "make -C org-#{ORG_VERSION} autoloads"
    puts "org-mode #{ORG_VERSION} has been locally installed"
  end

  file 'org-config.el' do
    next if ORG_VERSION.nil?
    orgtpl = []
    Neruda::Config.settings['org-html']&.each do |k, v|
      orgtpl << "#{k} \"#{v.strip.gsub(/"/, '\"')}\""
    end
    puts 'Write org-config.el'
    File.open('org-config.el', 'w') do |f|
      f.puts org_config(orgtpl)
    end
  end

  desc 'Install org'
  task install: ['org:download', 'org-config.el']
end
