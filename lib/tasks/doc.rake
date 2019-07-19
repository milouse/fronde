# frozen_string_literal: true

require 'yard'
require 'fileutils'
require 'neruda/utils'

YARD::Templates::Helpers::MarkupHelper::MARKUP_PROVIDERS[:org] << \
  { lib: 'neruda/org_file'.to_sym, const: 'Neruda::OrgFile' }

# rubocop:disable Style/ClassAndModuleChildren
# Replace org converting method by a custom one
module YARD::Templates::Helpers::HtmlHelper
  def html_markup_org(text)
    opts = { content: text, verbose: Rake::FileUtilsExt.verbose_flag }
    Neruda::OrgFile.new(nil, opts).to_html
  end
end
# rubocop:enable Style/ClassAndModuleChildren

namespace :doc do
  desc 'Compile documentation from source code with yard'
  task generate: ['org:download'] do
    org_config = <<~ORGCONFIG
      (package-initialize)
      (add-to-list 'load-path "#{Dir.pwd}/org-9.2.4/lisp")
      (require 'org)
      (setq org-export-with-toc nil
            org-export-with-section-numbers nil
            org-export-with-sub-superscripts nil)
    ORGCONFIG
    IO.write 'org-config.el', org_config
    build = Thread.new do
      YARD::CLI::CommandParser.run('-m', 'org',
                                   '-M', 'neruda/org_file',
                                   '--no-progress')
    end
    Neruda::Utils.throbber(build, 'Generating documentation:')
  end
end
