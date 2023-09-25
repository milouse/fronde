# frozen_string_literal: true

require 'json'
require 'open-uri'
require_relative '../version'
require_relative 'helpers'

require_relative '../../ext/r18n'
using R18nPatch

module Fronde
  module Config
    # This module contains utilitary methods to ease ~org-config.el~
    # file generation
    module Lisp
      # Fetch and return the last published version of Org.
      #
      # @return [String] the new x.x.x version string of Org
      def org_last_version
        return @org_version if @org_version
        if File.exist?('var/tmp/last_org_version')
          @org_version = File.read('var/tmp/last_org_version')
          return @org_version
        end
        @org_version = Fronde::Config::Helpers.fetch_org_version
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
      def write_org_lisp_config
        workdir = Dir.pwd
        all_projects = sources.map(&:org_config).flatten
        all_themes = org_generate_themes(all_projects)
        FileUtils.mkdir_p "#{workdir}/var/lib"
        content = Helpers.render_liquid_template(
          File.read(File.expand_path('./data/org-config.el', __dir__)),
          'version' => Fronde::VERSION,
          'work_dir' => workdir,
          'fronde_data_dir' => File.expand_path('data', __dir__),
          'org_version' => org_last_version,
          'long_date_fmt' => R18n.t.full_datetime_format.to_s,
          'author' => { 'email' => get('author_email', ''),
                        'name' => get('author') },
          'all_projects' => all_projects + all_themes
        )
        File.write("#{workdir}/var/lib/org-config.el", content)
      end
      # rubocop:enable Metrics/MethodLength

      private

      def org_theme_config(theme)
        workdir = Dir.pwd
        { 'base-directory' => "#{workdir}/themes/#{theme}",
          # rubocop:disable Layout/LineLength
          'base-extension' => %w[css js gif jpg png svg otf ttf woff2?].join('\\\\|'),
          'publishing-directory' => "#{workdir}/#{get('html_public_folder')}/assets/#{theme}",
          # rubocop:enable Layout/LineLength
          'publishing-function' => 'org-publish-attachment',
          'recursive' => true }
      end

      def org_generate_themes(projects)
        all_themes = projects.map do |project|
          project['attributes']['theme']
        end
        all_themes << get('theme')
        all_themes.uniq.compact.filter_map do |theme|
          next if theme == 'default'

          { 'name' => "theme-#{theme}",
            'attributes' => org_theme_config(theme) }
        end
      end
    end
  end
end
