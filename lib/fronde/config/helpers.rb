# frozen_string_literal: true

require 'liquid'

module Fronde
  module Config
    # Various utilitaries methods
    module Helpers
      def self.extract_lang_from_env(default)
        (ENV['LANG'] || default).split('_', 2).first
      end

      def self.migrate(config)
        if config.has_key?('html_public_folder') || \
           !config.has_key?('public_folder')
          return config
        end
        warn '‘public_folder’ setting is deprecated. Please use either ‘html_public_folder’ or ‘gemini_public_folder’.' # rubocop:disable Layout/LineLength
        config['html_public_folder'] = config.delete('public_folder')
        config
      end

      def self.fetch_org_version
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

      # Generate emacs directory variables file.
      #
      # This method generate the file ~.dir-locals.el~, which is
      # responsible to load fronde Org settings when visiting an Org file
      # of this fronde instance.
      #
      # @return [Integer] the length written (as returned by the
      #   underlying ~File.write~ method call)
      def self.write_dir_locals
        workdir = Dir.pwd
        # rubocop:disable Layout/LineLength
        File.write(
          "#{workdir}/.dir-locals.el",
          "((org-mode . ((eval . (load-file \"#{workdir}/var/lib/org-config.el\")))))"
        )
        # rubocop:enable Layout/LineLength
      end

      def self.render_liquid_template(content, data)
        template = Liquid::Template.parse(content)
        template.render(data)
      end
    end

    # Filter for liquid templates
    module Filters
      def cast_lisp_value(value, key)
        return 't' if value.is_a?(TrueClass)
        return 'nil' if value.nil? || value.is_a?(FalseClass)

        value = value.strip
        lisp_keywords = ['t', 'nil', '1', '-1', '0']
        if key.end_with?('-function') || lisp_keywords.include?(value)
          return value
        end

        value.gsub!('"', '\"')
        %("#{value}")
      end

      def md5(value)
        Digest::MD5.hexdigest(value)
      end
    end
  end
end

Liquid::Template.register_filter(Fronde::Config::Filters)
