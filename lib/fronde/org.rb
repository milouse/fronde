# frozen_string_literal: true

module Fronde
  module Org
    class << self
      def current_version
        # Do not crash if Org is not yet installed (and thus return nil)
        Dir['lib/org-*'].first&.delete_prefix('lib/org-')
      end

      # Fetch and return the last published version of Org.
      #
      # To be nice with Org servers, this method will keep the fetched
      # version number in a cache file. You can bypass it by using the
      # force parameter.
      #
      # @param force [Boolean] Whether we should first remove the guard
      #   file if it exists
      # @return [String] the new x.x.x version string of Org
      def last_version(force: false)
        if !force && ::File.exist?('var/tmp/last_org_version')
          return ::File.read('var/tmp/last_org_version')
        end
        org_version = fetch_version_number
        FileUtils.mkdir_p 'var/tmp'
        ::File.write('var/tmp/last_org_version', org_version)
        org_version
      end

      def fetch_version_number
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

      # Download latest org-mode tarball.
      #
      # @param destination [String] where to save the org-mode tarball
      # @return [String] the downloaded org-mode version
      def download(destination = 'var/tmp')
        org_last_version = last_version

        # :nocov:
        return if org_last_version.nil?

        # :nocov:
        # Remove version number in dest file to allow easy rake file
        # task naming
        dest_file = ::File.expand_path('org.tar.gz', destination)
        return org_last_version if ::File.exist?(dest_file)

        tarball = "org-mode-release_#{org_last_version}.tar.gz"
        uri = URI("https://git.savannah.gnu.org/cgit/emacs/org-mode.git/snapshot/#{tarball}")
        # Will crash on purpose if anything goes wrong
        Net::HTTP.start(uri.host) do |http|
          request = Net::HTTP::Get.new uri

          http.request request do |response|
            ::File.open(dest_file, 'w') do |io|
              response.read_body { |chunk| io.write chunk }
            end
          end
        end
        org_last_version
      end
    end
  end
end
