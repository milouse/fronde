# frozen_string_literal: true

require 'net/http'
require_relative 'version'

module Fronde
  # Everything related to Org mode
  #
  # The module itself wraps code necessary to download the last version
  # of the Emacs package. It also serves as a namespace for the class
  # responsible for handling Org files: {Fronde::Org::File}.
  module Org
    GNU_ELPA_URL = 'https://elpa.gnu.org/packages/org'

    class << self
      def current_version
        # Do not crash if Org is not yet installed (and thus return nil)
        Dir.glob('lib/org-*').first&.delete_prefix('lib/org-')
      end

      # Fetch and return the last published version of Org.
      #
      # To be nice with Org servers, this method will keep the fetched
      # version number in a cache file. You can bypass it by using the
      # force parameter.
      #
      # @param force [Boolean] Whether we should first remove the guard
      #   file if it exists
      # @param destination [String] Where to store the cookie file to
      #   remember the last version number
      # @return [String] the new x.x.x version string of Org
      def last_version(force: false, cookie_dir: 'var/tmp')
        cookie = "#{cookie_dir}/last_org_version"
        return ::File.read cookie if !force && ::File.exist?(cookie)

        org_version = fetch_version_number
        raise 'No remote Org version found' unless org_version

        FileUtils.mkdir_p cookie_dir
        ::File.write cookie, org_version
        org_version
      end

      def http_get_client(uri, &)
        Net::HTTP.start(uri.host, uri.port, use_ssl: true) do |http|
          request = Net::HTTP::Get.new(uri)
          request['User-Agent'] = Fronde::USER_AGENT
          http.request request, &
        end
      end

      def fetch_version_number
        # Retrieve last org version from GNU ELPA page.
        uri = URI("#{GNU_ELPA_URL}.html")
        response = http_get_client(uri).body
        version_line = response.each_line(chomp: true).find do |line|
          line.start_with? '<dt>Latest</dt> <dd><a href='
        end
        return unless version_line

        version_match = version_line.match(/org-(?<version>[0-9.]+)\.tar/)
        return version_match[:version] if version_match

        nil
      end

      # Download latest org-mode tarball.
      #
      # @param destination [String] where to save the org-mode tarball
      # @return [String] the downloaded org-mode version
      def download(destination = 'var/tmp')
        org_last_version = last_version(force: false, cookie_dir: destination)
        uri = URI("#{GNU_ELPA_URL}-#{org_last_version}.tar")
        # Will crash on purpose if anything goes wrong
        http_get_client(uri) do |response|
          fetch_org_tarball response, destination
        end
        org_last_version
      end

      def fetch_org_tarball(response, destination)
        # Remove version number in dest file to allow easy rake file
        # task naming
        dest_file = ::File.expand_path('org.tar', destination)
        ::File.open(dest_file, 'w') do |io|
          response.read_body { |chunk| io.write chunk }
        end
      end

      # Extract downloaded Org tarball
      #
      # @param source [String] path to the org-mode tarball to install
      # @param target [String] path to the final install directory
      def extract(source, target)
        system 'tar', '-C', target, '-xf', source
      end
    end
  end
end
