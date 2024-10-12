# frozen_string_literal: true

module Fronde
  # Everything related to Org mode
  #
  # The module itself wraps code necessary to download the last version
  # of the Emacs package. It also serves as a namespace for the class
  # responsible for handling Org files: {Fronde::Org::File}.
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
        org_last_version = last_version(force: false, cookie_dir: destination)
        tarball = "org-mode-release_#{org_last_version}.tar.gz"
        uri = URI("https://git.savannah.gnu.org/cgit/emacs/org-mode.git/snapshot/#{tarball}")
        # Will crash on purpose if anything goes wrong
        Net::HTTP.start(uri.host) do |http|
          fetch_org_tarball http, Net::HTTP::Get.new(uri), destination
        end
        org_last_version
      end

      def fetch_org_tarball(http, request, destination)
        # Remove version number in dest file to allow easy rake file
        # task naming
        dest_file = ::File.expand_path('org.tar.gz', destination)
        http.request request do |response|
          ::File.open(dest_file, 'w') do |io|
            response.read_body { |chunk| io.write chunk }
          end
        end
      end

      def make_org_cmd(org_dir, target, verbose: false)
        make = ['make', '-C', org_dir, target]
        return make.join(' ') if verbose

        make.insert(3, '-s')
        make << 'EMACSQ="emacs -Q --eval \'(setq inhibit-message t)\'"'
        make.join(' ')
      end

      # Compile downloaded Org package
      #
      # @param source [String] path to the org-mode tarball to install
      # @param version [String] version of the org package to install
      # @param target [String] path to the final install directory
      # @param verbose [Boolean] whether the process should be verbose
      def compile(source, version, target, verbose: false)
        untar_cmd = ['tar', '-xzf', source]
        system(*untar_cmd)
        FileUtils.mv "org-mode-release_#{version}", target
        # Fix a weird unknown package version
        ::File.write("#{target}/mk/version.mk", "ORGVERSION ?= #{version}")
        system(*make_org_cmd(target, 'compile', verbose:))
        system(*make_org_cmd(target, 'autoloads', verbose:))
      end
    end
  end
end
