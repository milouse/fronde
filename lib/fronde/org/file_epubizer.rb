# frozen_string_literal: true

module Fronde
  module Org
    # This module holds epub related methods for the {Fronde::Org::File}
    #   class.
    module FileEpubizer
      # Create an epub version for the current file
      #
      # @return [Boolean, nil] the underlying ~system~ method return value
      def to_epub
        pub_file = @data[:pub_file]
        return unless pub_file

        file_name = Fronde::CONFIG.get('html_public_folder') + pub_file
        return unless ::File.exist? file_name

        epub_file = file_name.sub!(/\.html\z/, '.epub')
        system 'pandoc', '-S', '-o', epub_file, @file
      end
    end
  end
end
