# frozen_string_literal: true

module Neruda
  # This module holds class methods for the {Neruda::OrgFile} class.
  module OrgFileClassMethods
    def html_file(file_name)
      path = Neruda::OrgFile.target_for_source(file_name)
      pubfolder = Neruda::Config.settings['public_folder']
      path.sub(/^#{pubfolder}\//, '/')
    end

    def html_file_with_domain(file_name)
      Neruda::Config.settings['domain'] + html_file(file_name)
    end

    def source_for_target(file_name)
      # file_name may be frozen...
      src = file_name.sub(/\.html$/, '.org')
      pubfolder = Neruda::Config.settings['public_folder']
      src.sub(/^#{pubfolder}\//, 'src/')
    end

    def target_for_source(file_name)
      # file_name may be frozen...
      target = file_name.sub(/\.org$/, '.html')
      pubfolder = Neruda::Config.settings['public_folder']
      return target.sub(/^src\//, "#{pubfolder}/") if /^src\//.match?(target)
      subfolder = File.basename(File.dirname(target))
      leaf = File.basename(target)
      "#{pubfolder}/#{subfolder}/#{leaf}"
    end

    def file_name(title, for_blog = false)
      title = 'new' if title.nil? || title == ''
      filename = Neruda::OrgFile.slug title
      return "src/#{filename}.org" unless for_blog
      blog_path = Neruda::Config.settings['blog_path']
      "src/#{blog_path}/#{filename}/content.org"
    end

    def slug(title)
      title.downcase.gsub(' ', '-')
           .encode('ascii', fallback: ->(k) { translit(k) })
           .gsub(/[^\w-]/, '').gsub(/-$/, '')
    end

    private

    def translit(char)
      return 'a' if ['á', 'à', 'â', 'ä', 'ǎ', 'ã', 'å'].include?(char)
      return 'e' if ['é', 'è', 'ê', 'ë', 'ě', 'ẽ'].include?(char)
      return 'i' if ['í', 'ì', 'î', 'ï', 'ǐ', 'ĩ'].include?(char)
      return 'o' if ['ó', 'ò', 'ô', 'ö', 'ǒ', 'õ'].include?(char)
      return 'u' if ['ú', 'ù', 'û', 'ü', 'ǔ', 'ũ'].include?(char)
      return 'y' if ['ý', 'ỳ', 'ŷ', 'ÿ', 'ỹ'].include?(char)
      return 'c' if char == 'ç'
      return 'n' if char == 'ñ'
      '-'
    end
  end
end
