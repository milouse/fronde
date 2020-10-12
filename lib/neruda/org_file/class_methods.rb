# frozen_string_literal: true

module Neruda
  # This module holds class methods for the {Neruda::OrgFile} class.
  module OrgFileClassMethods
    def source_for_target(file_name)
      # file_name may be frozen...
      src = file_name.sub(/\.html$/, '.org')
      pubfolder = Neruda::Config.settings['public_folder']
      src.sub!(/^#{pubfolder}\//, '')
      # Look for match in each possible sources. The first found wins.
      Neruda::Config.sources.each do |project|
        if project['target'] == '.'
          origin = File.join(project['path'], src)
        else
          origin = File.join(
            project['path'], src.sub(/^#{project['target']}\//, '')
          )
        end
        return origin if File.exist?(origin)
      end
      nil
    end

    def target_for_source(file_name, project, with_public_folder: true)
      return nil if file_name.nil?
      # file_name may be frozen...
      target = file_name.sub(/\.org$/, '.html').sub(/^#{Dir.pwd}\//, '')
      if project.nil?
        subfolder = File.basename(File.dirname(target))
        target = File.basename(target)
        target = "#{subfolder}/#{target}" if subfolder != '.'
      else
        project_relative_path = project['path'].sub(/^#{Dir.pwd}\//, '')
        target.sub!(/^#{project_relative_path}\//, '')
        target = "#{project['target']}/#{target}" if project['target'] != '.'
      end
      return target unless with_public_folder
      pubfolder = Neruda::Config.settings['public_folder']
      "#{pubfolder}/#{target}"
    end

    def project_for_source(file_name)
      # Look for match in each possible sources. The first found wins.
      Neruda::Config.sources.each do |project|
        project_relative_path = project['path'].sub(/^#{Dir.pwd}\//, '')
        return project if file_name.match?(/^#{project_relative_path}\//)
      end
      nil
    end

    def slug(title)
      title.downcase.tr(' ', '-')
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
