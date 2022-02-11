# frozen_string_literal: true

module Fronde
  # This module holds class methods for the {Fronde::OrgFile} class.
  module OrgFileClassMethods
    def source_for_target(file_name)
      src = possible_source_path(file_name)
      # Look for match in each possible sources. The first found wins.
      Fronde::Config.sources.each do |project|
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
      pub_settings = project_publish_settings(project)
      # file_name may be frozen...
      target = file_name.sub(/^#{Dir.pwd}\//, '')
      target.sub!(/\.org\z/, pub_settings[:ext])
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
      "#{pub_settings[:folder]}/#{target}"
    end

    def project_for_source(file_name)
      # Look for match in each possible sources. The first found wins.
      Fronde::Config.sources.each do |project|
        project_relative_path = project['path'].sub(/^#{Dir.pwd}\//, '')
        return project if file_name.match?(/^#{project_relative_path}\//)
      end
      nil
    end

    def slug(title)
      title.downcase.tr(' ', '-')
           .encode('ascii', fallback: ->(k) { translit(k) })
           .gsub(/[^\w-]/, '').delete_suffix('-')
    end

    private

    def project_publish_settings(project)
      default = {
        ext: '.html',
        folder: Fronde::Config.get('html_public_folder')
      }
      # project may be nil, often in test cases
      return default unless project
      if project['type'] == 'gemini'
        return {
          ext: '.gmi',
          folder: Fronde::Config.get('gemini_public_folder')
        }
      end
      default
    end

    def possible_source_path(file_name)
      ext = File.extname(file_name)
      if ext == '.gmi'
        pubfolder = Fronde::Config.get('gemini_public_folder')
      else
        pubfolder = Fronde::Config.get('html_public_folder')
      end
      file_name.sub(/^#{pubfolder}\//, '').sub(/#{ext}\z/, '.org')
    end

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
