# frozen_string_literal: true

require 'fileutils'

module Fronde
  module CLI
    # Various utilitaries methods
    module Helpers
      def self.init_required_files
        FileUtils.cp(
          File.expand_path('./data/Rakefile', __dir__),
          'Rakefile'
        )
        return if File.exist? '.gitignore'

        FileUtils.cp(
          File.expand_path('./data/gitignore', __dir__),
          '.gitignore'
        )
      end

      def self.update_config(options)
        cnf = options.merge
        cnf.delete(:verbose)
        cnf.transform_keys!(&:to_s)
        Fronde::CONFIG.save(Fronde::CONFIG.settings.merge(cnf))
      end

      def self.launch_app_for_uri(uri)
        case Fronde::Utils.current_os
        when 'windows'
          system 'start', uri
        when 'apple'
          system 'open', uri
        else
          system 'gio', 'open', uri
        end
      end

      def self.help_command_body(command)
        body = [
          R18n.t.fronde.bin.options.cmd_title,
          Fronde::Utils.summarize_command(command)
        ]
        return body unless command == 'basic'
        body + [
          '',
          R18n.t.fronde.bin.commands.cmd_title,
          Fronde::Utils.list_commands
        ]
      end
    end
  end
end
