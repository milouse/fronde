# frozen_string_literal: true

module Fronde
  module CLI
    # Helper code to help build the fronde option parser
    module OptParse
      # @return [Hash] the possible ~fronde~ options and their
      #   configuration
      FRONDE_OPTIONS = {
        '-a' => { long: 'author' },
        '-f' => { long: 'force', boolean: true },
        '-h' => { long: 'help', boolean: true, method: :on_tail,
                  help: I18n.t('fronde.bin.options.help') },
        '-l' => { long: 'lang', keyword: 'LOCALE' },
        '-o' => { long: 'output', keyword: 'FORMAT', choices: %w[gemini html] },
        '-t' => { long: 'title' },
        '-v' => { long: 'verbose', boolean: true, method: :on_tail },
        '-V' => { long: 'version', boolean: true, method: :on_tail,
                  help: I18n.t('fronde.bin.options.version') }
      }.freeze

      # TODO: jekyll new [path] / jekyll build / jekyll clean / jekyll serve
      # TODO: hugo new site [path] / hugo / hugo new content / hugo server
      # TODO: zola init [path] / zola build --root path_to_project / zola serve

      # @return [Hash] the possible ~fronde~ subcommands and their
      #   configuration
      FRONDE_COMMANDS = {
        'new' => { opts: ['-a', '-l', '-o', '-t', '-v'], label: 'new <path>' },
        'init' => { alias: 'new' },
        'update' => {},
        'config' => { alias: 'update' },
        'preview' => {},
        'open' => { opts: ['-a', '-l', '-t', '-v'], label: 'open <path>' },
        'edit' => { alias: 'open' },
        'build' => { opts: ['-f'] },
        'publish' => {},
        'help' => {},
        'basic' => { opts: ['-h', '-V'], label: '<command>' }
      }.freeze

      class << self
        # Returns the short and long options specification for a given
        #   short option.
        #
        # This method use the {Fronde::CLI::OptParse::FRONDE_OPTIONS}
        # Hash to retrieve corresponding values.
        #
        # @example
        #     spec = Fronde::CLI::OptParse.decorate_option('-a')
        #     => ['-a AUTHOR', '--author AUTHOR']
        #
        # @param short [String] the short option to decorate
        # @return [Array] the short and long specification for an option
        def decorate_option(short)
          opt = FRONDE_OPTIONS[short]
          long = "--#{opt[:long]}"
          if opt[:boolean]
            config = [short, long]
          else
            key = opt[:keyword] || opt[:long].upcase
            config = [short, format('%<long>s %<key>s', long: long, key: key)]
          end
          config.push opt[:choices], opt[:help]
          config.compact
        end

        # Returns the ~fronde~ help summary for a given command.
        #
        # @param command [String] the command for which a summary
        #   should be given
        # @return [String]
        def summarize_command(command)
          (FRONDE_COMMANDS[command][:opts] || []).map do |k|
            short, long = decorate_option(k)
            opt = FRONDE_OPTIONS[k]
            label = [short, long].join(', ')
            line = [format('    %<opt>s', opt: label).ljust(30), opt[:help]]
            choices = opt[:choices]
            line << "(#{choices.join(', ')})" if choices
            line.compact.join(' ')
          end.join("\n")
        end

        def help_command_body(command)
          command_opts_doc = summarize_command(command)
          return '' if command_opts_doc == ''

          body = [I18n.t('fronde.bin.options.cmd_title'), command_opts_doc]
          if command == 'basic'
            body += ['', I18n.t('fronde.bin.commands.cmd_title'), list_commands]
          end
          body.join("\n")
        end

        # Returns a formatted list of available commands for ~fronde~.
        #
        # @return [String]
        def list_commands
          FRONDE_COMMANDS.filter_map do |cmd, opt|
            next if cmd == 'basic'

            line = ['   ', cmd.ljust(10)]
            if opt.has_key? :alias
              line << I18n.t('fronde.bin.commands.alias', alias: opt[:alias])
            else
              line << I18n.t("fronde.bin.commands.#{cmd}")
            end
            line.join(' ')
          end.join("\n")
        end

        # Returns the real command name for a given command, which may be
        #   an alias.
        #
        # @param command [String] the command to resolve
        # @return [String]
        def resolve_possible_alias(command)
          return 'basic' unless FRONDE_COMMANDS.include?(command)

          cmd_opt = FRONDE_COMMANDS[command]
          return cmd_opt[:alias] if cmd_opt.has_key?(:alias)

          command
        end

        # Returns the given command options.
        #
        # This method will first try to resolve command alias, if any.
        #
        # @param command [String] the command, which options should be returned
        # @return [Hash] the command options
        def command_options(command)
          cmd = resolve_possible_alias command
          FRONDE_COMMANDS[cmd].merge(name: cmd)
        end
      end
    end
  end
end
