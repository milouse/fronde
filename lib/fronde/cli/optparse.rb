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
        '-h' => { long: 'help', boolean: true, meth: :on_tail },
        '-l' => { long: 'lang', keyword: 'LOCALE' },
        '-t' => { long: 'title' },
        '-v' => { long: 'verbose', boolean: true, meth: :on_tail },
        '-V' => { long: 'version', boolean: true, meth: :on_tail }
      }.freeze

      # @return [Hash] the possible ~fronde~ subcommands and their
      #   configuration
      FRONDE_COMMANDS = {
        'init' => { opts: ['-a', '-h', '-l', '-t', '-v'] },
        'update' => { opts: ['-a', '-h', '-l', '-t', '-v'] },
        'config' => { alias: 'update' },
        'preview' => { opts: ['-h'] },
        'open' => { opts: ['-a', '-h', '-l', '-t', '-v'] },
        'edit' => { alias: 'open' },
        'build' => { opts: ['-f', '-h'] },
        'publish' => { opts: ['-h'], with_args: true },
        'help' => { opts: ['-h'] },
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
          return [short, long] if opt[:boolean]

          key = opt[:keyword] || opt[:long].upcase
          [short + key, format('%<long>s %<key>s', long: long, key: key)]
        end

        # Returns the ~fronde~ help summary for a given command.
        #
        # @param command [String] the command for which a summary
        #   should be given
        # @return [String]
        def summarize_command(command)
          FRONDE_COMMANDS[command][:opts].map do |k|
            short, long = decorate_option(k)
            opt = FRONDE_OPTIONS[k]
            label = [short, long].join(', ')
            line = [format('    %<opt>s', opt: label).ljust(30)]
            if R18n.t.fronde.bin.options[opt[:long]].translated?
              line << R18n.t.fronde.bin.options[opt[:long]]
            end
            line.join(' ')
          end.join("\n")
        end

        def help_command_body(command)
          body = [
            R18n.t.fronde.bin.options.cmd_title,
            summarize_command(command)
          ]
          return body unless command == 'basic'

          body + [
            '',
            R18n.t.fronde.bin.commands.cmd_title,
            list_commands
          ]
        end

        # Returns a formatted list of available commands for ~fronde~.
        #
        # @return [String]
        def list_commands
          FRONDE_COMMANDS.filter_map do |cmd, opt|
            next if cmd == 'basic'
            line = ['   ', cmd.ljust(10)]
            if opt.has_key? :alias
              line << R18n.t.fronde.bin.commands.alias(opt[:alias])
            else
              line << R18n.t.fronde.bin.commands[cmd]
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
