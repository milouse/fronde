# frozen_string_literal: true

require 'rainbow'
require 'r18n-core'
require 'neruda/config'

module Neruda
  # Embeds usefull methods, mainly used in rake tasks.
  module Utils
    # @return [Hash] the possible throbber themes
    THROBBER_FRAMES = {
      'basic' => '-\|/',
      'basicdots' => '⋯⋱⋮⋰',
      'moon' => '🌑🌒🌓🌔🌕🌖🌗🌘',
      'clock' => '🕛🕐🕑🕒🕓🕔🕕🕖🕗🕘🕙🕚',
      'bricks' => '⣷⣯⣟⡿⢿⣻⣽⣾',
      'points' => '·⁘∷⁛∷⁘',
      'quadrant2' => '▙▛▜▟',
      'default' => ['⠁ ⠂ ⠄ ⡀ ⠄ ⠂ ⠁', '⠂ ⠁ ⠂ ⠄ ⡀ ⠄ ⠂', '⠄ ⠂ ⠁ ⠂ ⠄ ⡀ ⠄',
                    '⡀ ⠄ ⠂ ⠁ ⠂ ⠄ ⡀', '⠄ ⡀ ⠄ ⠂ ⠁ ⠂ ⠄', '⠂ ⠄ ⡀ ⠄ ⠂ ⠁ ⠂']
    }.freeze

    # @return [Hash] the possible ~pablo~ options and their
    #   configuration
    PABLO_OPTIONS = {
      '-a' => { long: 'author' },
      '-l' => { long: 'lang', keyword: 'LOCALE' },
      '-t' => { long: 'title' },
      '-p' => { long: 'path' },
      '-d' => { long: 'directory', boolean: true },
      '-v' => { long: 'verbose', boolean: true, meth: :on_tail },
      '-h' => { long: 'help', boolean: true, meth: :on_tail },
      '-V' => { long: 'version', boolean: true, meth: :on_tail }
    }.freeze

    # @return [Hash] the possible ~pablo~ subcommands and their
    #   configuration
    PABLO_COMMANDS = {
      'init' => { opts: ['-a', '-l', '-t', '-v', '-h'] },
      'config' => { alias: 'init' },
      'preview' => { opts: ['-h'] },
      'open' => { opts: ['-a', '-l', '-t', '-d', '-p', '-v', '-h'] },
      'edit' => { alias: 'open' },
      'build' => { opts: ['-h'] },
      'publish' => { opts: ['-h'] },
      'help' => { opts: ['-h'] },
      'basic' => { opts: ['-h', '-V'], label: '<command>' }
    }.freeze

    class << self
      # Animates strings in the user console to alert him that something
      #   is running in the background.
      #
      # The animation is chosen among a bunch of themes, with the
      # configuration option ~throbber~ (retrieved via
      # {Neruda::Config#settings}).
      #
      # @example
      #     long_stuff = Thread.new { very_long_operation }
      #     Neruda::Utils.throbber(long_stuff, 'Computing hard stuff:')
      #
      # @param thread [Thread] the long-running operation to decorate
      # @param message [String] the message to display before the throbber
      # @return [void]
      def throbber(thread, message)
        frames = select_throbber_frames
        begin
          run_and_decorate_thread thread, message, frames
        rescue RuntimeError => e
          done = Rainbow('An error occured.').bold.red + "\n"
          done += Rainbow('To see it, run again your command with more ' \
                          'verbosity, i.e. pablo build -v').bold
          warn "#{message} #{done}"
          raise e
        else
          done = Rainbow('done'.ljust(frames[0].length)).green
          puts "#{message} #{done}"
        end
      end

      # Returns the short and long options specification for a given
      #   short option.
      #
      # This method use the {Neruda::Utils::PABLO_OPTIONS} Hash to
      # retrieve corresponding values.
      #
      # @example
      #     spec = Neruda::Utils.decorate_option('-a')
      #     => ['-a AUTHOR', '--author AUTHOR']
      #
      # @param short [String] the short option to decorate
      # @return [Array] the short and long specification for an option
      def decorate_option(short)
        opt = Neruda::Utils::PABLO_OPTIONS[short]
        long = "--#{opt[:long]}"
        return [short, long] if opt[:boolean]
        key = opt[:keyword] || opt[:long].upcase
        [short + key, long + ' ' + key]
      end

      # Returns the ~pablo~ help summary for a given command.
      #
      # @param command [String] the command for which a summary
      #   should be given
      # @return [String]
      def summarize_command(command)
        Neruda::Utils::PABLO_COMMANDS[command][:opts].map do |k|
          short, long = Neruda::Utils.decorate_option(k)
          opt = Neruda::Utils::PABLO_OPTIONS[k]
          line = [('    ' + [short, long].join(', ')).ljust(30)]
          if R18n.t.pablo.options[opt[:long]].translated?
            line << R18n.t.pablo.options[opt[:long]]
          end
          line.join(' ')
        end.join("\n")
      end

      # Returns a formatted list of available commands for ~pablo~.
      #
      # @return [String]
      def list_commands
        lines = []
        Neruda::Utils::PABLO_COMMANDS.each do |cmd, opt|
          next if cmd == 'basic'
          line = ['   ', cmd.ljust(10)]
          if opt.has_key? :alias
            line << R18n.t.pablo.commands.alias(opt[:alias])
          elsif R18n.t.pablo.commands[cmd].translated?
            line << R18n.t.pablo.commands[cmd]
          end
          lines << line.join(' ')
        end
        lines.join("\n")
      end

      # Returns the real command name for a given command, which may be
      #   an alias.
      #
      # @param command [String] the command to resolve
      # @return [String]
      def resolve_possible_alias(command)
        return 'basic' unless Neruda::Utils::PABLO_COMMANDS.include?(command)
        cmd_opt = Neruda::Utils::PABLO_COMMANDS[command]
        return cmd_opt[:alias] if cmd_opt.has_key?(:alias)
        command
      end

      # Try to discover the current host operating system.
      #
      # @return [String] either apple, windows or linux (default)
      def current_os
        if ENV['OS'] == 'Windows_NT' || RUBY_PLATFORM =~ /cygwin/
          return 'windows'
        end
        return 'apple' if RUBY_PLATFORM =~ /darwin/
        'linux'
      end

      private

      def select_throbber_frames
        model = Neruda::Config.settings['throbber'] || 'default'
        model = 'default' unless Neruda::Utils::THROBBER_FRAMES.has_key?(model)
        Neruda::Utils::THROBBER_FRAMES[model]
      end

      def run_and_decorate_thread(thread, message, frames)
        thread.abort_on_exception = true
        current = 0
        while thread.alive?
          sleep 0.1
          print "#{message} #{frames[current % frames.length]}\r"
          current += 1
        end
      end
    end
  end
end
