# frozen_string_literal: true

require 'rainbow'
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
      '-p' => { long: 'path', desc: 'Path to the new file' },
      '-d' => { long: 'directory', boolean: true,
                desc: 'Wrap the new org file in a named folder' },
      '-v' => { long: 'verbose', boolean: true, meth: :on_tail },
      '-h' => { long: 'help', boolean: true, meth: :on_tail,
                desc: 'Display help for a command and exit' },
      '-V' => { long: 'version', boolean: true, meth: :on_tail,
                desc: 'Display Neruda version and exit' }
    }.freeze

    # @return [Hash] the possible ~pablo~ subcommands and their
    #   configuration
    PABLO_COMMANDS = {
      'init' => { opts: ['-a', '-l', '-t', '-v', '-h'],
                  desc: 'Initialize your Neruda instance ' \
                        '(you just need to do it once).' },
      'preview' => { opts: ['-h'],
                     desc: 'Start a test webserver to preview ' \
                           'your website on http://127.0.0.1:5000' },
      'open' => { opts: ['-a', '-l', '-t', '-d', '-p', '-v', '-h'],
                  desc: 'Open or create an org file for edition.' },
      'help' => { opts: ['-h'], desc: 'Alias for the -h switch.' },
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
        model = Neruda::Config.settings['throbber'] || 'default'
        model = 'default' unless Neruda::Utils::THROBBER_FRAMES.has_key?(model)
        frames = Neruda::Utils::THROBBER_FRAMES[model]
        current = 0
        while thread.alive?
          sleep 0.1
          print "#{message} #{frames[current % frames.length]}\r"
          current += 1
        end
        done = Rainbow('done'.ljust(frames[0].length)).green
        puts "#{message} #{done}"
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
          line = '    ' + [short, long].join(', ')
          line = line.ljust(34) + " #{opt[:desc]}" if opt.has_key?(:desc)
          line + "\n"
        end.join
      end

      # Returns a formatted list of available commands for ~pablo~.
      #
      # @return [String]
      def list_commands
        lines = ''
        Neruda::Utils::PABLO_COMMANDS.each do |cmd, opt|
          next if cmd == 'basic'
          lines += "    #{cmd.ljust(10)} #{opt[:desc]}\n"
        end
        lines
      end
    end
  end
end
