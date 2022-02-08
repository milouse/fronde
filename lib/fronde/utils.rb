# frozen_string_literal: true

require 'uri'
require 'rainbow'
require 'net/http'
require 'r18n-core'
require 'fronde/config'

module Fronde
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
      # {Fronde::Config#settings}).
      #
      # @example
      #     long_stuff = Thread.new { very_long_operation }
      #     Fronde::Utils.throbber(long_stuff, 'Computing hard stuff:')
      #
      # @param thread [Thread] the long-running operation to decorate
      # @param message [String] the message to display before the throbber
      # @return [void]
      def throbber(thread, message)
        frames = select_throbber_frames
        begin
          run_and_decorate_thread thread, message, frames
        rescue RuntimeError => e
          throbber_error message
          raise e
        else
          done = Rainbow('done'.ljust(frames[0].length)).green
          puts "#{message} #{done}"
        end
      end

      # Returns the short and long options specification for a given
      #   short option.
      #
      # This method use the {Fronde::Utils::FRONDE_OPTIONS} Hash to
      # retrieve corresponding values.
      #
      # @example
      #     spec = Fronde::Utils.decorate_option('-a')
      #     => ['-a AUTHOR', '--author AUTHOR']
      #
      # @param short [String] the short option to decorate
      # @return [Array] the short and long specification for an option
      def decorate_option(short)
        opt = Fronde::Utils::FRONDE_OPTIONS[short]
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
        Fronde::Utils::FRONDE_COMMANDS[command][:opts].map do |k|
          short, long = Fronde::Utils.decorate_option(k)
          opt = Fronde::Utils::FRONDE_OPTIONS[k]
          label = [short, long].join(', ')
          line = [format('    %<opt>s', opt: label).ljust(30)]
          if R18n.t.fronde.bin.options[opt[:long]].translated?
            line << R18n.t.fronde.bin.options[opt[:long]]
          end
          line.join(' ')
        end.join("\n")
      end

      # Returns a formatted list of available commands for ~fronde~.
      #
      # @return [String]
      def list_commands
        lines = []
        Fronde::Utils::FRONDE_COMMANDS.each do |cmd, opt|
          next if cmd == 'basic'
          line = ['   ', cmd.ljust(10)]
          if opt.has_key? :alias
            line << R18n.t.fronde.bin.commands.alias(opt[:alias])
          else
            line << R18n.t.fronde.bin.commands[cmd]
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
        return 'basic' unless Fronde::Utils::FRONDE_COMMANDS.include?(command)
        cmd_opt = Fronde::Utils::FRONDE_COMMANDS[command]
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

      # Try to discover the current host operating system.
      #
      # @return [String] either apple, windows or linux (default)
      # :nocov:
      def current_os
        if ENV['OS'] == 'Windows_NT' || RUBY_PLATFORM.include?('cygwin')
          return 'windows'
        end
        return 'apple' if RUBY_PLATFORM.include?('darwin')
        'linux'
      end
      # :nocov:

      # Download latest org-mode tarball.
      #
      # @param destination [String] where to save the org-mode tarball
      # @return [String] the downloaded org-mode version
      def download_org(destination = 'var/tmp')
        # :nocov:
        return if Fronde::Config.org_last_version.nil?
        # :nocov:
        # Remove version number in dest file to allow easy rake file
        # task naming
        dest_file = File.expand_path('org.tar.gz', destination)
        return if File.exist?(dest_file)
        tarball = "org-mode-release_#{Fronde::Config.org_last_version}.tar.gz"
        uri = URI("https://git.savannah.gnu.org/cgit/emacs/org-mode.git/snapshot/#{tarball}")
        # Will crash on purpose if anything goes wrong
        Net::HTTP.start(uri.host, uri.port, :use_ssl => true) do |http|
          http.request(Net::HTTP::Get.new(uri)) do |response|
            File.open(dest_file, 'w') do |io|
              response.read_body { |chunk| io.write chunk }
            end
          end
        end
      end

      private

      def throbber_error(message)
        warn(
          format(
            "%<message>s %<label>s\n%<explanation>s",
            message: message,
            label: Rainbow(R18n.t.fronde.error.label).bold.red,
            explanation: Rainbow(R18n.t.fronde.error.explanation).bold
          )
        )
      end

      def select_throbber_frames
        model = Fronde::Config.settings['throbber'] || 'default'
        model = 'default' unless Fronde::Utils::THROBBER_FRAMES.has_key?(model)
        Fronde::Utils::THROBBER_FRAMES[model]
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
