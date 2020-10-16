# frozen_string_literal: true

require 'uri'
require 'rainbow'
require 'net/http'
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
      '-d' => { long: 'directory', boolean: true },
      '-f' => { long: 'force', boolean: true },
      '-h' => { long: 'help', boolean: true, meth: :on_tail },
      '-l' => { long: 'lang', keyword: 'LOCALE' },
      '-p' => { long: 'path' },
      '-t' => { long: 'title' },
      '-v' => { long: 'verbose', boolean: true, meth: :on_tail },
      '-V' => { long: 'version', boolean: true, meth: :on_tail }
    }.freeze

    # @return [Hash] the possible ~pablo~ subcommands and their
    #   configuration
    PABLO_COMMANDS = {
      'init' => { opts: ['-a', '-h', '-l', '-t', '-v'] },
      'config' => { alias: 'init' },
      'preview' => { opts: ['-h'] },
      'open' => { opts: ['-a', '-d', '-h', '-l', '-p', '-t', '-v'] },
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
        [short + key, format('%<long>s %<key>s', long: long, key: key)]
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
          label = [short, long].join(', ')
          line = [format('    %<opt>s', opt: label).ljust(30)]
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
          else
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
      # @return [String] the downloaded org-mode version
      def download_org
        # :nocov:
        return if Neruda::Config.org_last_version.nil?
        # :nocov:
        tarball = "org-#{Neruda::Config.org_last_version}.tar.gz"
        # Remove version number in dest file to allow easy rake file
        # task naming
        dest_file = 'tmp/org.tar.gz'
        return if File.exist?(dest_file)
        uri = URI("https://orgmode.org/#{tarball}")
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
            label: Rainbow(R18n.t.neruda.error.label).bold.red,
            explanation: Rainbow(R18n.t.neruda.error.explanation).bold
          )
        )
      end

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
