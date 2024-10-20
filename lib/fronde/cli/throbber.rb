# frozen_string_literal: true

require 'rainbow'

module Fronde
  module CLI
    # Decorations for the command line
    class Throbber
      # @return [Hash] the possible throbber themes
      THROBBER_FRAMES = {
        'basic' => '-\|/',
        'basicdots' => '⋯⋱⋮⋰',
        'moon' => '🌑🌒🌓🌔🌕🌖🌗🌘',
        'clock' => '🕛🕐🕑🕒🕓🕔🕕🕖🕗🕘🕙🕚',
        'bricks' => '⣷⣯⣟⡿⢿⣻⣽⣾',
        'points' => '·⁘⁛⁘',
        'quadrant' => '▙▛▜▟',
        'default' => ['⠁ ⠂ ⠄ ⡀ ⠄ ⠂ ⠁', '⠂ ⠁ ⠂ ⠄ ⡀ ⠄ ⠂', '⠄ ⠂ ⠁ ⠂ ⠄ ⡀ ⠄',
                      '⡀ ⠄ ⠂ ⠁ ⠂ ⠄ ⡀', '⠄ ⡀ ⠄ ⠂ ⠁ ⠂ ⠄', '⠂ ⠄ ⡀ ⠄ ⠂ ⠁ ⠂']
      }.freeze

      def initialize(thread, message)
        @frames = select_frames
        @term_width = terminal_width
        @thread = thread
        @thread.abort_on_exception = false
        @thread.report_on_exception = false
        @message = message
      end

      def run
        thread_loop
      rescue RuntimeError => e
        show_error
        raise e
        # :nocov: not sure how to emulate a Ctrl+c in rspec
      rescue Interrupt => e
        show_message Rainbow(I18n.t('fronde.bin.interrupted')).red, "\n"
        raise e
        # :nocov:
      else
        show_message Rainbow(I18n.t('fronde.bin.done')).green, "\n"
      end

      class << self
        # Animates strings in the user console to alert him that something
        #   is running in the background.
        #
        # The animation is chosen among a bunch of themes, with the
        # configuration option ~throbber~ (retrieved via
        # {Fronde::Config::Store#get}).
        #
        # @example
        #     long_stuff = Thread.new { very_long_operation }
        #     Fronde::CLI::Throbber.run(long_stuff, 'Computing hard stuff:')
        #
        # @param thread [Thread] the long-running operation to decorate
        # @param message [String] the message to display before the throbber
        # @param verbose [Boolean] whether the decoration should be shown
        #   or skipped
        # @return [void]
        def run(thread, message, verbose)
          if verbose
            thread.join
          else
            throbber = new(thread, message)
            throbber.run
          end
        end
      end

      private

      def thread_loop
        frames_len = @frames.length
        current = 0
        while @thread.alive?
          sleep 0.1
          show_message @frames[current % frames_len]
          current += 1
        end
        @thread.join # Ensure any inner exception is re-raised
      end

      def terminal_width
        # Not a tty. Docker?
        return 0 unless system('test -t 0')

        `stty size`.strip.split[1].to_i - 1
      end

      def show_message(suffix, end_of_line = "\r")
        message = "#{@message} #{suffix}".ljust(@term_width)
        print "#{message}#{end_of_line}"
      end

      def show_error
        warn(
          format(
            "%<message>s %<label>s\n%<explanation>s",
            message: @message,
            label: Rainbow(I18n.t('fronde.error.bin.label')).bold.red,
            explanation: Rainbow(I18n.t('fronde.error.bin.explanation')).bold
          )
        )
      end

      def select_frames
        model = Fronde::CONFIG.get 'throbber', 'default'
        model = 'default' unless THROBBER_FRAMES.has_key?(model)

        THROBBER_FRAMES[model]
      end
    end
  end
end
