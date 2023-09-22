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
        @thread = thread
        @thread.abort_on_exception = true
        @message = message
      end

      def run
        terminal_width = `tput cols`.strip.to_i - 1
        frames_len = @frames.length
        current = 0
        while @thread.alive?
          sleep 0.1
          frame = @frames[current % frames_len]
          message = "#{@message} #{frame}"
          print "#{message.ljust(terminal_width)}\r"
          current += 1
        end
      rescue RuntimeError => e
        show_error
        raise e
      else
        puts "#{@message} #{Rainbow('done').green}".ljust(terminal_width)
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
        # @return [void]
        def run(thread, message)
          throbber = new(thread, message)
          throbber.run
        end
      end

      private

      def show_error
        warn(
          format(
            "%<message>s %<label>s\n%<explanation>s",
            message: @message,
            label: Rainbow(R18n.t.fronde.error.label).bold.red,
            explanation: Rainbow(R18n.t.fronde.error.explanation).bold
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
