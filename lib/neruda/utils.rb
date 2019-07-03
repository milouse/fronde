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
      'bricks' => '⣾⣽⣻⢿⡿⣟⣯⣷',
      'points' => '·⁘∷⁛∷⁘',
      'quadrant2' => '▙▛▜▟',
      'default' => ['⠁ ⠂ ⠄ ⡀ ⠄ ⠂ ⠁', '⠂ ⠁ ⠂ ⠄ ⡀ ⠄ ⠂', '⠄ ⠂ ⠁ ⠂ ⠄ ⡀ ⠄',
                    '⡀ ⠄ ⠂ ⠁ ⠂ ⠄ ⡀', '⠄ ⡀ ⠄ ⠂ ⠁ ⠂ ⠄', '⠂ ⠄ ⡀ ⠄ ⠂ ⠁ ⠂']
    }.freeze

    class << self
      # Animates strings in the user console to alert him that something
      #   is running in the background.
      #
      # The animation is chosen among a bunch of themes, with the
      # configuration option `throbber` (retrieved via
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
    end
  end
end
