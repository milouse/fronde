# frozen_string_literal: true

require 'rake'
require_relative 'cli/commands'

module Fronde
  module CLI
    # Fronde CLI app
    class App
      def initialize(opts = {})
        @options = { verbose: false }.merge(opts)
        Helpers.init_required_files
        @rake = Rake.application
        init_rake
      end

      include Commands

      private

      def init_rake
        Rake.verbose(false) unless @options[:verbose]
        @rake.raw_load_rakefile
      end
    end
  end
end
