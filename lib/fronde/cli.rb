# frozen_string_literal: true

require 'rake'
require_relative 'cli/commands'

module Fronde
  module CLI
    # Fronde CLI app
    class App
      def initialize(opts = {})
        @options = { verbose: false }.merge(opts)
        @command = @rake = nil
        @argv = []
      end

      include Commands

      def run(argv)
        @argv = argv
        @command = OptParse.resolve_possible_alias(@argv.shift || 'basic')
        if help_param_given?
          return 2 if @options[:recover_from_error]
          return 0
        end

        init_rake if %w[build preview publish].include?(@command)

        method = "fronde_#{@command}".to_sym
        return 2 if method_unknown?(method)

        send method
      end

      private

      def init_rake
        @rake = Rake.application
        Rake.verbose(false) unless @options[:verbose]
        @rake.raw_load_rakefile
      end

      def help_param_given?
        return false unless @options[:help]

        fronde_help
        true
      end

      def method_unknown?(method)
        return false if respond_to?(method)

        warn R18n.t.fronde.error.bin.no_command
        fronde_help
        true
      end
    end
  end
end
