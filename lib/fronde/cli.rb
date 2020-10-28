# frozen_string_literal: true

require 'rake'
require 'fronde/cli/commands'

module Fronde
  # Fronde CLI app
  class CLI
    def initialize(opts = {})
      @options = { verbose: false }.merge(opts)
      init_rake
    end

    include Fronde::CLICommands

    private

    def init_rake
      init_rakefile unless File.exist?('Rakefile')
      @rake = Rake.application
      Rake.verbose(false) unless @options[:verbose]
      @rake.raw_load_rakefile
    end

    def init_rakefile
      rakefile = <<~RAKE
        # frozen_string_literal: true

        require 'fronde/config'
        require 'r18n-core'

        fronde_spec = Gem::Specification.find_by_name 'fronde'
        R18n.default_places = "\#{fronde_spec.gem_dir}/locales"
        R18n.set(Fronde::Config.settings['lang'] || 'en')
        R18n::Filters.on(:named_variables)

        Dir.glob("\#{fronde_spec.gem_dir}/lib/tasks/*.rake").each { |r| import r }

        task default: 'site:build'
      RAKE
      IO.write 'Rakefile', rakefile
    end
  end
end
