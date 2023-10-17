# frozen_string_literal: true

require_relative '../fronde/cli/optparse'

namespace :cli do
  desc 'Generate an autocomplete file for zsh'
  task :zsh_complete do
    data = {}
    all_commands = Fronde::CLI::OptParse::FRONDE_COMMANDS
    data['commands'] = all_commands.filter_map do |command, options|
      next if options[:alias] || command == 'basic'

      opts = (options[:opts] || []).map do |opt|
        opt_config = Fronde::CLI::OptParse::FRONDE_OPTIONS[opt]
        keyword = nil
        unless opt_config[:boolean]
          keyword = opt_config[:keyword] || opt_config[:long].upcase
        end
        { 'short' => opt,
          'long' => "--#{opt_config[:long]}",
          'keyword' => keyword }
      end

      translation = R18n.t.fronde.bin.commands[command].tr("'", '’')
      { 'name' => command,
        'translation' => translation,
        'options' => opts }
    end
    source = File.expand_path '../fronde/cli/data/zsh_completion', __dir__
    template = Liquid::Template.parse(File.read(source))
    puts template.render(data)
  end
end
