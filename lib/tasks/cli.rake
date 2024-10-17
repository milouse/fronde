# frozen_string_literal: true

require_relative '../fronde/cli/opt_parse'

def comp_opt_to_liquid(option, command)
  opt_config = Fronde::CLI::OptParse::FRONDE_OPTIONS[option]
  keyword = nil
  unless opt_config[:boolean]
    keyword = opt_config[:keyword] || opt_config[:long].upcase
  end
  {
    'command' => command,
    'short' => option,
    'short_no_dash' => option.delete_prefix('-'),
    'long' => "--#{opt_config[:long]}",
    'long_no_dash' => opt_config[:long],
    'keyword' => keyword,
    'choices' => opt_config[:choices],
    'help' => opt_config[:help]
  }
end

namespace :cli do
  desc 'Generate an autocomplete file for zsh'
  task :zsh_complete do
    data = {}
    all_commands = Fronde::CLI::OptParse::FRONDE_COMMANDS
    data['commands'] = all_commands.filter_map do |command, options|
      next if options[:alias] || command == 'basic'

      opts = (options[:opts] || []).map { comp_opt_to_liquid(_1, command) }
      { 'name' => command,
        'translation' => I18n.t("fronde.bin.commands.#{command}"),
        'options' => opts }
    end
    source = File.expand_path '../fronde/cli/data/zsh_completion', __dir__
    template = Liquid::Template.parse(File.read(source))
    puts template.render(data)
  end

  desc 'Generate an autocomplete file for fish'
  task :fish_complete do
    data = { 'commands' => [], 'details' => [] }
    all_commands = []
    Fronde::CLI::OptParse::FRONDE_COMMANDS.each do |command, options|
      next if options[:alias]

      data['details'] += (options[:opts] || []).map do |opt|
        comp_opt_to_liquid opt, command
      end
      next if command == 'basic'

      data['commands'] << command

      help = I18n.t("fronde.bin.commands.#{command}")
      all_commands << "#{command}\\t'#{help}'"
    end

    data['comcomp'] = all_commands.join('\n')

    source = File.expand_path '../fronde/cli/data/fish_completion', __dir__
    template = Liquid::Template.parse(File.read(source))
    puts template.render(data)
  end
end
