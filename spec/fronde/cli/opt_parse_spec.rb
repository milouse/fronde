# frozen_string_literal: true

require_relative '../../../lib/fronde/cli/opt_parse'

BASIC_CMDS = [
  '    new        Initialize a new Fronde instance.',
  '    init       Alias for ‘new’.',
  '    update     Update Fronde configuration and dependency ' \
  '(to be run after each modification of the config.yml file and ' \
  'once in a while to stay up-to-date with Org).',
  '    config     Alias for ‘update’.',
  '    preview    Start a test web server to preview the generated website.',
  '    open       Open or create an org file.',
  '    edit       Alias for ‘open’.',
  '    build      Compile all org files to HTML or gemtext.',
  '    publish    Push local changes to the public web server.',
  '    help       Alias for the -h switch.'
].join("\n")

BASIC_OPTS = [
  "    -h, --help                 Display help for a command and exit.\n",
  '    -V, --version              Display Fronde version and exit.'
].join

NEW_OPTS = [
  '    -a, --author AUTHOR       ',
  '    -l, --lang LOCALE         ',
  '    -o, --output FORMAT        (gemini, html)',
  '    -t, --title TITLE         ',
  '    -v, --verbose             '
].join("\n")

describe Fronde::CLI::OptParse do
  context 'with theoritical fronde arguments' do
    it 'resolves alias' do
      {
        'new' => 'new',
        'init' => 'new',
        'update' => 'update',
        'config' => 'update',
        'build' => 'build',
        'edit' => 'open',
        'wrong' => 'basic'
      }.each do |command, alias_name|
        expect(described_class.resolve_possible_alias(command)).to \
          eq alias_name
      end
    end

    it 'returns possible options for a command', :aggregate_failures do
      init_opts = described_class.command_options('new')
      expect(init_opts).to have_key(:name)
      expect(init_opts[:name]).to eq('new')
      expect(init_opts).to have_key(:opts)
      expect(init_opts[:opts]).to contain_exactly('-a', '-l', '-o', '-t', '-v')
      expect(init_opts).not_to have_key(:alias)

      config_opts = described_class.command_options('edit')
      expect(config_opts).to have_key(:name)
      expect(config_opts[:name]).to eq('open')
      expect(config_opts).to have_key(:opts)
      expect(config_opts[:opts]).to contain_exactly('-a', '-l', '-t')
      expect(config_opts).not_to have_key(:alias)
    end

    it 'returns decorated options' do
      {
        '-a' => ['-a', '--author AUTHOR'],
        '-l' => ['-l', '--lang LOCALE'],
        '-v' => ['-v', '--verbose'],
        '-o' => ['-o', '--output FORMAT', %w[gemini html]]
      }.each do |option, output|
        expect(described_class.decorate_option(option)).to \
          eq output
      end
    end

    it 'lists possible commands' do
      expect(described_class.list_commands).to eq(BASIC_CMDS)
    end

    it 'summarizes basic command' do
      expect(described_class.summarize_command('basic')).to eq(BASIC_OPTS)
    end

    it 'summarizes new command' do
      expect(described_class.summarize_command('new')).to eq(NEW_OPTS)
    end

    it 'summarizes open command' do
      open_opts = [
        '    -a, --author AUTHOR       ',
        '    -l, --lang LOCALE         ',
        '    -t, --title TITLE         '
      ].join("\n")
      expect(described_class.summarize_command('open')).to eq(open_opts)
    end

    it 'displays help for basic command' do
      help = ['Options', BASIC_OPTS, "\nCommands", BASIC_CMDS].join("\n")
      expect(described_class.help_command_body('basic')).to eq help
    end

    it 'displays help for other commands' do
      {
        'new' => "Options\n#{NEW_OPTS}",
        'preview' => ''
      }.each do |command, help_message|
        expect(described_class.help_command_body(command)).to \
          eq help_message
      end
    end
  end
end
