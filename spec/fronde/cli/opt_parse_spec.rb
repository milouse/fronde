# frozen_string_literal: true

require_relative '../../../lib/fronde/cli/opt_parse'

BASIC_CMDS = [
  '    new        Initialize your Fronde instance (you just need to do it once).',
  '    init       Alias for ‘new’.',
  '    update     Update Fronde dependency (to be run once in a while).',
  '    config     Alias for ‘update’.',
  '    preview    Start a test web server to preview your website on http://127.0.0.1:5000',
  '    open       Open or create an org file.',
  '    edit       Alias for ‘open’.',
  '    build      Compile your org files to HTML or gemtext.',
  '    publish    Push local changes to your public web server.',
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
      expect(described_class.resolve_possible_alias('new')).to eq('new')
      expect(described_class.resolve_possible_alias('init')).to eq('new')
      expect(described_class.resolve_possible_alias('update')).to eq('update')
      expect(described_class.resolve_possible_alias('config')).to eq('update')
      expect(described_class.resolve_possible_alias('build')).to eq('build')
      expect(described_class.resolve_possible_alias('edit')).to eq('open')
      expect(described_class.resolve_possible_alias('wrong')).to eq('basic')
    end

    it 'returns possible options for a command' do
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
      expect(config_opts[:opts]).to contain_exactly('-a', '-l', '-t', '-v')
      expect(config_opts).not_to have_key(:alias)
    end

    it 'returns decorated options' do
      expect(described_class.decorate_option('-a')).to(
        eq(['-a', '--author AUTHOR'])
      )
      expect(described_class.decorate_option('-l')).to(
        eq(['-l', '--lang LOCALE'])
      )
      expect(described_class.decorate_option('-v')).to(
        eq(['-v', '--verbose'])
      )
      expect(described_class.decorate_option('-o')).to(
        eq(['-o', '--output FORMAT', %w[gemini html]])
      )
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
        '    -t, --title TITLE         ',
        '    -v, --verbose             '
      ].join("\n")
      expect(described_class.summarize_command('open')).to eq(open_opts)
    end

    it 'displays help for basic command' do
      help = ['Options', BASIC_OPTS, "\nCommands", BASIC_CMDS].join("\n")
      expect(described_class.help_command_body('basic')).to eq help
    end

    it 'displays help for other commands' do
      expect(described_class.help_command_body('new')).to(
        eq "Options\n#{NEW_OPTS}"
      )
      expect(described_class.help_command_body('update')).to eq ''
    end
  end
end
