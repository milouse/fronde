# frozen_string_literal: true

require 'fronde/utils'

describe Fronde::Utils do
  context 'with theoritical fronde arguments' do
    it 'returns decorated options' do
      expect(described_class.decorate_option('-a')).to(
        eq(['-aAUTHOR', '--author AUTHOR'])
      )
      expect(described_class.decorate_option('-l')).to(
        eq(['-lLOCALE', '--lang LOCALE'])
      )
      expect(described_class.decorate_option('-v')).to(
        eq(['-v', '--verbose'])
      )
    end

    it 'summarizes basic commands' do
      basic_cmd = [
        "    -h, --help                 Display help for a command and exit.\n",
        '    -V, --version              Display Fronde version and exit.'
      ].join
      expect(described_class.summarize_command('basic')).to eq(basic_cmd)
    end

    it 'summarizes open commands' do
      basic_cmd = [
        "    -aAUTHOR, --author AUTHOR \n",
        "    -h, --help                 Display help for a command and exit.\n",
        "    -lLOCALE, --lang LOCALE   \n",
        "    -tTITLE, --title TITLE    \n",
        '    -v, --verbose             '
      ].join
      expect(described_class.summarize_command('open')).to eq(basic_cmd)
    end

    it 'lists possible commands' do
      basic_cmd = [
        "    init       Initialize your Fronde instance (you just need to do it once).\n",
        "    update     Update Fronde dependency (to be run once in a while).\n",
        "    config     Alias for update.\n",
        "    preview    Start a test web server to preview your website on http://127.0.0.1:5000\n",
        "    open       Open or create an org file.\n",
        "    edit       Alias for open.\n",
        "    build      Compile your org files to HTML.\n",
        "    publish    Push local changes to your public web server.\n",
        '    help       Alias for the -h switch.'
      ].join
      expect(described_class.list_commands).to eq(basic_cmd)
    end

    it 'resolves alias' do
      expect(described_class.resolve_possible_alias('init')).to eq('init')
      expect(described_class.resolve_possible_alias('update')).to eq('update')
      expect(described_class.resolve_possible_alias('config')).to eq('update')
      expect(described_class.resolve_possible_alias('build')).to eq('build')
      expect(described_class.resolve_possible_alias('edit')).to eq('open')
      expect(described_class.resolve_possible_alias('wrong')).to eq('basic')
    end

    it 'returns possible options for a command' do
      init_opts = described_class.command_options('init')
      expect(init_opts).to have_key(:name)
      expect(init_opts[:name]).to eq('init')
      expect(init_opts).to have_key(:opts)
      expect(init_opts[:opts]).to contain_exactly('-a', '-h', '-l', '-t', '-v')
      expect(init_opts).not_to have_key(:alias)

      config_opts = described_class.command_options('config')
      expect(config_opts).to have_key(:name)
      expect(config_opts[:name]).to eq('update')
      expect(config_opts).to have_key(:opts)
      expect(config_opts[:opts]).to contain_exactly('-a', '-h', '-l', '-t', '-v')
      expect(config_opts).not_to have_key(:alias)
    end

    context 'with config' do
      before do
        Fronde::Config.reset
      end

      it 'selects the right throbber' do
        frames = described_class.send(:select_throbber_frames)
        expect(frames).to(
          eq(['⠁ ⠂ ⠄ ⡀ ⠄ ⠂ ⠁', '⠂ ⠁ ⠂ ⠄ ⡀ ⠄ ⠂', '⠄ ⠂ ⠁ ⠂ ⠄ ⡀ ⠄',
              '⡀ ⠄ ⠂ ⠁ ⠂ ⠄ ⡀', '⠄ ⡀ ⠄ ⠂ ⠁ ⠂ ⠄', '⠂ ⠄ ⡀ ⠄ ⠂ ⠁ ⠂'])
        )
        Fronde::Config.load_test('throbber' => 'default')
        frames = described_class.send(:select_throbber_frames)
        expect(frames).to(
          eq(['⠁ ⠂ ⠄ ⡀ ⠄ ⠂ ⠁', '⠂ ⠁ ⠂ ⠄ ⡀ ⠄ ⠂', '⠄ ⠂ ⠁ ⠂ ⠄ ⡀ ⠄',
              '⡀ ⠄ ⠂ ⠁ ⠂ ⠄ ⡀', '⠄ ⡀ ⠄ ⠂ ⠁ ⠂ ⠄', '⠂ ⠄ ⡀ ⠄ ⠂ ⠁ ⠂'])
        )
        Fronde::Config.load_test('throbber' => 'wrong')
        frames = described_class.send(:select_throbber_frames)
        expect(frames).to(
          eq(['⠁ ⠂ ⠄ ⡀ ⠄ ⠂ ⠁', '⠂ ⠁ ⠂ ⠄ ⡀ ⠄ ⠂', '⠄ ⠂ ⠁ ⠂ ⠄ ⡀ ⠄',
              '⡀ ⠄ ⠂ ⠁ ⠂ ⠄ ⡀', '⠄ ⡀ ⠄ ⠂ ⠁ ⠂ ⠄', '⠂ ⠄ ⡀ ⠄ ⠂ ⠁ ⠂'])
        )
        Fronde::Config.load_test('throbber' => 'basic')
        frames = described_class.send(:select_throbber_frames)
        expect(frames).to eq('-\|/')
      end
    end
  end
end
