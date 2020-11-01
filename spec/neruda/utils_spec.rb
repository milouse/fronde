# frozen_string_literal: true

require 'neruda/utils'

describe Neruda::Utils do
  context 'with theoritical pablo arguments' do
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
        '    -V, --version              Display Neruda version and exit.'
      ].join
      expect(described_class.summarize_command('basic')).to eq(basic_cmd)
    end

    it 'summarizes open commands' do
      basic_cmd = [
        "    -aAUTHOR, --author AUTHOR \n",
        "    -h, --help                 Display help for a command and exit.\n",
        "    -lLOCALE, --lang LOCALE   \n",
        "    -pPATH, --path PATH        Path to the new file.\n",
        "    -tTITLE, --title TITLE    \n",
        '    -v, --verbose             '
      ].join
      expect(described_class.summarize_command('open')).to eq(basic_cmd)
    end

    it 'lists possible commands' do
      basic_cmd = [
        "    init       Initialize your Neruda instance (you just need to do it once).\n",
        "    config     Alias for init.\n",
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
      expect(described_class.resolve_possible_alias('config')).to eq('init')
      expect(described_class.resolve_possible_alias('build')).to eq('build')
      expect(described_class.resolve_possible_alias('edit')).to eq('open')
      expect(described_class.resolve_possible_alias('wrong')).to eq('basic')
    end

    context 'with config' do
      after do
        Neruda::Config.load_test({})
      end

      it 'selects the right throbber' do
        Neruda::Config.load_test({})
        frames = described_class.send(:select_throbber_frames)
        expect(frames).to(
          eq(['⠁ ⠂ ⠄ ⡀ ⠄ ⠂ ⠁', '⠂ ⠁ ⠂ ⠄ ⡀ ⠄ ⠂', '⠄ ⠂ ⠁ ⠂ ⠄ ⡀ ⠄',
              '⡀ ⠄ ⠂ ⠁ ⠂ ⠄ ⡀', '⠄ ⡀ ⠄ ⠂ ⠁ ⠂ ⠄', '⠂ ⠄ ⡀ ⠄ ⠂ ⠁ ⠂'])
        )
        Neruda::Config.load_test('throbber' => 'default')
        frames = described_class.send(:select_throbber_frames)
        expect(frames).to(
          eq(['⠁ ⠂ ⠄ ⡀ ⠄ ⠂ ⠁', '⠂ ⠁ ⠂ ⠄ ⡀ ⠄ ⠂', '⠄ ⠂ ⠁ ⠂ ⠄ ⡀ ⠄',
              '⡀ ⠄ ⠂ ⠁ ⠂ ⠄ ⡀', '⠄ ⡀ ⠄ ⠂ ⠁ ⠂ ⠄', '⠂ ⠄ ⡀ ⠄ ⠂ ⠁ ⠂'])
        )
        Neruda::Config.load_test('throbber' => 'wrong')
        frames = described_class.send(:select_throbber_frames)
        expect(frames).to(
          eq(['⠁ ⠂ ⠄ ⡀ ⠄ ⠂ ⠁', '⠂ ⠁ ⠂ ⠄ ⡀ ⠄ ⠂', '⠄ ⠂ ⠁ ⠂ ⠄ ⡀ ⠄',
              '⡀ ⠄ ⠂ ⠁ ⠂ ⠄ ⡀', '⠄ ⡀ ⠄ ⠂ ⠁ ⠂ ⠄', '⠂ ⠄ ⡀ ⠄ ⠂ ⠁ ⠂'])
        )
        Neruda::Config.load_test('throbber' => 'basic')
        frames = described_class.send(:select_throbber_frames)
        expect(frames).to eq('-\|/')
      end
    end
  end
end
