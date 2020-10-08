# frozen_string_literal: true

require 'neruda/utils'

describe 'With theoritical pablo arguments' do
  it 'should return decorated options' do
    expect(Neruda::Utils.decorate_option('-a')).to \
      eq(['-aAUTHOR', '--author AUTHOR'])
    expect(Neruda::Utils.decorate_option('-l')).to \
      eq(['-lLOCALE', '--lang LOCALE'])
    expect(Neruda::Utils.decorate_option('-d')).to \
      eq(['-d', '--directory'])
  end

  it 'should summarize basic commands' do
    basic_cmd = [
      "    -h, --help                 Display help for a command and exit.\n",
      '    -V, --version              Display Neruda version and exit.'
    ].join
    expect(Neruda::Utils.summarize_command('basic')).to eq(basic_cmd)
  end

  it 'should summarize open commands' do
    basic_cmd = [
      "    -aAUTHOR, --author AUTHOR \n",
      "    -lLOCALE, --lang LOCALE   \n",
      "    -tTITLE, --title TITLE    \n",
      "    -d, --directory            Wrap the new org file in this named folder.\n",
      "    -pPATH, --path PATH        Path to the new file.\n",
      "    -v, --verbose             \n",
      '    -h, --help                 Display help for a command and exit.'
    ].join
    expect(Neruda::Utils.summarize_command('open')).to eq(basic_cmd)
  end

  it 'should list possible commands' do
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
    expect(Neruda::Utils.list_commands).to eq(basic_cmd)
  end

  it 'should resolve alias' do
    expect(Neruda::Utils.resolve_possible_alias('init')).to eq('init')
    expect(Neruda::Utils.resolve_possible_alias('config')).to eq('init')
    expect(Neruda::Utils.resolve_possible_alias('build')).to eq('build')
    expect(Neruda::Utils.resolve_possible_alias('edit')).to eq('open')
    expect(Neruda::Utils.resolve_possible_alias('wrong')).to eq('basic')
  end

  describe 'with config' do
    after(:each) do
      Neruda::Config.load_test({})
    end

    it 'should select the right throbber' do
      Neruda::Config.load_test({})
      frames = Neruda::Utils.send(:select_throbber_frames)
      expect(frames).to(
        eq(['⠁ ⠂ ⠄ ⡀ ⠄ ⠂ ⠁', '⠂ ⠁ ⠂ ⠄ ⡀ ⠄ ⠂', '⠄ ⠂ ⠁ ⠂ ⠄ ⡀ ⠄',
            '⡀ ⠄ ⠂ ⠁ ⠂ ⠄ ⡀', '⠄ ⡀ ⠄ ⠂ ⠁ ⠂ ⠄', '⠂ ⠄ ⡀ ⠄ ⠂ ⠁ ⠂'])
      )
      Neruda::Config.load_test('throbber' => 'default')
      frames = Neruda::Utils.send(:select_throbber_frames)
      expect(frames).to(
        eq(['⠁ ⠂ ⠄ ⡀ ⠄ ⠂ ⠁', '⠂ ⠁ ⠂ ⠄ ⡀ ⠄ ⠂', '⠄ ⠂ ⠁ ⠂ ⠄ ⡀ ⠄',
            '⡀ ⠄ ⠂ ⠁ ⠂ ⠄ ⡀', '⠄ ⡀ ⠄ ⠂ ⠁ ⠂ ⠄', '⠂ ⠄ ⡀ ⠄ ⠂ ⠁ ⠂'])
      )
      Neruda::Config.load_test('throbber' => 'wrong')
      frames = Neruda::Utils.send(:select_throbber_frames)
      expect(frames).to(
        eq(['⠁ ⠂ ⠄ ⡀ ⠄ ⠂ ⠁', '⠂ ⠁ ⠂ ⠄ ⡀ ⠄ ⠂', '⠄ ⠂ ⠁ ⠂ ⠄ ⡀ ⠄',
            '⡀ ⠄ ⠂ ⠁ ⠂ ⠄ ⡀', '⠄ ⡀ ⠄ ⠂ ⠁ ⠂ ⠄', '⠂ ⠄ ⡀ ⠄ ⠂ ⠁ ⠂'])
      )
      Neruda::Config.load_test('throbber' => 'basic')
      frames = Neruda::Utils.send(:select_throbber_frames)
      expect(frames).to eq('-\|/')
    end
  end
end
