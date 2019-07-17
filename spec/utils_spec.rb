# frozen_string_literal: true

require 'neruda/utils'

describe 'With theoritical pablo arguments' do
  it 'should return decorated options' do
    expect(Neruda::Utils.decorate_option('-a')).to \
      eq(['-a AUTHOR', '--author AUTHOR'])
    expect(Neruda::Utils.decorate_option('-l')).to \
      eq(['-l LOCALE', '--lang LOCALE'])
    expect(Neruda::Utils.decorate_option('-d')).to \
      eq(['-d', '--directory'])
  end

  it 'should summarize basic commands' do
    basic_cmd = [
      "    -h, --help                     Display help for a command and exit\n",
      "    -V, --version                  Display Neruda version and exit\n"
    ].join
    expect(Neruda::Utils.summarize_command('basic')).to eq(basic_cmd)
  end

  it 'should summarize open commands' do
    basic_cmd = [
      "    -a AUTHOR, --author AUTHOR\n",
      "    -l LOCALE, --lang LOCALE\n",
      "    -t TITLE, --title TITLE\n",
      "    -d, --directory                Wrap the new org file in a named folder\n",
      "    -p PATH, --path PATH           Path to the new file\n",
      "    -v, --verbose\n",
      "    -h, --help                     Display help for a command and exit\n"
    ].join
    expect(Neruda::Utils.summarize_command('open')).to eq(basic_cmd)
  end

  it 'should list possible commands' do
    basic_cmd = [
      "    init       Initialize your Neruda instance (you just need to do it once).\n",
      "    preview    Start a test webserver to preview your website on http://127.0.0.1:5000\n",
      "    open       Open or create an org file for edition.\n",
      "    help       Alias for the -h switch.\n"
    ].join
    expect(Neruda::Utils.list_commands).to eq(basic_cmd)
  end
end
