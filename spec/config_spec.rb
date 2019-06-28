# frozen_string_literal: true

SAMPLE_CONFIG = <<~CONF
  ---
  author: Tata
  title: This is a website about test
CONF

SAMPLE_CONFIG_2 = <<~CONF2
  ---
  author: Titi
  title: Nevermind
CONF2

describe 'With a config file' do
  before(:all) do
    FileUtils.mkdir_p 'spec/data/config'
    IO.write('spec/data/config/config.yml', SAMPLE_CONFIG)
    Dir.chdir 'spec/data/config'
    # When run with all other specs, config may have been already loaded
    Neruda::Config.send(:load_settings)
  end

  after(:all) do
    Dir.chdir File.expand_path('../', __dir__)
    FileUtils.rm_r 'spec/data/config'
  end

  it 'should parse it successfully' do
    conf = Neruda::Config.settings
    expect(conf['author']).to eq('Tata')
    expect(conf['title']).to eq('This is a website about test')
  end

  it 'should save it successfully' do
    Neruda::Config.save('author' => 'Titi', 'title' => 'Nevermind')
    expect(IO.read('config.yml')).to eq(SAMPLE_CONFIG_2)
  end
end
