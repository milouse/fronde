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
  domain: https://tutu.com
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

  it 'should expose correct head header' do
    head = <<~HEAD
      <link rel="stylesheet" type="text/css" media="screen"
            href="/assets/css/style.css">
      <link rel="stylesheet" type="text/css" media="screen"
            href="/assets/css/htmlize.css">
    HEAD
    expect(Neruda::Config.send(:build_html_head)).to eq(head)
  end

  it 'should save it successfully' do
    Neruda::Config.save('author' => 'Titi', 'title' => 'Nevermind',
                        'domain' => 'https://tutu.com')
    expect(IO.read('config.yml')).to eq(SAMPLE_CONFIG_2)
  end

  it 'should expose correct head header' do
    head = <<~HEAD
      <link rel="stylesheet" type="text/css" media="screen"
            href="https://tutu.com/assets/css/style.css">
      <link rel="stylesheet" type="text/css" media="screen"
            href="https://tutu.com/assets/css/htmlize.css">
    HEAD
    expect(Neruda::Config.send(:build_html_head)).to eq(head)
  end
end

SAMPLE_CONFIG_3 = <<~CONF
  ---
  author: Tata
  title: This is a website with a blog
  blog_path: news
CONF

SAMPLE_CONFIG_4 = <<~CONF
  ---
  author: Tata
  title: This is a website with a blog
  blog_path: news
  domain: https://test.com
CONF

describe 'With a blog config file' do
  before(:all) do
    FileUtils.mkdir_p 'spec/data/config'
    Dir.chdir 'spec/data/config'
  end

  after(:all) do
    Dir.chdir File.expand_path('../', __dir__)
    FileUtils.rm_r 'spec/data/config'
  end

  it 'should expose correct head header' do
    IO.write('config.yml', SAMPLE_CONFIG_3)
    Neruda::Config.send(:load_settings)
    head = <<~HEAD
      <link rel="stylesheet" type="text/css" media="screen"
            href="/assets/css/style.css">
      <link rel="stylesheet" type="text/css" media="screen"
            href="/assets/css/htmlize.css">
      <link rel="alternate" type="application/atom+xml" title="Atom 1.0"
            href="/feeds/index.xml" />
    HEAD
    expect(Neruda::Config.send(:build_html_head)).to eq(head)
  end

  it 'should expose correct head header with custom domain' do
    IO.write('config.yml', SAMPLE_CONFIG_4)
    Neruda::Config.send(:load_settings)
    head = <<~HEAD
      <link rel="stylesheet" type="text/css" media="screen"
            href="https://test.com/assets/css/style.css">
      <link rel="stylesheet" type="text/css" media="screen"
            href="https://test.com/assets/css/htmlize.css">
      <link rel="alternate" type="application/atom+xml" title="Atom 1.0"
            href="https://test.com/feeds/index.xml" />
    HEAD
    expect(Neruda::Config.send(:build_html_head)).to eq(head)
  end
end
