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
    FileUtils.mkdir_p 'tmp/config'
    IO.write('tmp/config/config.yml', SAMPLE_CONFIG)
    Dir.chdir 'tmp/config'
  end

  before(:each) do
    # When run with all other specs, config may have been already loaded
    Neruda::Config.send(:load_settings)
  end

  after(:each) do
    Neruda::Config.load_test({})
  end

  after(:all) do
    Dir.chdir File.expand_path('..', __dir__)
    FileUtils.rm_r 'tmp/config'
  end

  it 'should parse it successfully' do
    # Be sure to unset @config to force parsing
    Neruda::Config.class_eval '@config = nil', __FILE__, __LINE__
    conf = Neruda::Config.settings
    expect(conf['author']).to eq('Tata')
    expect(conf['title']).to eq('This is a website about test')
    expect(conf['lang']).to eq('en')
    expect(conf['theme']).to eq('default')
    expect(conf['domain']).to eq('')
  end

  it 'should save it successfully' do
    Neruda::Config.save('author' => 'Titi', 'title' => 'Nevermind',
                        'domain' => 'https://tutu.com')
    expect(IO.read('config.yml')).to eq(SAMPLE_CONFIG_2)
    conf = Neruda::Config.settings
    expect(conf['author']).to eq('Titi')
    expect(conf['title']).to eq('Nevermind')
    expect(conf['lang']).to eq('en')
    expect(conf['theme']).to eq('default')
    expect(conf['domain']).to eq('https://tutu.com')
  end
end

SAMPLE_CONFIG_3 = <<~CONF
  ---
  author: Tata
  title: This is a website with a blog
  sources:
  - src
  - path: src/news
    is_blog: true
  - wrong: source
CONF

SAMPLE_CONFIG_4 = <<~CONF
  ---
  author: Tata
  title: This is a website with a blog
  domain: https://test.com
  sources:
  - src
  - path: news
    is_blog: true
    theme: my-theme
  - path: other
    theme: my-theme
CONF

describe 'With a blog config file' do
  before(:all) do
    FileUtils.mkdir_p 'tmp/config'
    Dir.chdir 'tmp/config'
  end

  after(:each) do
    Neruda::Config.load_test({})
  end

  after(:all) do
    Dir.chdir File.expand_path('..', __dir__)
    FileUtils.rm_r 'tmp/config'
  end

  it 'should list sources' do
    IO.write('config.yml', SAMPLE_CONFIG_3)
    Neruda::Config.send(:load_settings)
    expect(Neruda::Config.sources.length).to eq(2)
    expect(Neruda::Config.sources[0]['name']).to eq('src')
    expect(Neruda::Config.sources[0]['path']).to(
      eq(File.expand_path('src'))
    )
    expect(Neruda::Config.sources[0]['target']).to eq('src')
    expect(Neruda::Config.sources[0]['is_blog']).to be(false)
    expect(Neruda::Config.sources[0]['recursive']).to be(true)
    expect(Neruda::Config.sources[1]['name']).to eq('news')
    expect(Neruda::Config.sources[1]['path']).to(
      eq(File.expand_path('src/news'))
    )
    expect(Neruda::Config.sources[1]['target']).to eq('news')
    expect(Neruda::Config.sources[1]['is_blog']).to be(true)
    expect(Neruda::Config.sources[1]['recursive']).to be(true)
  end

  it 'should generate projects hash' do
    IO.write('config.yml', SAMPLE_CONFIG_3)
    Neruda::Config.send(:load_settings)
    projects = Neruda::Config.send(:org_generate_projects)
    expect(projects).to have_key('news')
  end

  it 'should expose correct head header' do
    IO.write('config.yml', SAMPLE_CONFIG_3)
    Neruda::Config.send(:load_settings)
    projects = Neruda::Config.sources
    headers = Neruda::Config.send(:build_project_org_headers, projects[0])
    head = <<~HEAD
      :html-head "<link rel=\\\"stylesheet\\\" type=\\\"text/css\\\" media=\\\"screen\\\"
            href=\\\"/assets/default/css/style.css\\\">
      <link rel=\\\"stylesheet\\\" type=\\\"text/css\\\" media=\\\"screen\\\"
            href=\\\"/assets/default/css/htmlize.css\\\">"
       :html-postamble "<p><span class=\\\"author\\\">Written by %a</span>
      with %c, and published with %N</p>
      <p class=\\\"date\\\">Last modification on %C</p>
      <p class=\\\"validation\\\">%v</p>"
       :html-head-include-default-style t
       :html-head-include-scripts nil
    HEAD
    expect(headers).to eq(head.strip)
  end

  it 'should expose correct head header with custom domain' do
    IO.write('config.yml', SAMPLE_CONFIG_4)
    Neruda::Config.send(:load_settings)
    projects = Neruda::Config.sources
    headers = Neruda::Config.send(:build_project_org_headers, projects[0])
    head = <<~HEAD
      :html-head "<link rel=\\\"stylesheet\\\" type=\\\"text/css\\\" media=\\\"screen\\\"
            href=\\\"https://test.com/assets/default/css/style.css\\\">
      <link rel=\\\"stylesheet\\\" type=\\\"text/css\\\" media=\\\"screen\\\"
            href=\\\"https://test.com/assets/default/css/htmlize.css\\\">"
       :html-postamble "<p><span class=\\\"author\\\">Written by %a</span>
      with %c, and published with %N</p>
      <p class=\\\"date\\\">Last modification on %C</p>
      <p class=\\\"validation\\\">%v</p>"
       :html-head-include-default-style t
       :html-head-include-scripts nil
    HEAD
    expect(headers).to eq(head.strip)
    headers = Neruda::Config.send(:build_project_org_headers, projects[1])
    head = <<~HEAD
      :html-head "<link rel=\\\"stylesheet\\\" type=\\\"text/css\\\" media=\\\"screen\\\"
            href=\\\"https://test.com/assets/my-theme/css/style.css\\\">
      <link rel=\\\"stylesheet\\\" type=\\\"text/css\\\" media=\\\"screen\\\"
            href=\\\"https://test.com/assets/my-theme/css/htmlize.css\\\">
      <link rel=\\\"alternate\\\" type=\\\"application/atom+xml\\\" title=\\\"Atom 1.0\\\"
            href=\\\"https://test.com/feeds/index.xml\\\" />"
       :html-postamble "<p><span class=\\\"author\\\">Written by %a</span>
      with %c, and published with %N</p>
      <p class=\\\"date\\\">Last modification on %C</p>
      <p class=\\\"validation\\\">%v</p>"
       :html-head-include-default-style t
       :html-head-include-scripts nil
    HEAD
    expect(headers).to eq(head.strip)
  end

  it 'should generate projects' do
    IO.write('config.yml', SAMPLE_CONFIG_4)
    Neruda::Config.send(:load_settings)
    projects = Neruda::Config.send(:org_generate_projects)
    srcconf = <<~SRCCONF
      ("src"
       :base-directory "#{Dir.pwd}/src"
       :base-extension "org"
       :recursive t
       :publishing-directory "#{Dir.pwd}/public_html/src"
       :publishing-function org-html-publish-to-html
       :section-numbers nil
       :with-toc nil
       :html-head "<link rel=\\\"stylesheet\\\" type=\\\"text/css\\\" media=\\\"screen\\\"
            href=\\\"https://test.com/assets/default/css/style.css\\\">
      <link rel=\\\"stylesheet\\\" type=\\\"text/css\\\" media=\\\"screen\\\"
            href=\\\"https://test.com/assets/default/css/htmlize.css\\\">"
       :html-postamble "<p><span class=\\\"author\\\">Written by %a</span>
      with %c, and published with %N</p>
      <p class=\\\"date\\\">Last modification on %C</p>
      <p class=\\\"validation\\\">%v</p>"
       :html-head-include-default-style t
       :html-head-include-scripts nil)
      ("src-assets"
       :base-directory "#{Dir.pwd}/src"
       :base-extension "jpg\\\\\\|gif\\\\\\|png\\\\\\|svg\\\\\\|pdf"
       :recursive t
       :publishing-directory "#{Dir.pwd}/public_html/src"
       :publishing-function org-publish-attachment)

    SRCCONF
    blogconf = <<~BLOGCONF
      ("news"
       :base-directory "#{Dir.pwd}/news"
       :base-extension "org"
       :recursive t
       :publishing-directory "#{Dir.pwd}/public_html/news"
       :publishing-function org-html-publish-to-html
       :section-numbers nil
       :with-toc nil
       :html-head "<link rel=\\\"stylesheet\\\" type=\\\"text/css\\\" media=\\\"screen\\\"
            href=\\\"https://test.com/assets/my-theme/css/style.css\\\">
      <link rel=\\\"stylesheet\\\" type=\\\"text/css\\\" media=\\\"screen\\\"
            href=\\\"https://test.com/assets/my-theme/css/htmlize.css\\\">
      <link rel=\\\"alternate\\\" type=\\\"application/atom+xml\\\" title=\\\"Atom 1.0\\\"
            href=\\\"https://test.com/feeds/index.xml\\\" />"
       :html-postamble "<p><span class=\\\"author\\\">Written by %a</span>
      with %c, and published with %N</p>
      <p class=\\\"date\\\">Last modification on %C</p>
      <p class=\\\"validation\\\">%v</p>"
       :html-head-include-default-style t
       :html-head-include-scripts nil)
      ("news-assets"
       :base-directory "#{Dir.pwd}/news"
       :base-extension "jpg\\\\\\|gif\\\\\\|png\\\\\\|svg\\\\\\|pdf"
       :recursive t
       :publishing-directory "#{Dir.pwd}/public_html/news"
       :publishing-function org-publish-attachment)
      ("theme-my-theme"
       :base-directory "#{Dir.pwd}/themes/my-theme"
       :base-extension "jpg\\\\\\|gif\\\\\\|png\\\\\\|js\\\\\\|css\\\\\\|otf\\\\\\|ttf\\\\\\|woff2?"
       :recursive t
       :publishing-directory "#{Dir.pwd}/public_html/assets/my-theme"
       :publishing-function org-publish-attachment)

    BLOGCONF
    expect(projects).to have_key('src')
    expect(projects).to have_key('news')
    expect(projects['src']).to eq(srcconf)
    expect(projects['news']).to eq(blogconf)
  end

  it 'should generate projects names list' do
    IO.write('config.yml', SAMPLE_CONFIG_4)
    Neruda::Config.send(:load_settings)
    projects = Neruda::Config.send(:org_generate_projects)
    expect(Neruda::Config.send(:project_names, projects)).to(
      eq('"src" "src-assets" "news" "news-assets" "other" "other-assets" "theme-default" "theme-my-theme"')
    )
  end
end

describe 'Without a config file' do
  before(:all) do
    FileUtils.mkdir_p 'tmp/config2'
    Dir.chdir 'tmp/config2'
  end

  before(:each) do
    # When run with all other specs, config may have been already loaded
    Neruda::Config.send(:load_settings)
  end

  after(:each) do
    Neruda::Config.load_test({})
  end

  after(:all) do
    Dir.chdir File.expand_path('..', __dir__)
    FileUtils.rm_r 'tmp/config2'
  end

  it 'should use default config' do
    conf = Neruda::Config.settings
    author = ENV['USER'] || ''
    expect(conf['author']).to eq(author)
    expect(conf['lang']).to eq('en') # Defined in ENV in requirements.rb
    expect(conf['public_folder']).to eq('public_html')
    expect(conf['domain']).to eq('')
  end

  it 'should handle weird case' do
    expect(Neruda::Config.send(:build_source, 'name' => 'wrong')).to be(nil)
    expect(Neruda::Config.send(:build_source, :test)).to be(nil)
  end
end

describe 'With config utils method' do
  after(:each) do
    FileUtils.rm 'tmp/__last_org_version__', force: true
    Neruda::Config.class_eval '@org_version = nil', __FILE__, __LINE__
  end

  it 'should give current org version' do
    Neruda::Config.class_eval '@org_version = nil', __FILE__, __LINE__
    IO.write('tmp/__last_org_version__', 'test')
    expect(Neruda::Config.org_last_version).to eq('test')
  end
end
