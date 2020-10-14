# frozen_string_literal: true

SAMPLE_CONFIG = <<~CONF
  ---
  author: Tata
  title: This is a website about test
CONF

SAMPLE_CONFIG_2 = <<~CONF
  ---
  author: Titi
  title: Nevermind
  domain: https://tutu.com
CONF

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

describe Neruda::Config do
  context 'with a config file' do
    before do
      FileUtils.mkdir_p 'tmp/config'
      IO.write('tmp/config/config.yml', SAMPLE_CONFIG)
      Dir.chdir 'tmp/config'
      described_class.send(:load_settings)
    end

    after do
      described_class.load_test({})
      Dir.chdir File.expand_path('../..', __dir__)
      FileUtils.rm_r 'tmp/config'
    end

    it 'parses it successfully' do
      # Be sure to unset @config to force parsing
      described_class.class_eval '@config = nil', __FILE__, __LINE__
      conf = described_class.settings
      expect(conf['author']).to eq('Tata')
      expect(conf['title']).to eq('This is a website about test')
      expect(conf['lang']).to eq('en')
      expect(conf['theme']).to eq('default')
      expect(conf['domain']).to eq('')
    end

    it 'saves it successfully' do
      described_class.save('author' => 'Titi', 'title' => 'Nevermind',
                           'domain' => 'https://tutu.com')
      expect(IO.read('config.yml')).to eq(SAMPLE_CONFIG_2)
      conf = described_class.settings
      expect(conf['author']).to eq('Titi')
      expect(conf['title']).to eq('Nevermind')
      expect(conf['lang']).to eq('en')
      expect(conf['theme']).to eq('default')
      expect(conf['domain']).to eq('https://tutu.com')
    end
  end

  context 'with a blog config file' do
    before do
      FileUtils.mkdir_p 'tmp/config'
      Dir.chdir 'tmp/config'
      described_class.load_test({})
    end

    after do
      Dir.chdir File.expand_path('../..', __dir__)
      FileUtils.rm_r 'tmp/config'
    end

    it 'lists sources' do
      IO.write('config.yml', SAMPLE_CONFIG_3)
      described_class.send(:load_settings)
      expect(described_class.sources.length).to eq(2)
      expect(described_class.sources[0]['name']).to eq('src')
      expect(described_class.sources[0]['path']).to(
        eq(File.expand_path('src'))
      )
      expect(described_class.sources[0]['target']).to eq('src')
      expect(described_class.sources[0]['is_blog']).to be(false)
      expect(described_class.sources[0]['recursive']).to be(true)
      expect(described_class.sources[1]['name']).to eq('news')
      expect(described_class.sources[1]['path']).to(
        eq(File.expand_path('src/news'))
      )
      expect(described_class.sources[1]['target']).to eq('news')
      expect(described_class.sources[1]['is_blog']).to be(true)
      expect(described_class.sources[1]['recursive']).to be(true)
    end

    it 'generates projects hash' do
      IO.write('config.yml', SAMPLE_CONFIG_3)
      described_class.send(:load_settings)
      projects = described_class.send(:org_generate_projects)
      expect(projects).to have_key('news')
    end

    it 'exposes correct head header' do
      IO.write('config.yml', SAMPLE_CONFIG_3)
      described_class.send(:load_settings)
      projects = described_class.sources
      headers = described_class.send(:build_project_org_headers, projects[0])
      head = <<~HEAD.strip
        :html-head ""
         :html-postamble "<p><span class=\\\"author\\\">Written by %a</span>
        with %c, and published with %N</p>
        <p class=\\\"date\\\">Last modification on %C</p>
        <p class=\\\"validation\\\">%v</p>"
         :html-head-include-default-style t
         :html-head-include-scripts t
      HEAD
      expect(headers).to eq(head)
    end

    it 'exposes correct head header with custom domain' do
      IO.write('config.yml', SAMPLE_CONFIG_4)
      described_class.send(:load_settings)
      projects = described_class.sources
      headers = described_class.send(:build_project_org_headers, projects[0])
      head = <<~HEAD.strip
        :html-head ""
         :html-postamble "<p><span class=\\\"author\\\">Written by %a</span>
        with %c, and published with %N</p>
        <p class=\\\"date\\\">Last modification on %C</p>
        <p class=\\\"validation\\\">%v</p>"
         :html-head-include-default-style t
         :html-head-include-scripts t
      HEAD
      expect(headers).to eq(head)
      headers = described_class.send(:build_project_org_headers, projects[1])
      head = <<~HEAD.strip
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
         :html-head-include-default-style nil
         :html-head-include-scripts nil
      HEAD
      expect(headers).to eq(head)
    end

    it 'generates projects' do
      IO.write('config.yml', SAMPLE_CONFIG_4)
      described_class.send(:load_settings)
      projects = described_class.send(:org_generate_projects)
      srcconf = <<~SRCCONF
        ("src"
         :base-directory "#{Dir.pwd}/src"
         :base-extension "org"
         :recursive t
         :publishing-directory "#{Dir.pwd}/public_html/src"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :html-head ""
         :html-postamble "<p><span class=\\\"author\\\">Written by %a</span>
        with %c, and published with %N</p>
        <p class=\\\"date\\\">Last modification on %C</p>
        <p class=\\\"validation\\\">%v</p>"
         :html-head-include-default-style t
         :html-head-include-scripts t)
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
         :html-head-include-default-style nil
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

    it 'generates projects names list' do
      IO.write('config.yml', SAMPLE_CONFIG_4)
      described_class.send(:load_settings)
      projects = described_class.send(:org_generate_projects)
      expect(described_class.send(:project_names, projects)).to(
        eq('"src" "src-assets" "news" "news-assets" "other" "other-assets" "theme-my-theme"')
      )
    end
  end

  context 'without a config file' do
    before do
      FileUtils.mkdir_p 'tmp/config2'
      Dir.chdir 'tmp/config2'
      described_class.send(:load_settings)
    end

    after do
      described_class.load_test({})
      Dir.chdir File.expand_path('../..', __dir__)
      FileUtils.rm_r 'tmp/config2'
    end

    it 'uses default config' do
      conf = described_class.settings
      author = ENV['USER'] || ''
      expect(conf['author']).to eq(author)
      expect(conf['lang']).to eq('en') # Defined in ENV in requirements.rb
      expect(conf['public_folder']).to eq('public_html')
      expect(conf['domain']).to eq('')
    end

    it 'handles weird case' do
      expect(described_class.send(:build_source, 'name' => 'wrong')).to be(nil)
      expect(described_class.send(:build_source, :test)).to be(nil)
    end
  end

  context 'with config utils method' do
    after do
      FileUtils.rm 'tmp/__last_org_version__', force: true
      described_class.class_eval '@org_version = nil', __FILE__, __LINE__
    end

    it 'gives current org version' do
      described_class.class_eval '@org_version = nil', __FILE__, __LINE__
      IO.write('tmp/__last_org_version__', 'test')
      expect(described_class.org_last_version).to eq('test')
    end
  end
end
