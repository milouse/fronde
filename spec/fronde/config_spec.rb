# frozen_string_literal: true

SAMPLE_CONFIG = <<~CONF
  ---
  author: Tata
CONF

SAMPLE_CONFIG_2 = <<~CONF
  ---
  author: Titi
  domain: https://tutu.com
CONF

SAMPLE_CONFIG_3 = <<~CONF
  ---
  author: Tata
  sources:
  - path: src
    recursive: no
  - path: src/news
    is_blog: true
  - wrong: source
CONF

SAMPLE_CONFIG_4 = <<~CONF
  ---
  author: Tata
  domain: https://test.com
  sources:
  - src
  - path: news
    is_blog: true
    theme: my-theme
  - path: other
    theme: my-theme
CONF

SAMPLE_CONFIG_5 = <<~CONF
  ---
  author: Tata
  domain: https://test.com
  theme: my-theme
  sources:
  - src
  - path: news
    theme: default
CONF

describe Fronde::CONFIG do
  context 'without a config file' do
    before do
      FileUtils.mkdir_p 'tmp/no_config'
      Dir.chdir 'tmp/no_config'
      described_class.reset
    end

    after do
      tear_down 'tmp/no_config'
    end

    it 'uses default config' do
      conf = described_class.settings
      {
        'author' => ENV['USER'] || '',
        'lang' => 'en', # Defined in ENV in requirements.rb
        'html_public_folder' => File.expand_path('public_html'),
        'domain' => ''
      }.each { |key, value| expect(conf[key]).to eq value }
    end

    it 'warns about duplicate sources' do
      config = {
        'sources' => [
          { 'path' => 'src', 'type' => 'html' },
          { 'path' => 'src', 'type' => 'html' }
        ]
      }
      expect { described_class.load_test(config) }.to(
        output(/Skipping src as it appears at least twice/).to_stderr
      )
    end

    it 'warns about embedded sources' do
      config = {
        'sources' => [
          { 'path' => 'src/test', 'type' => 'html' },
          { 'path' => 'src', 'type' => 'html' },
          { 'path' => 'src/other_test', 'type' => 'html' } # for coverage
        ]
      }
      label = 'Skipping src/test as it might be already ' \
              'embedded into the other source'
      expect { described_class.load_test(config) }.to \
        output(/#{label}/).to_stderr
    end

    it 'does not warn about embedded sources when no recursive' do
      config = {
        'sources' => [
          { 'path' => 'src', 'type' => 'html', 'recursive' => false },
          { 'path' => 'src/test', 'type' => 'html' }
        ]
      }
      expect { described_class.load_test(config) }.not_to output.to_stderr
    end
  end

  context 'with a config file' do
    before do
      FileUtils.mkdir_p 'tmp/config'
      Dir.chdir 'tmp/config'
      File.write('config.yml', SAMPLE_CONFIG)
      described_class.reset
    end

    after do
      tear_down 'tmp/config'
    end

    it 'parses it successfully' do
      conf = described_class.settings
      {
        'author' => 'Tata',
        'lang' => 'en',
        'theme' => 'default',
        'domain' => ''
      }.each { |key, value| expect(conf[key]).to eq value }
    end

    it 'runs migrations', :aggregate_failures do
      config_with_migrations_to_do = <<~CONF
        ---
        public_folder: public
      CONF
      File.write('config.yml', config_with_migrations_to_do)
      expect { described_class.reset }.to(
        output(/‘public_folder’ setting is deprecated./).to_stderr
      )
      conf = described_class.settings
      expect(conf['public_folder']).to be_nil
      expect(conf['html_public_folder']).to eq File.expand_path('public')
    end

    it 'cleans config file if needed', :aggregate_failures do
      config_with_migrations_to_do = <<~CONF
        ---
        public_folder: old_public
        html_public_folder: new_public
      CONF
      File.write('config.yml', config_with_migrations_to_do)
      expect { described_class.reset }.to(
        output(/‘public_folder’ setting is deprecated./).to_stderr
      )
      conf = described_class.settings
      expect(conf['public_folder']).to be_nil
      expect(conf['html_public_folder']).to eq File.expand_path('new_public')
    end
  end

  context 'with a blog config file (config 3)' do
    before do
      FileUtils.mkdir_p 'tmp/config'
      Dir.chdir 'tmp/config'
      File.write('config.yml', SAMPLE_CONFIG_3)
      described_class.reset
    end

    after do
      tear_down 'tmp/config'
    end

    it 'lists sources', :aggregate_failures do
      # for coverage testing, should not load sources twice and actually
      # returns the already loaded sources
      projects = described_class.load_sources
      expect(projects.length).to eq(2)
      expect(projects[0]['name']).to eq('src')
      expect(projects[0]['path']).to(
        eq(File.expand_path('src'))
      )
      expect(projects[0]['target']).to eq('src')
      expect(projects[0]['is_blog']).to be(false)
      expect(projects[0].blog?).to be(false)
      expect(projects[0]['recursive']).to be(false)

      expect(projects[1]['name']).to eq('src-news')
      expect(projects[1]['path']).to(
        eq(File.expand_path('src/news'))
      )
      expect(projects[1]['target']).to eq('news')
      expect(projects[1]['is_blog']).to be(true)
      expect(projects[1].blog?).to be(true)
      expect(projects[1]['recursive']).to be(true)
    end

    it 'generates lisp-config.el', :aggregate_failures do
      described_class.write_org_lisp_config
      lisp_config = File.expand_path('var/lib/org-config.el')
      expect(File.file?(lisp_config)).to be(true)
      proof = proof_content('config_3_org_config.el')
      expect(File.read(lisp_config)).to eq(proof)
    end

    it 'generates lisp-config.el for gemini projects', :aggregate_failures do
      old_conf = described_class.settings.merge
      old_conf['sources'][0] = {
        'path' => 'src',
        'recursive' => false,
        'type' => 'gemini'
      }
      described_class.load_test(old_conf)
      projects = described_class.sources
      expect(projects[0]['type']).to eq('gemini')
      described_class.write_org_lisp_config
      proof = proof_content('config_3_gemini_org_config.el')
      lisp_config = File.expand_path('var/lib/org-config.el')
      expect(File.read(lisp_config)).to eq(proof)
    end
  end

  context 'with a blog and a custom domain (config 4)' do
    before do
      FileUtils.mkdir_p 'tmp/config/themes/my-theme'
      Dir.chdir 'tmp/config'
      File.write('config.yml', SAMPLE_CONFIG_4)
      described_class.reset
    end

    after do
      tear_down 'tmp/config'
    end

    it 'generates lisp-config.el', :aggregate_failures do
      described_class.write_org_lisp_config
      lisp_config = File.expand_path('var/lib/org-config.el')
      expect(File.file?(lisp_config)).to be(true)
      proof = proof_content('config_4_org_config.el')
      expect(File.read(lisp_config)).to eq(proof)
    end

    it 'generates lisp-config.el for gemini projects', :aggregate_failures do
      old_conf = described_class.settings.merge
      old_conf['sources'][0] = { 'path' => 'src', 'type' => 'gemini' }
      described_class.load_test(old_conf)
      projects = described_class.sources
      expect(projects[0]['type']).to eq('gemini')
      described_class.write_org_lisp_config
      proof = proof_content('config_4_gemini_org_config.el')
      lisp_config = File.expand_path('var/lib/org-config.el')
      expect(File.read(lisp_config)).to eq(proof)
    end
  end

  context 'without a blog and with default theme (config 5)' do
    before do
      FileUtils.mkdir_p 'tmp/config/themes/my-theme'
      Dir.chdir 'tmp/config'
      File.write('config.yml', SAMPLE_CONFIG_5)
      described_class.reset
    end

    after do
      tear_down 'tmp/config'
    end

    it 'generates lisp-config.el', :aggregate_failures do
      described_class.write_org_lisp_config
      lisp_config = File.expand_path('var/lib/org-config.el')
      expect(File.file?(lisp_config)).to be(true)
      proof = proof_content('config_5_org_config.el')
      expect(File.read(lisp_config)).to eq(proof)
    end
  end

  context 'with various theme configuration' do
    before do
      FileUtils.mkdir_p 'tmp/config/themes/my-theme'
      Dir.chdir 'tmp/config'
    end

    after do
      tear_down 'tmp/config'
    end

    it 'fails with unknown theme' do
      File.write 'config.yml', ['---', 'theme: unknown'].join("\n")
      described_class.reset
      expect { described_class.write_org_lisp_config }.to(
        raise_error(
          Errno::ENOENT,
          'No such file or directory - Theme unknown not found'
        )
      )
    end

    it 'generates lisp-config.el with packaged theme', :aggregate_failures do
      File.write 'config.yml', ['---', 'theme: umaneti'].join("\n")
      described_class.reset
      described_class.write_org_lisp_config
      content = File.read File.expand_path('var/lib/org-config.el')
      theme_dir = File.expand_path(
        '../../lib/fronde/config/data/themes', __dir__
      )
      path_rx = %r{
        "theme-umaneti"\n\s{9}
        :base-directory\s"(?<path>[^"]+)/umaneti"$
      }xm
      match = path_rx.match(content)
      expect(match).not_to be_nil
      expect(match[:path]).to eq theme_dir
    end

    it 'generates lisp-config.el with local theme', :aggregate_failures do
      File.write 'config.yml', ['---', 'theme: my-theme'].join("\n")
      described_class.reset
      described_class.write_org_lisp_config
      content = File.read File.expand_path('var/lib/org-config.el')
      theme_dir = File.expand_path 'themes'
      path_rx = %r{
        "theme-my-theme"\n\s{9}
        :base-directory\s"(?<path>[^"]+)/my-theme"$
      }xm
      match = path_rx.match(content)
      expect(match).not_to be_nil
      expect(match[:path]).to eq theme_dir
    end
  end
end
