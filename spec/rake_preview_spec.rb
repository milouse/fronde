# frozen_string_literal: true

require 'rake'
require 'open-uri'
require 'net/http'

def fetch_test_content(path)
  page_content = URI("http://localhost:5000/#{path}").open.read
  # Ensure tests always pass, whatever charset is generated
  page_content.gsub!('charset=UTF-8', 'charset=utf-8')
  page_content.gsub(
    /[FMSTW][a-z]+, [ADFJMNOS][a-z]+ \d{1,2}, \d{4} at \d{2}:\d{2}/,
    '__PUB_DATE__'
  )
end

def init_preview
  config = <<~CONF
    ---
    author: Tata
    title: This is a website about test
    org-html:
      html-postamble: '<footer>Published by Fronde.</footer>'
    templates:
    - type: before
      selector: title
      content: |
        <meta property="fronde-test" content="test"/>
    preview:
      routes:
        /test: public_html/index.html
  CONF
  File.write 'config.yml', config
  # It is required to reload config now to have the right default author
  # name
  Fronde::CONFIG.reset
  Fronde::CONFIG.write_org_lisp_config
  FileUtils.mkdir_p 'src' # Might already exists in first example
  FileUtils.cp(
    File.expand_path('../tigre.png', __dir__),
    'src/tigre.png'
  )
  Fronde::Org::File.new(
    'src/index.org',
    title: 'My website',
    content: <<~CONTENT
      Nice content.

      [[http://mydomain.local/tigre.png][Tigre]]
    CONTENT
  ).write
end

context 'when trying preview mode' do
  before(:all) do # rubocop:disable RSpec/BeforeAfterAll
    @webrick_app = nil # init variable
    init_testing_environment
    Fronde::CONFIG.reset # Take into account dir change
    init_fake_org_install
    copy_org_tarball_to_fake_tmp
    copy_org_lisp_files_to_fake_tmp
    rake.invoke_task('org:install')
  end

  after do
    # rubocop:disable RSpec/InstanceVariable
    @webrick_app.exit # Be sure to kill test server
    @webrick_app.join # Be patient before quitting example
    # rubocop:enable RSpec/InstanceVariable
    clean_testing_environment
  end

  after(:all) do # rubocop:disable RSpec/BeforeAfterAll
    tear_down 'tmp/website_testing'
  end

  context 'without a domain name' do
    before do
      init_preview
      rake.invoke_task('site:build')

      @webrick_app = Thread.new do
        rake.invoke_task('site:preview')
      end
      sleep 1 # Necessary to let webrick start
    end

    it 'is viewable with preview', :aggregate_failures do
      home_page = fetch_test_content 'index.html'
      proof = File.expand_path('data/index_proof.html', __dir__)
      expect(home_page).to eq(File.read(proof))
      tigre = URI('http://localhost:5000/tigre.png')
      res = Net::HTTP.get_response(tigre)
      expect(res).to be_a(Net::HTTPOK)
      expect(res['content-type']).to eq('image/png')
      expect(res['content-length']).to eq(File.size('src/tigre.png').to_s)
    end

    it 'serves index' do
      home_page = fetch_test_content ''
      proof = File.expand_path('data/index_proof.html', __dir__)
      expect(home_page).to eq(File.read(proof))
    end

    it 'is viewable with routes testing' do
      test_page = fetch_test_content 'test'
      proof = File.expand_path('data/index_proof.html', __dir__)
      expect(test_page).to eq(File.read(proof))
    end

    it 'sends an error if a page is not found' do
      expect { URI('http://localhost:5000/not_found.html').open.read }.to(
        raise_error(/404 Not Found/)
      )
    end
  end

  context 'with a domain name' do
    before do
      init_preview
      Fronde::CONFIG.reset
      old_conf = Fronde::CONFIG.settings.merge
      old_conf['domain'] = 'http://mydomain.local'
      Fronde::CONFIG.load_test(old_conf)
      Fronde::CONFIG.write_org_lisp_config
      rake.invoke_task('site:build')
      @webrick_app = Thread.new do
        rake.invoke_task('site:preview')
      end
      sleep 1
    end

    it 'replaces domain occurence by localhost URIs' do
      home_page = fetch_test_content 'index.html'
      proof = File.expand_path('data/index_proof.html', __dir__)
      proof_content = File.read(proof).gsub('mydomain.local', 'localhost:5000')
      expect(home_page).to eq(proof_content)
    end
  end
end
