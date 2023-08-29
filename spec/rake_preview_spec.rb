# frozen_string_literal: true

require 'rake'
require 'open-uri'
require 'net/http'

def init_preview
  copy_org_tarball_to_fake_tmp
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
  File.write('config.yml', config)
  Fronde::CONFIG.reset
  rake.invoke_task('org:install')
  FileUtils.cp(
    File.expand_path('../tigre.png', __dir__),
    'src/tigre.png'
  )
  Fronde::OrgFile.new(
    'src/index.org',
    title: 'My website',
    content: <<~CONTENT
      Nice content.

      [[http://mydomain.local/tigre.png][Tigre]]
    CONTENT
  ).write
end

context 'when trying preview mode' do
  let(:now_str) { DateTime.now.strftime('%A %-d of %B, %Y at %R') }

  after { tear_down 'tmp/website_testing' }

  context 'without a domain name' do
    around do |test|
      init_testing_environment
      init_preview

      rake.invoke_task('site:build')

      webrick_app = Thread.new do
        rake.invoke_task('site:preview')
      end
      sleep 1          # Necessary to let webrick start
      test.run         # Actually run test case
      webrick_app.exit # Be sure to kill test server
      webrick_app.join # Be patient before quitting example
    end

    it 'is viewable with preview', rake: true do
      home_page = URI('http://localhost:5000/index.html').open.read
      proof = File.expand_path('data/index_proof.html', __dir__)
      proof_content = File.read(proof).gsub('__PUB_DATE__', now_str)
      expect(home_page).to eq(proof_content)
      tigre = URI('http://localhost:5000/tigre.png')
      res = Net::HTTP.get_response(tigre)
      expect(res).to be_a(Net::HTTPOK)
      expect(res['content-type']).to eq('image/png')
      expect(res['content-length']).to eq(File.size('src/tigre.png').to_s)
    end

    it 'serves index', rake: true do
      home_page = URI('http://localhost:5000/').open.read
      proof = File.expand_path('data/index_proof.html', __dir__)
      proof_content = File.read(proof).gsub('__PUB_DATE__', now_str)
      expect(home_page).to eq(proof_content)
    end

    it 'is viewable with routes testing', rake: true do
      home_page = URI('http://localhost:5000/test').open.read
      proof = File.expand_path('data/index_proof.html', __dir__)
      proof_content = File.read(proof).gsub('__PUB_DATE__', now_str)
      expect(home_page).to eq(proof_content)
    end

    it 'sends an error if a page is not found', rake: true do
      expect { URI('http://localhost:5000/not_found.html').open.read }.to(
        raise_error(/404 Not Found/)
      )
    end
  end

  context 'with a domain name' do
    around do |test|
      init_testing_environment
      init_preview
      old_conf = Fronde::CONFIG.settings.merge
      old_conf['domain'] = 'http://mydomain.local'
      Fronde::CONFIG.load_test(old_conf)
      rake.invoke_task('site:build')
      webrick_app = Thread.new do
        rake.invoke_task('site:preview')
      end
      sleep 1
      test.run # Actually run test
      webrick_app.exit
      webrick_app.join
    end

    it 'replaces domain occurence by localhost URIs', rake: true do
      home_page = URI('http://localhost:5000/index.html').open.read
      proof = File.expand_path('data/index_proof.html', __dir__)
      proof_content = File.read(proof).gsub('__PUB_DATE__', now_str)
      proof_content.gsub!('mydomain.local', 'localhost:5000')
      expect(home_page).to eq(proof_content)
    end
  end
end
