# frozen_string_literal: true

require_relative '../../lib/fronde/templater'

def template_digest(template_config)
  Digest::MD5.hexdigest(template_config.to_s)
end

def template_proof(template_config)
  <<~RESULT
    <!DOCTYPE html>
    <html>
      <head>
        <meta property="test" content="TEST">
    <title>My website</title>
      </head>
      <body>
        <h1>My website</h1>
      </body>
    <!-- Fronde Template: #{template_digest(template_config)} -->
    </html>
  RESULT
end

describe Fronde::Templater do
  let(:html_base) do
    <<~HTML
      <!DOCTYPE html>
      <html>
        <head>
          <title>My website</title>
        </head>
        <body>
          <h1>My website</h1>
        </body>
      </html>
    HTML
  end

  let(:metatag) { '<meta property="test" content="TEST">' }

  before { init_testing_environment }

  after { tear_down 'tmp/website_testing' }

  context 'with a simple customization process' do
    before do
      FileUtils.mkdir %w[src public_html]
      FileUtils.touch 'src/customize_test.org'
      File.write 'public_html/customize_test.html', html_base
    end

    after do
      FileUtils.rm_r %w[public_html src]
      Fronde::CONFIG.reset
    end

    it 'skips file on unknown source' do
      FileUtils.mkdir 'public_html/customize'
      test_path = './public_html/customize/test.html'
      File.write test_path, html_base
      expect do
        described_class.customize_output('public_html/customize/test.html')
      end.to(
        output(
          "No project found for #{test_path}. Publication will fail.\n"
        ).to_stderr
      )
    end

    it 'warns when target node cannot be found' do
      Fronde::CONFIG.load_test(
        'templates' => [{ 'selector' => 'title', 'source' => 'h2' }]
      )
      expect do
        described_class.customize_output('public_html/customize_test.html')
      end.to(
        output(
          'No element found with the selector h2 in ' \
          "./public_html/customize_test.html.\n"
        ).to_stderr
      )
    end

    it 'customizes a given html file with simple template' do
      rule = { 'selector' => 'title', 'content' => metatag }
      Fronde::CONFIG.load_test('templates' => [rule])
      described_class.customize_output('public_html/customize_test.html')
      local_result = <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
            <title>My website</title>
        <meta property="test" content="TEST">
          </head>
          <body>
            <h1>My website</h1>
          </body>
        <!-- Fronde Template: #{template_digest(rule)} -->
        </html>
      RESULT
      expect(File.read('public_html/customize_test.html')).to eq(local_result)
    end

    it 'customizes a given html file with a given org object' do
      rule = { 'selector' => 'title', 'content' => metatag }
      Fronde::CONFIG.load_test('templates' => [rule])
      org_content = <<~ORG
        #+title: Index file

        My website
      ORG
      File.write 'src/customize_test.org', org_content
      described_class.customize_output('public_html/customize_test.html')
      local_result = <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
            <title>My website</title>
        <meta property="test" content="TEST">
          </head>
          <body>
            <h1>My website</h1>
          </body>
        <!-- Fronde Template: #{template_digest(rule)} -->
        </html>
      RESULT
      expect(File.read('public_html/customize_test.html')).to eq(local_result)
    end

    it 'customizes a given html file with before' do
      rule = { 'selector' => 'title', 'type' => 'before',
               'content' => metatag }
      Fronde::CONFIG.load_test('templates' => [rule])
      described_class.customize_output('public_html/customize_test.html')
      expect(File.read('public_html/customize_test.html')).to(
        eq(template_proof(rule))
      )
    end

    it 'customizes a given html file with after' do
      rule = { 'selector' => 'title', 'type' => 'after', 'content' => metatag }
      Fronde::CONFIG.load_test('templates' => [rule])
      described_class.customize_output('public_html/customize_test.html')
      local_result = <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
            <title>My website</title>
        <meta property="test" content="TEST">
          </head>
          <body>
            <h1>My website</h1>
          </body>
        <!-- Fronde Template: #{template_digest(rule)} -->
        </html>
      RESULT
      expect(File.read('public_html/customize_test.html')).to eq(local_result)
    end

    it 'customizes a given html file with replace content' do
      rule = { 'selector' => 'body>h1', 'type' => 'replace',
               'content' => '<p>Toto tata</p>' }
      Fronde::CONFIG.load_test('templates' => [rule])
      described_class.customize_output('public_html/customize_test.html')
      local_result = <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
            <title>My website</title>
          </head>
          <body>
            <p>Toto tata</p>
          </body>
        <!-- Fronde Template: #{template_digest(rule)} -->
        </html>
      RESULT
      expect(File.read('public_html/customize_test.html')).to eq(local_result)
    end

    it 'customizes a given html file with previous comments in head' do
      rule = { 'selector' => 'body>h1', 'type' => 'replace',
               'content' => '<p>Toto tata</p>' }
      Fronde::CONFIG.load_test('templates' => [rule])
      local_html_base = <<~HTML
        <!DOCTYPE html>
        <html>
          <head>
            <!-- This is a test comment -->
            <title>My website</title>
          </head>
          <body>
            <h1>My website</h1>
          </body>
        </html>
      HTML
      FileUtils.touch 'src/customize_test2.org'
      File.write 'public_html/customize_test2.html', local_html_base
      described_class.customize_output('public_html/customize_test2.html')
      local_result = <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
            <!-- This is a test comment -->
            <title>My website</title>
          </head>
          <body>
            <p>Toto tata</p>
          </body>
        <!-- Fronde Template: #{template_digest(rule)} -->
        </html>
      RESULT
      expect(File.read('public_html/customize_test2.html')).to eq(local_result)
    end

    it 'does not customize a given html file with wrong templates' do
      result = File.read('public_html/customize_test.html')
      Fronde::CONFIG.load_test(
        'templates' => [
          { 'type' => 'replace',
            'content' => '<p>Toto tata</p>' },
          { 'selector' => 'body>h1',
            'type' => 'replace' }
        ]
      )
      described_class.customize_output('public_html/customize_test.html')
      expect(File.read('public_html/customize_test.html')).to eq(result)
    end

    it 'does not customize a given html file with no templates' do
      result = File.read 'public_html/customize_test.html'
      Fronde::CONFIG.load_test({})
      described_class.customize_output('public_html/customize_test.html')
      expect(File.read('public_html/customize_test.html')).to eq(result)
    end

    it 'moves elements around' do
      other_html_base = <<~HTML
        <!DOCTYPE html>
        <html>
          <head>
            <title>My website</title>
          </head>
          <body>
            <div id="content">
              <h1>My website</h1>
              <nav>My menu</nav><p>Lorem ipsum...</p>
            </div>
          </body>
        </html>
      HTML
      File.write 'public_html/customize_test.html', other_html_base
      rule = { 'type' => 'before', 'selector' => 'div#content',
               'source' => 'div#content>nav' }
      Fronde::CONFIG.load_test('templates' => [rule])
      described_class.customize_output('public_html/customize_test.html')
      result = <<~HTML
        <!DOCTYPE html>
        <html>
          <head>
            <title>My website</title>
          </head>
          <body>
            <nav>My menu</nav><div id="content">
              <h1>My website</h1>
              <p>Lorem ipsum...</p>
            </div>
          </body>
        <!-- Fronde Template: #{template_digest(rule)} -->
        </html>
      HTML
      expect(File.read('public_html/customize_test.html')).to eq(result)
    end
  end

  context 'with multiple customize call' do
    let(:template_rule) do
      { 'selector' => 'title',
        'path' => '/customize/*',
        'type' => 'before',
        'content' => metatag }
    end

    before do
      FileUtils.mkdir_p ['src/customize', 'public_html/customize']
      FileUtils.touch 'src/customize_test.org'
      File.write 'public_html/customize_test.html', html_base
      FileUtils.touch 'src/customize/test.org'
      File.write 'public_html/customize/test.html', html_base
      Fronde::CONFIG.load_test('templates' => [template_rule])
    end

    after { FileUtils.rm_r %w[public_html src] }

    it 'customizes a file on a specific path', :aggregate_failures do
      described_class.customize_output('public_html/customize_test.html')
      expect(File.read('public_html/customize_test.html')).to eq(html_base)
      # Run it a second time
      described_class.customize_output('public_html/customize/test.html')
      expect(File.read('public_html/customize/test.html')).to(
        eq(template_proof(template_rule))
      )
    end

    it 'does not customize twice a file', :aggregate_failures do
      described_class.customize_output('public_html/customize/test.html')
      expect(File.read('public_html/customize/test.html')).to(
        eq(template_proof(template_rule))
      )
      # Run it a second time
      described_class.customize_output('public_html/customize/test.html')
      expect(File.read('public_html/customize/test.html')).to(
        eq(template_proof(template_rule))
      )
    end

    it 'does not remove moving elements', :aggregate_failures do
      other_html_base = <<~HTML
        <!DOCTYPE html>
        <html>
          <head>
            <title>My website</title>
          </head>
          <body>
            <div id="content">
              <h1>My website</h1>
              <nav>My menu</nav><p>Lorem ipsum...</p>
            </div>
          </body>
        </html>
      HTML
      File.write 'public_html/customize_test.html', other_html_base
      rule = { 'type' => 'before', 'selector' => 'div#content',
               'source' => 'div#content>nav' }
      Fronde::CONFIG.load_test('templates' => [rule])
      described_class.customize_output('public_html/customize_test.html')
      result = <<~HTML
        <!DOCTYPE html>
        <html>
          <head>
            <title>My website</title>
          </head>
          <body>
            <nav>My menu</nav><div id="content">
              <h1>My website</h1>
              <p>Lorem ipsum...</p>
            </div>
          </body>
        <!-- Fronde Template: #{template_digest(rule)} -->
        </html>
      HTML
      expect(File.read('public_html/customize_test.html')).to eq(result)
      # Run it a second time
      described_class.customize_output('public_html/customize_test.html')
      expect(File.read('public_html/customize_test.html')).to eq(result)
    end
  end

  context 'with multiple path to customize' do
    let(:template_rule) do
      { 'selector' => 'title',
        'type' => 'before',
        'path' => ['/customize/*', '/other/*'],
        'content' => metatag }
    end

    before do
      FileUtils.mkdir_p(
        ['src/customize', 'src/other',
         'public_html/customize', 'public_html/other']
      )
      FileUtils.touch 'src/customize_test.org'
      File.write 'public_html/customize_test.html', html_base
      FileUtils.touch 'src/customize/test.org'
      File.write 'public_html/customize/test.html', html_base
      FileUtils.touch 'src/other/file.org'
      File.write 'public_html/other/file.html', html_base
      Fronde::CONFIG.load_test('templates' => [template_rule])
    end

    after { FileUtils.rm_r %w[public_html src] }

    it 'customizes a file on a specific path', :aggregate_failures do
      described_class.customize_output('public_html/customize_test.html')
      expect(File.read('public_html/customize_test.html')).to eq(html_base)
      described_class.customize_output('public_html/customize/test.html')
      expect(File.read('public_html/customize/test.html')).to(
        eq(template_proof(template_rule))
      )
      described_class.customize_output('public_html/other/file.html')
      expect(File.read('public_html/other/file.html')).to(
        eq(template_proof(template_rule))
      )
    end
  end
end
