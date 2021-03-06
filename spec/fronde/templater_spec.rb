# frozen_string_literal: true

require 'fronde/templater'

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

  context 'with a testing website' do
    let(:metatag) { '<meta property="test" content="TEST">' }
    let(:metatag_digest) { Digest::MD5.hexdigest(metatag) }
    let(:result) do
      <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
        <!-- Fronde Template: #{metatag_digest} -->

            <meta property="test" content="TEST">
        <title>My website</title>
          </head>
          <body>
            <h1>My website</h1>
          </body>
        </html>
      RESULT
    end

    before do
      init_testing_website
    end

    after do
      Dir.chdir File.expand_path('../..', __dir__)
      FileUtils.rm_r 'tmp/website_testing', force: true
    end

    context 'with a simple customization process' do
      before do
        FileUtils.mkdir 'public_html'
        IO.write('public_html/customize_test.html', html_base)
      end

      after do
        FileUtils.rm_r ['public_html', 'src'], force: true
        Fronde::Config.load_test({})
      end

      it 'customizes a given html file with simple template' do
        Fronde::Config.load_test(
          'templates' => [
            { 'selector' => 'title',
              'content' => metatag }
          ]
        )
        described_class.customize_output('public_html/customize_test.html')
        local_result = <<~RESULT
          <!DOCTYPE html>
          <html>
            <head>
          <!-- Fronde Template: #{metatag_digest} -->

              <title>My website</title>
          <meta property="test" content="TEST">
            </head>
            <body>
              <h1>My website</h1>
            </body>
          </html>
        RESULT
        expect(IO.read('public_html/customize_test.html')).to eq(local_result)
      end

      it 'customizes a given html file with a given org object' do
        Fronde::Config.load_test(
          'templates' => [
            { 'selector' => 'title',
              'content' => metatag }
          ]
        )
        org_content = <<~ORG
          #+title: Index file

          My website
        ORG
        FileUtils.mkdir 'src'
        IO.write('src/index.org', org_content)
        o = Fronde::OrgFile.new('src/index.org')
        described_class.customize_output('public_html/customize_test.html', o)
        local_result = <<~RESULT
          <!DOCTYPE html>
          <html>
            <head>
          <!-- Fronde Template: #{metatag_digest} -->

              <title>My website</title>
          <meta property="test" content="TEST">
            </head>
            <body>
              <h1>My website</h1>
            </body>
          </html>
        RESULT
        expect(IO.read('public_html/customize_test.html')).to eq(local_result)
      end

      it 'customizes a given html file with before' do
        Fronde::Config.load_test(
          'templates' => [
            { 'selector' => 'title',
              'type' => 'before',
              'content' => metatag }
          ]
        )
        described_class.customize_output('public_html/customize_test.html')
        expect(IO.read('public_html/customize_test.html')).to eq(result)
      end

      it 'customizes a given html file with after' do
        Fronde::Config.load_test(
          'templates' => [
            { 'selector' => 'title',
              'type' => 'after',
              'content' => metatag }
          ]
        )
        described_class.customize_output('public_html/customize_test.html')
        local_result = <<~RESULT
          <!DOCTYPE html>
          <html>
            <head>
          <!-- Fronde Template: #{metatag_digest} -->

              <title>My website</title>
          <meta property="test" content="TEST">
            </head>
            <body>
              <h1>My website</h1>
            </body>
          </html>
        RESULT
        expect(IO.read('public_html/customize_test.html')).to eq(local_result)
      end

      it 'customizes a given html file with replace content' do
        Fronde::Config.load_test(
          'templates' => [
            { 'selector' => 'body>h1',
              'type' => 'replace',
              'content' => '<p>Toto tata</p>' }
          ]
        )
        described_class.customize_output('public_html/customize_test.html')
        local_result = <<~RESULT
          <!DOCTYPE html>
          <html>
            <head>
          <!-- Fronde Template: #{Digest::MD5.hexdigest('<p>Toto tata</p>')} -->

              <title>My website</title>
            </head>
            <body>
              <p>Toto tata</p>
            </body>
          </html>
        RESULT
        expect(IO.read('public_html/customize_test.html')).to eq(local_result)
      end

      it 'customizes a given html file with previous comments in head' do
        Fronde::Config.load_test(
          'templates' => [
            { 'selector' => 'body>h1',
              'type' => 'replace',
              'content' => '<p>Toto tata</p>' }
          ]
        )
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
        IO.write('public_html/customize_test2.html', local_html_base)
        described_class.customize_output('public_html/customize_test2.html')
        local_result = <<~RESULT
          <!DOCTYPE html>
          <html>
            <head>
          <!-- Fronde Template: #{Digest::MD5.hexdigest('<p>Toto tata</p>')} -->

              <!-- This is a test comment -->
              <title>My website</title>
            </head>
            <body>
              <p>Toto tata</p>
            </body>
          </html>
        RESULT
        expect(IO.read('public_html/customize_test2.html')).to eq(local_result)
      end

      it 'does not customize a given html file with wrong templates' do
        result = IO.read('public_html/customize_test.html')
        Fronde::Config.load_test(
          'templates' => [
            { 'type' => 'replace',
              'content' => '<p>Toto tata</p>' },
            { 'selector' => 'body>h1',
              'type' => 'replace' }
          ]
        )
        described_class.customize_output('public_html/customize_test.html')
        expect(IO.read('public_html/customize_test.html')).to eq(result)
      end

      it 'does not customize a given html file with no templates' do
        result = IO.read('public_html/customize_test.html')
        Fronde::Config.load_test({})
        described_class.customize_output('public_html/customize_test.html')
        expect(IO.read('public_html/customize_test.html')).to eq(result)
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
        IO.write('public_html/customize_test.html', other_html_base)
        Fronde::Config.load_test(
          'templates' => [
            { 'type' => 'before',
              'selector' => 'div#content',
              'source' => 'div#content>nav' }
          ]
        )
        described_class.customize_output('public_html/customize_test.html')
        result = <<~HTML
          <!DOCTYPE html>
          <html>
            <head>
          <!-- Fronde Template: #{Digest::MD5.hexdigest('<nav>My menu</nav>')} -->

              <title>My website</title>
            </head>
            <body>
              <nav>My menu</nav><div id="content">
                <h1>My website</h1>
                <p>Lorem ipsum...</p>
              </div>
            </body>
          </html>
        HTML
        expect(IO.read('public_html/customize_test.html')).to eq(result)
      end
    end

    context 'with multiple customize call' do
      before do
        FileUtils.mkdir_p 'public_html/customize'
        IO.write('public_html/customize_test.html', html_base)
        IO.write('public_html/customize/test.html', html_base)
        Fronde::Config.load_test(
          'templates' => [
            { 'selector' => 'title',
              'path' => '/customize/*',
              'type' => 'before',
              'content' => metatag }
          ]
        )
      end

      after do
        FileUtils.rm_r 'public_html', force: true
        Fronde::Config.load_test({})
      end

      it 'customizes a file on a specific path' do
        described_class.customize_output('public_html/customize_test.html')
        expect(IO.read('public_html/customize_test.html')).to eq(html_base)
        described_class.customize_output('public_html/customize/test.html')
        expect(IO.read('public_html/customize/test.html')).to eq(result)
      end

      it 'does not customize twice a file' do
        described_class.customize_output('public_html/customize/test.html')
        expect(IO.read('public_html/customize/test.html')).to eq(result)
        described_class.customize_output('public_html/customize/test.html')
        expect(IO.read('public_html/customize/test.html')).to eq(result)
      end
    end

    context 'with multiple path to customize' do
      before do
        FileUtils.mkdir_p ['public_html/customize', 'public_html/other']
        IO.write('public_html/customize_test.html', html_base)
        IO.write('public_html/customize/test.html', html_base)
        IO.write('public_html/other/file.html', html_base)
        Fronde::Config.load_test(
          'templates' => [
            { 'selector' => 'title',
              'path' => ['/customize/*', '/other/*'],
              'type' => 'before',
              'content' => metatag },
            { 'type' => 'replace',
              'content' => 'Youpeee' }
          ]
        )
      end

      after do
        FileUtils.rm_r 'public_html', force: true
        Fronde::Config.load_test({})
      end

      it 'customizes a file on a specific path' do
        described_class.customize_output('public_html/customize_test.html')
        expect(IO.read('public_html/customize_test.html')).to eq(html_base)
        described_class.customize_output('public_html/customize/test.html')
        expect(IO.read('public_html/customize/test.html')).to eq(result)
        described_class.customize_output('public_html/other/file.html')
        expect(IO.read('public_html/other/file.html')).to eq(result)
      end
    end
  end
end
