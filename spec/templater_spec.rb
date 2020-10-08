# frozen_string_literal: true

require 'neruda/templater'

describe 'With a testing website' do
  before(:all) do
    init_testing_website
    @metatag = '<meta property="test" content="TEST">'
    @metatag_digest = Digest::MD5.hexdigest(@metatag)
  end

  after(:all) do
    Dir.chdir File.expand_path('..', __dir__)
    FileUtils.rm_r 'tmp/website_testing', force: true
  end

  describe 'Simple customize process' do
    before(:each) do
      FileUtils.mkdir 'public_html'
      html_base = <<~HTML
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
      IO.write('public_html/customize_test.html', html_base)
    end

    after(:each) do
      FileUtils.rm_r ['public_html', 'src'], force: true
      Neruda::Config.load_test({})
    end

    it 'should customize a given html file with simple template' do
      Neruda::Config.load_test(
        'templates' => [
          { 'selector' => 'title',
            'content' => @metatag }
        ]
      )
      Neruda::Templater.customize_output('public_html/customize_test.html')
      result = <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
        <!-- Neruda Template: #{@metatag_digest} -->

            <title>My website</title>
        <meta property="test" content="TEST">
          </head>
          <body>
            <h1>My website</h1>
          </body>
        </html>
      RESULT
      expect(IO.read('public_html/customize_test.html')).to eq(result)
    end

    it 'should customize a given html file with a given org object' do
      Neruda::Config.load_test(
        'templates' => [
          { 'selector' => 'title',
            'content' => @metatag }
        ]
      )
      org_content = <<~ORG
        #+title: Index file

        My website
      ORG
      FileUtils.mkdir 'src'
      IO.write('src/index.org', org_content)
      o = Neruda::OrgFile.new('src/index.org')
      Neruda::Templater.customize_output('public_html/customize_test.html', o)
      result = <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
        <!-- Neruda Template: #{@metatag_digest} -->

            <title>My website</title>
        <meta property="test" content="TEST">
          </head>
          <body>
            <h1>My website</h1>
          </body>
        </html>
      RESULT
      expect(IO.read('public_html/customize_test.html')).to eq(result)
    end

    it 'should customize a given html file with before' do
      Neruda::Config.load_test(
        'templates' => [
          { 'selector' => 'title',
            'type' => 'before',
            'content' => @metatag }
        ]
      )
      Neruda::Templater.customize_output('public_html/customize_test.html')
      result = <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
        <!-- Neruda Template: #{@metatag_digest} -->

            <meta property="test" content="TEST">
        <title>My website</title>
          </head>
          <body>
            <h1>My website</h1>
          </body>
        </html>
      RESULT
      expect(IO.read('public_html/customize_test.html')).to eq(result)
    end

    it 'should customize a given html file with after' do
      Neruda::Config.load_test(
        'templates' => [
          { 'selector' => 'title',
            'type' => 'after',
            'content' => @metatag }
        ]
      )
      Neruda::Templater.customize_output('public_html/customize_test.html')
      result = <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
        <!-- Neruda Template: #{@metatag_digest} -->

            <title>My website</title>
        <meta property="test" content="TEST">
          </head>
          <body>
            <h1>My website</h1>
          </body>
        </html>
      RESULT
      expect(IO.read('public_html/customize_test.html')).to eq(result)
    end

    it 'should customize a given html file with replace content' do
      Neruda::Config.load_test(
        'templates' => [
          { 'selector' => 'body>h1',
            'type' => 'replace',
            'content' => '<p>Toto tata</p>' }
        ]
      )
      Neruda::Templater.customize_output('public_html/customize_test.html')
      result = <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
        <!-- Neruda Template: #{Digest::MD5.hexdigest('<p>Toto tata</p>')} -->

            <title>My website</title>
          </head>
          <body>
            <p>Toto tata</p>
          </body>
        </html>
      RESULT
      expect(IO.read('public_html/customize_test.html')).to eq(result)
    end

    it 'should customize a given html file with previous comments in head' do
      Neruda::Config.load_test(
        'templates' => [
          { 'selector' => 'body>h1',
            'type' => 'replace',
            'content' => '<p>Toto tata</p>' }
        ]
      )
      html_base = <<~HTML
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
      IO.write('public_html/customize_test2.html', html_base)
      Neruda::Templater.customize_output('public_html/customize_test2.html')
      result = <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
        <!-- Neruda Template: #{Digest::MD5.hexdigest('<p>Toto tata</p>')} -->

            <!-- This is a test comment -->
            <title>My website</title>
          </head>
          <body>
            <p>Toto tata</p>
          </body>
        </html>
      RESULT
      expect(IO.read('public_html/customize_test2.html')).to eq(result)
    end

    it 'should not customize a given html file with wrong templates' do
      result = IO.read('public_html/customize_test.html')
      Neruda::Config.load_test(
        'templates' => [
          { 'type' => 'replace',
            'content' => '<p>Toto tata</p>' },
          { 'selector' => 'body>h1',
            'type' => 'replace' }
        ]
      )
      Neruda::Templater.customize_output('public_html/customize_test.html')
      expect(IO.read('public_html/customize_test.html')).to eq(result)
    end

    it 'should not customize a given html file with no templates' do
      result = IO.read('public_html/customize_test.html')
      Neruda::Config.load_test({})
      Neruda::Templater.customize_output('public_html/customize_test.html')
      expect(IO.read('public_html/customize_test.html')).to eq(result)
    end
  end

  describe 'Multiple customize call' do
    before(:each) do
      FileUtils.mkdir_p 'public_html/customize'
      html_base = <<~HTML
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
      IO.write('public_html/customize_test.html', html_base)
      @html_base = <<~HTML
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
      IO.write('public_html/customize/test.html', @html_base)
      @result = <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
        <!-- Neruda Template: #{@metatag_digest} -->

            <meta property="test" content="TEST">
        <title>My website</title>
          </head>
          <body>
            <h1>My website</h1>
          </body>
        </html>
      RESULT
      Neruda::Config.load_test(
        'templates' => [
          { 'selector' => 'title',
            'path' => '/customize/*',
            'type' => 'before',
            'content' => @metatag }
        ]
      )
    end

    after(:each) do
      FileUtils.rm_r 'public_html', force: true
      Neruda::Config.load_test({})
    end

    it 'should customize a file on a specific path' do
      Neruda::Templater.customize_output('public_html/customize_test.html')
      expect(IO.read('public_html/customize_test.html')).to eq(@html_base)
      Neruda::Templater.customize_output('public_html/customize/test.html')
      expect(IO.read('public_html/customize/test.html')).to eq(@result)
    end

    it 'should not customize twice a file' do
      Neruda::Templater.customize_output('public_html/customize/test.html')
      expect(IO.read('public_html/customize/test.html')).to eq(@result)
      Neruda::Templater.customize_output('public_html/customize/test.html')
      expect(IO.read('public_html/customize/test.html')).to eq(@result)
    end
  end

  describe 'Multiple path to customize' do
    before(:each) do
      FileUtils.mkdir_p ['public_html/customize', 'public_html/other']
      html_base = <<~HTML
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
      IO.write('public_html/customize_test.html', html_base)
      @html_base = <<~HTML
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
      IO.write('public_html/customize/test.html', @html_base)
      IO.write('public_html/other/file.html', @html_base)
      @result = <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
        <!-- Neruda Template: #{@metatag_digest} -->

            <meta property="test" content="TEST">
        <title>My website</title>
          </head>
          <body>
            <h1>My website</h1>
          </body>
        </html>
      RESULT
      Neruda::Config.load_test(
        'templates' => [
          { 'selector' => 'title',
            'path' => ['/customize/*', '/other/*'],
            'type' => 'before',
            'content' => @metatag },
          { 'type' => 'replace',
            'content' => 'Youpeee' }
        ]
      )
    end

    after(:each) do
      FileUtils.rm_r 'public_html', force: true
      Neruda::Config.load_test({})
    end

    it 'should customize a file on a specific path' do
      Neruda::Templater.customize_output('public_html/customize_test.html')
      expect(IO.read('public_html/customize_test.html')).to eq(@html_base)
      Neruda::Templater.customize_output('public_html/customize/test.html')
      expect(IO.read('public_html/customize/test.html')).to eq(@result)
      Neruda::Templater.customize_output('public_html/other/file.html')
      expect(IO.read('public_html/other/file.html')).to eq(@result)
    end
  end
end
