# frozen_string_literal: true

require 'rake'
require 'digest/md5'

describe 'With working org files' do
  before(:all) do
    init_testing_website
    @rake = init_rake_and_install_org
  end

  before(:each) do
    @rake.options.build_all = true
    @rake.tasks.each(&:reenable)
    Rake.verbose(false)
  end

  after(:all) do
    Dir.chdir File.expand_path('../', __dir__)
    FileUtils.rm_r 'spec/data/website_testing', force: true
  end

  describe 'Build process' do
    before(:each) do
      o = Neruda::OrgFile.new('src/index.org', 'title' => 'My website')
      o.write
    end

    after(:each) do
      FileUtils.rm 'public_html/index.html', force: true
    end

    it 'should build something', rake: true do
      @rake.invoke_task('site:build')
      expect(File.exist?('public_html/index.html')).to be(true)
    end

    it 'should build something even in verbose mode', rake: true do
      Rake.verbose(true)
      @rake.invoke_task('site:build')
      expect(File.exist?('public_html/index.html')).to be(true)
    end

    it 'should build one specific file', rake: true do
      o = Neruda::OrgFile.new('src/tutu.org', 'title' => 'Tutu test')
      o.write
      @rake.invoke_task('site:build:one[src/tutu.org]')
      expect(File.exist?('public_html/index.html')).to be(false)
      expect(File.exist?('public_html/tutu.html')).to be(true)
    end

    it 'should return an error if no file is given in build:one', rake: true do
      expect { @rake.invoke_task('site:build:one') }.to \
        output("No source file given\n").to_stderr
    end
  end

  describe 'Customize process' do
    before(:each) do
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
      @metatag = '<meta property="test" content="TEST">'
    end

    after(:each) do
      FileUtils.rm 'public_html/customize_test.html'
    end

    it 'should return an error if no file is given', rake: true do
      expect { @rake.invoke_task('site:customize_output') }.to \
        output("No source file given\n").to_stderr
    end

    it 'should customize a given html file with simple template', rake: true do
      Neruda::Config.load_test(
        'templates' => [
          { 'selector' => 'title',
            'content' => @metatag }
        ]
      )
      @rake.invoke_task('site:customize_output[public_html/customize_test.html]')
      result = <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
        <!-- Neruda Template: #{Digest::MD5.hexdigest(@metatag)} -->

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

    it 'should customize a given html file with before', rake: true do
      Neruda::Config.load_test(
        'templates' => [
          { 'selector' => 'title',
            'type' => 'before',
            'content' => @metatag }
        ]
      )
      @rake.invoke_task('site:customize_output[public_html/customize_test.html]')
      result = <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
        <!-- Neruda Template: #{Digest::MD5.hexdigest(@metatag)} -->

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

    it 'should customize a given html file with after', rake: true do
      Neruda::Config.load_test(
        'templates' => [
          { 'selector' => 'title',
            'type' => 'after',
            'content' => @metatag }
        ]
      )
      Rake.verbose(true) # Test that in the same time
      @rake.invoke_task('site:customize_output[public_html/customize_test.html]')
      result = <<~RESULT
        <!DOCTYPE html>
        <html>
          <head>
        <!-- Neruda Template: #{Digest::MD5.hexdigest(@metatag)} -->

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

    it 'should customize a given html file with replace content', rake: true do
      Neruda::Config.load_test(
        'templates' => [
          { 'selector' => 'body>h1',
            'type' => 'replace',
            'content' => '<p>Toto tata</p>' }
        ]
      )
      @rake.invoke_task('site:customize_output[public_html/customize_test.html]')
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

    describe 'Multiple pass on customize' do
      before(:each) do
        FileUtils.mkdir_p 'public_html/customize'
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
        @metatag = '<meta property="test" content="TEST">'
        @result = <<~RESULT
          <!DOCTYPE html>
          <html>
            <head>
          <!-- Neruda Template: #{Digest::MD5.hexdigest(@metatag)} -->

              <meta property="test" content="TEST">
          <title>My website</title>
            </head>
            <body>
              <h1>My website</h1>
            </body>
          </html>
        RESULT
        IO.write('public_html/customize/test.html', @html_base)
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
        FileUtils.rm_r 'public_html/customize', force: true
      end

      it 'should customize a file on a specific path', rake: true do
        @rake.invoke_task('site:customize_output[public_html/customize_test.html]')
        expect(IO.read('public_html/customize_test.html')).to eq(@html_base)
        @rake.options.build_all = true
        @rake['site:customize_output'].reenable
        @rake.invoke_task('site:customize_output[public_html/customize/test.html]')
        expect(IO.read('public_html/customize/test.html')).to eq(@result)
      end

      it 'should not customize twice a file', rake: true do
        @rake.invoke_task('site:customize_output[public_html/customize/test.html]')
        expect(IO.read('public_html/customize/test.html')).to eq(@result)
        @rake.options.build_all = true
        @rake['site:customize_output'].reenable
        @rake.invoke_task('site:customize_output[public_html/customize/test.html]')
        expect(IO.read('public_html/customize/test.html')).to eq(@result)
      end
    end
  end
end

describe 'Generate indexes process' do
  before(:all) do
    init_testing_website
    @rake = init_rake_and_install_org
    org_content = <<~ORG
      #+title: Index file

      My website
    ORG
    IO.write('src/index.org', org_content)
    FileUtils.mkdir_p ['src/news/test1', 'src/news/test2']
    file1 = <<~ORG
      #+title: Index file
      #+keywords: toto, titi

      My website
    ORG
    IO.write('src/news/test1/content.org', file1)
    file2 = <<~ORG
      #+title: Index file
      #+keywords: toto

      My website
    ORG
    IO.write('src/news/test2/content.org', file2)
  end

  before(:each) do
    @rake.options.build_all = true
    @rake.tasks.each(&:reenable)
  end

  after(:each) do
    FileUtils.rm_r ['tmp', 'src/tags', 'public_html'], force: true
  end

  after(:all) do
    Dir.chdir File.expand_path('../', __dir__)
    FileUtils.rm_r 'spec/data/website_testing', force: true
  end

  it 'should not generate index without blog folder', rake: true do
    @rake.invoke_task('site:index')
    expect(File.exist?('src/news/index.org')).to be(false)
    expect(File.exist?('src/tags/toto.org')).to be(false)
    expect(File.exist?('src/tags/titi.org')).to be(false)
    expect(File.exist?('public_html/feeds/index.xml')).to be(false)
    expect(File.exist?('public_html/feeds/toto.xml')).to be(false)
    expect(File.exist?('public_html/feeds/titi.xml')).to be(false)
  end

  it 'should not generate index without blog folder when calling build', rake: true do
    @rake.invoke_task('site:build')
    expect(File.exist?('src/news/index.org')).to be(false)
    expect(File.exist?('src/tags/toto.org')).to be(false)
    expect(File.exist?('src/tags/titi.org')).to be(false)
    expect(File.exist?('public_html/news/index.html')).to be(false)
    expect(File.exist?('public_html/tags/toto.html')).to be(false)
    expect(File.exist?('public_html/tags/titi.html')).to be(false)
    expect(File.exist?('public_html/feeds/index.xml')).to be(false)
    expect(File.exist?('public_html/feeds/toto.xml')).to be(false)
    expect(File.exist?('public_html/feeds/titi.xml')).to be(false)
  end

  it 'should generate indexes with a correct blog path', rake: true do
    Neruda::Config.load_test('blog_path' => 'news')
    @rake.invoke_task('site:index')
    expect(File.exist?('src/news/index.org')).to be(true)
    expect(File.exist?('src/tags/toto.org')).to be(true)
    expect(File.exist?('src/tags/titi.org')).to be(true)
    expect(File.exist?('public_html/feeds/index.xml')).to be(true)
    expect(File.exist?('public_html/feeds/toto.xml')).to be(true)
    expect(File.exist?('public_html/feeds/titi.xml')).to be(true)
  end

  it 'should generate indexes with a correct blog path, even with build', rake: true do
    Neruda::Config.load_test('blog_path' => 'news')
    @rake.invoke_task('site:build')
    expect(File.exist?('src/news/index.org')).to be(true)
    expect(File.exist?('src/tags/toto.org')).to be(true)
    expect(File.exist?('src/tags/titi.org')).to be(true)
    expect(File.exist?('public_html/news/index.html')).to be(true)
    expect(File.exist?('public_html/tags/toto.html')).to be(true)
    expect(File.exist?('public_html/tags/titi.html')).to be(true)
    expect(File.exist?('public_html/feeds/index.xml')).to be(true)
    expect(File.exist?('public_html/feeds/toto.xml')).to be(true)
    expect(File.exist?('public_html/feeds/titi.xml')).to be(true)
  end

  it 'should use website title for blog index', rake: true do
    Neruda::Config.load_test('blog_path' => 'news', 'title' => 'Big title')
    Rake.verbose(true) # Check this too
    @rake.invoke_task('site:index')
    expect(File.exist?('src/news/index.org')).to be(true)
    expect(File.exist?('src/tags/toto.org')).to be(true)
  end
end



describe 'Generate indexes process with direct blog files' do
  before(:all) do
    init_testing_website
    @rake = init_rake_and_install_org
    org_content = <<~ORG
      #+title: Index file

      My website
    ORG
    IO.write('src/index.org', org_content)
    FileUtils.mkdir_p ['src/news/test1', 'src/news/test2']
    file1 = <<~ORG
      #+title: Index file
      #+keywords: toto, titi

      My website
    ORG
    IO.write('src/news/test1.org', file1)
    file2 = <<~ORG
      #+title: Index file
      #+keywords: toto

      My website
    ORG
    IO.write('src/news/test2.org', file2)
    Neruda::Config.load_test('blog_path' => 'news')
  end

  before(:each) do
    @rake.options.build_all = true
    @rake.tasks.each(&:reenable)
  end

  after(:each) do
    FileUtils.rm_r ['tmp', 'src/tags', 'public_html'], force: true
  end

  after(:all) do
    Dir.chdir File.expand_path('../', __dir__)
    FileUtils.rm_r 'spec/data/website_testing', force: true
  end

  it 'should generate indexes with a correct blog path', rake: true do
    @rake.invoke_task('site:index')
    expect(File.exist?('src/news/index.org')).to be(true)
    expect(File.exist?('src/tags/toto.org')).to be(true)
    expect(File.exist?('src/tags/titi.org')).to be(true)
    expect(File.exist?('public_html/feeds/index.xml')).to be(true)
    expect(File.exist?('public_html/feeds/toto.xml')).to be(true)
    expect(File.exist?('public_html/feeds/titi.xml')).to be(true)
  end

  it 'should generate indexes with a correct blog path, even with build', rake: true do
    @rake.invoke_task('site:build')
    expect(File.exist?('src/news/index.org')).to be(true)
    expect(File.exist?('src/tags/toto.org')).to be(true)
    expect(File.exist?('src/tags/titi.org')).to be(true)
    expect(File.exist?('public_html/news/index.html')).to be(true)
    expect(File.exist?('public_html/tags/toto.html')).to be(true)
    expect(File.exist?('public_html/tags/titi.html')).to be(true)
    expect(File.exist?('public_html/feeds/index.xml')).to be(true)
    expect(File.exist?('public_html/feeds/toto.xml')).to be(true)
    expect(File.exist?('public_html/feeds/titi.xml')).to be(true)
  end
end
