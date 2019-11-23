# frozen_string_literal: true

require 'rake'

def write_base_files
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
end

describe 'With a testing website' do
  before(:all) do
    init_testing_website
    @rake = init_rake_and_install_org
  end

  after(:all) do
    Dir.chdir File.expand_path('../', __dir__)
    FileUtils.rm_r 'spec/data/website_testing', force: true
  end

  describe 'building org files' do
    before(:each) do
      @rake.options.build_all = true
      @rake.tasks.each(&:reenable)
      Rake.verbose(false)
      o = Neruda::OrgFile.new('src/index.org',
                              title: 'My website',
                              content: 'Nice content.')
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
      o = Neruda::OrgFile.new('src/tutu.org', title: 'Tutu test')
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

  describe 'generate indexes process' do
    before(:all) do
      write_base_files
    end

    before(:each) do
      @rake.options.build_all = true
      @rake.tasks.each(&:reenable)
    end

    after(:each) do
      FileUtils.rm_r ['tmp', 'src/tags', 'public_html'], force: true
    end

    after(:all) do
      FileUtils.rm_r ['src/index.org', 'src/news']
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

    describe 'with a correct blog path' do
      before(:each) do
        Rake.verbose(false)
        Neruda::Config.load_test('blog_path' => 'news')
      end

      it 'should generate indexes', rake: true do
        @rake.invoke_task('site:index')
        expect(File.exist?('src/news/index.org')).to be(true)
        expect(File.exist?('src/tags/toto.org')).to be(true)
        expect(File.exist?('src/tags/titi.org')).to be(true)
        expect(File.exist?('public_html/feeds/index.xml')).to be(true)
        expect(File.exist?('public_html/feeds/toto.xml')).to be(true)
        expect(File.exist?('public_html/feeds/titi.xml')).to be(true)
      end

      it 'should generate indexes, even verbosely', rake: true do
        Rake.verbose(true)
        @rake.invoke_task('site:index')
        expect(File.exist?('src/news/index.org')).to be(true)
        expect(File.exist?('src/tags/toto.org')).to be(true)
        expect(File.exist?('src/tags/titi.org')).to be(true)
        expect(File.exist?('public_html/feeds/index.xml')).to be(true)
        expect(File.exist?('public_html/feeds/toto.xml')).to be(true)
        expect(File.exist?('public_html/feeds/titi.xml')).to be(true)
      end

      it 'should generate indexes, even with build', rake: true do
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

      it 'should list all tags by name', rake: true do
        expect { @rake.invoke_task('tags:name') }.to output("titi\ntoto\n").to_stdout
      end

      it 'should list all tags by weight', rake: true do
        expect { @rake.invoke_task('tags:weight') }.to output("toto\ntiti\n").to_stdout
      end
    end

    it 'should use website title for blog index', rake: true do
      Neruda::Config.load_test('blog_path' => 'news', 'title' => 'Big title')
      Rake.verbose(true) # Check this too
      @rake.invoke_task('site:index')
      expect(File.exist?('src/news/index.org')).to be(true)
      expect(File.exist?('src/tags/toto.org')).to be(true)
    end
  end
end
