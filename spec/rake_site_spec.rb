# frozen_string_literal: true

require 'rake'
require 'open-uri'

def init_rake_and_install_org
  # When run with all other specs, config may have been already loaded
  Neruda::Config.send(:load_settings)
  rake = Rake.application
  rake.raw_load_rakefile
  Rake.verbose(false)
  rake.options.build_all = true
  rake.tasks.each(&:reenable)
  tarball = File.expand_path(
    "../tmp/org-#{Neruda::Config.org_last_version}.tar.gz",
    __dir__
  )
  FileUtils.mkdir_p 'tmp'
  FileUtils.cp tarball, 'tmp'
  rake.invoke_task('org:install')
  rake
end

def write_base_files
  FileUtils.mkdir_p 'src/news'
  org_content = <<~ORG
    #+title: Index file

    My website
  ORG
  IO.write('src/index.org', org_content)
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
    Dir.chdir File.expand_path('..', __dir__)
    FileUtils.rm_r 'tmp/website_testing', force: true
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
      FileUtils.rm_r ['tmp', 'src', 'public_html', 'tags'], force: true
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

    it 'should fail gracefully with wrong project', rake: true do
      expect { @rake.invoke_task('site:build:one[very/wrong.org]') }.to(
        output("No project found for very/wrong.org\n").to_stderr
      )
    end

    it 'should fail gracefully with wrong file in project', rake: true do
      expect { @rake.invoke_task('site:build:one[src/wrong.org]') }.to(
        output(/Aborting/).to_stderr
      )
    end

    it 'should build one specific file even in verbose mode', rake: true do
      o = Neruda::OrgFile.new('src/tutu.org', title: 'Tutu test')
      o.write
      Rake.verbose(true)
      @rake.invoke_task('site:build:one[src/tutu.org]')
      expect(File.exist?('public_html/index.html')).to be(false)
      expect(File.exist?('public_html/tutu.html')).to be(true)
    end

    it 'should return an error if no file is given in build:one', rake: true do
      expect { @rake.invoke_task('site:build:one') }.to \
        output("No source file given\n").to_stderr
    end

    describe 'trying preview mode' do
      before(:each) do
        @now_str = DateTime.now.strftime('%A %-d of %B, %Y at %R')
        @rake.invoke_task('site:build')
        @webrick_app = Thread.new do
          @rake.invoke_task('site:preview')
        end
        sleep 1 # Necessary to let webrick start
      end

      after(:each) do
        @webrick_app.exit # Be sure to kill test server
        @webrick_app.join # Be patient before quitting example
      end

      it 'should be viewable with preview', rake: true do
        home_page = URI('http://localhost:5000/index.html').open.read
        proof = File.expand_path('data/index_proof.html', __dir__)
        proof_content = IO.read(proof).gsub(/__PUB_DATE__/, @now_str)
        expect(home_page).to eq(proof_content)
        style = URI('http://localhost:5000/assets/default/css/style.css').open.read
        proof = File.expand_path('../themes/default/css/style.css', __dir__)
        expect(style.force_encoding('utf-8')).to eq(IO.read(proof))
      end

      it 'should serve index', rake: true do
        home_page = URI('http://localhost:5000/').open.read
        proof = File.expand_path('data/index_proof.html', __dir__)
        proof_content = IO.read(proof).gsub(/__PUB_DATE__/, @now_str)
        expect(home_page).to eq(proof_content)
      end

      it 'should be viewable with routes testing', rake: true do
        home_page = URI('http://localhost:5000/test').open.read
        proof = File.expand_path('data/index_proof.html', __dir__)
        proof_content = IO.read(proof).gsub(/__PUB_DATE__/, @now_str)
        expect(home_page).to eq(proof_content)
      end

      it 'should send an error if a page is not found', rake: true do
        expect { URI('http://localhost:5000/not_found.html').open.read }.to(
          raise_error(/404 Not Found/)
        )
      end
    end
  end

  describe 'generate indexes process' do
    before(:each) do
      write_base_files
      @rake.options.build_all = true
      @rake.tasks.each(&:reenable)
      Neruda::Config.send(:load_settings)
    end

    after(:each) do
      FileUtils.rm_r ['tmp', 'src', 'public_html', 'tags'], force: true
    end

    describe 'without blog setting', rake: true do
      before(:each) do
        old_conf = Neruda::Config.settings.dup
        old_conf['sources'][1]['is_blog'] = false
        Neruda::Config.load_test(old_conf)
      end

      it 'should not generate index', rake: true do
        @rake.invoke_task('site:index')
        expect(File.exist?('src/news/index.org')).to be(false)
        expect(File.exist?('tags/index.org')).to be(false)
        expect(File.exist?('tags/toto.org')).to be(false)
        expect(File.exist?('tags/titi.org')).to be(false)
        expect(File.exist?('public_html/feeds/index.xml')).to be(false)
        expect(File.exist?('public_html/feeds/toto.xml')).to be(false)
        expect(File.exist?('public_html/feeds/titi.xml')).to be(false)
      end

      it 'should not generate index when calling build', rake: true do
        @rake.invoke_task('site:build')
        expect(File.exist?('src/news/index.org')).to be(false)
        expect(File.exist?('tags/index.org')).to be(false)
        expect(File.exist?('tags/toto.org')).to be(false)
        expect(File.exist?('tags/titi.org')).to be(false)
        expect(File.exist?('public_html/news/index.html')).to be(false)
        expect(File.exist?('public_html/tags/index.html')).to be(false)
        expect(File.exist?('public_html/tags/index.html')).to be(false)
        expect(File.exist?('public_html/tags/toto.html')).to be(false)
        expect(File.exist?('public_html/tags/titi.html')).to be(false)
        expect(File.exist?('public_html/feeds/index.xml')).to be(false)
        expect(File.exist?('public_html/feeds/toto.xml')).to be(false)
        expect(File.exist?('public_html/feeds/titi.xml')).to be(false)
      end

      it 'should not list tags', rake: true do
        expect { @rake.invoke_task('tags:name') }.to output('').to_stdout
        expect { @rake.invoke_task('tags:weight') }.to output('').to_stdout
      end
    end

    describe 'with wrong blog settings', rake: true do
      before(:each) do
        old_conf = Neruda::Config.settings.dup
        old_conf['sources'][1]['path'] = 'src/test'
        Neruda::Config.load_test(old_conf)
      end

      it 'should not generate index', rake: true do
        @rake.invoke_task('site:index')
        expect(File.exist?('src/test/index.org')).to be(false)
        expect(File.exist?('public_html/feeds/index.xml')).to be(false)
      end

      it 'should not generate index when calling build', rake: true do
        @rake.invoke_task('site:build')
        expect(File.exist?('src/test/index.org')).to be(false)
        expect(File.exist?('tags/index.org')).to be(false)
        expect(File.exist?('tags/toto.org')).to be(false)
        expect(File.exist?('tags/titi.org')).to be(false)
        expect(File.exist?('public_html/test/index.html')).to be(false)
        expect(File.exist?('public_html/tags/index.html')).to be(false)
        expect(File.exist?('public_html/feeds/index.xml')).to be(false)
      end

      it 'should not list tags', rake: true do
        expect { @rake.invoke_task('tags:name') }.to output('').to_stdout
        expect { @rake.invoke_task('tags:weight') }.to output('').to_stdout
      end
    end

    describe 'with a correct blog path' do
      before(:each) do
        Rake.verbose(false)
      end

      it 'should generate indexes', rake: true do
        @rake.invoke_task('site:index')
        expect(File.exist?('src/news/index.org')).to be(true)
        expect(File.exist?('tags/index.org')).to be(true)
        expect(File.exist?('tags/toto.org')).to be(true)
        expect(File.exist?('tags/titi.org')).to be(true)
        expect(File.exist?('public_html/feeds/index.xml')).to be(true)
        expect(File.exist?('public_html/feeds/toto.xml')).to be(true)
        expect(File.exist?('public_html/feeds/titi.xml')).to be(true)
      end

      it 'should generate indexes, even verbosely', rake: true do
        Rake.verbose(true)
        @rake.invoke_task('site:index')
        expect(File.exist?('src/news/index.org')).to be(true)
        expect(File.exist?('tags/index.org')).to be(true)
        expect(File.exist?('tags/toto.org')).to be(true)
        expect(File.exist?('tags/titi.org')).to be(true)
        expect(File.exist?('public_html/feeds/index.xml')).to be(true)
        expect(File.exist?('public_html/feeds/toto.xml')).to be(true)
        expect(File.exist?('public_html/feeds/titi.xml')).to be(true)
      end

      it 'should generate indexes, even with build', rake: true do
        @rake.invoke_task('site:build')
        expect(File.exist?('src/news/index.org')).to be(true)
        expect(File.exist?('tags/index.org')).to be(true)
        expect(File.exist?('tags/toto.org')).to be(true)
        expect(File.exist?('tags/titi.org')).to be(true)
        expect(File.exist?('public_html/news/index.html')).to be(true)
        expect(File.exist?('public_html/tags/index.html')).to be(true)
        expect(File.exist?('public_html/tags/index.html')).to be(true)
        expect(File.exist?('public_html/tags/toto.html')).to be(true)
        expect(File.exist?('public_html/tags/titi.html')).to be(true)
        expect(File.exist?('public_html/feeds/index.xml')).to be(true)
        expect(File.exist?('public_html/feeds/toto.xml')).to be(true)
        expect(File.exist?('public_html/feeds/titi.xml')).to be(true)
      end

      it 'should list all tags by name', rake: true do
        expect { @rake.invoke_task('tags:name') }.to(
          output("toto (2)\ntiti (1)\n").to_stdout
        )
      end

      it 'should list all tags by weight', rake: true do
        expect { @rake.invoke_task('tags:weight') }.to(
          output("titi (1)\ntoto (2)\n").to_stdout
        )
      end
    end
  end
end
